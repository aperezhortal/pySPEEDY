module time_stepping
    use types, only : p
    use params
    use spectral, only : ModSpectral_t

    implicit none

    private
    public first_step, step

contains
    ! Call initialization of semi-implicit scheme and perform initial time step
    subroutine first_step(state)
        use model_state, only : ModelState_t

        type(ModelState_t), intent(inout) :: state

        call state%mod_implicit%set_time_step(0.5 * delt)

        call step(state, 1, 1, 0.5 * delt)

        call state%mod_implicit%set_time_step(delt)

        call step(state, 1, 2, delt)

        call state%mod_implicit%set_time_step(2 * delt)
    end

    ! Perform one time step starting from F(1) and F(2) and using the following scheme:
    ! Fnew = F(1) + DT * [ T_dyn(F(J2)) + T_phy(F(1)) ]
    ! F(1) = (1-2*eps)*F(J1) + eps*[F(1)+Fnew]
    ! F(2) = Fnew
    ! Input:
    ! If j1 == 1, j2 == 1 : forward time step (eps = 0)
    ! If j1 == 1, j2 == 2 : initial leapfrog time step (eps = 0)
    ! If j1 == 2, j2 == 2 : leapfrog time step with time filter (eps = ROB)
    ! dt = time step
    subroutine step(state, j1, j2, dt)
        use physical_constants, only : tdrs
        use model_state, only : ModelState_t
        use horizontal_diffusion, only : do_horizontal_diffusion
        use implicit, only : ModImplicit_t

        use tendencies, only : get_tendencies

        type(ModelState_t), intent(inout), target :: state

        integer, intent(in) :: j1, j2
        real(p), intent(in) :: dt

        ! Local vars
        real(p) :: eps, sdrag
        integer :: n, itr, k, m

        complex(p), allocatable, dimension(:, :, :) :: vordt, divdt, tdt, ctmp
        complex(p), allocatable, dimension(:, :) :: psdt
        complex(p), allocatable, dimension(:, :, :, :) :: trdt

        class(ModSpectral_t), pointer :: mod_spectral
        class(ModImplicit_t), pointer :: mod_implicit

        mod_spectral => state%mod_spectral
        mod_implicit => state%mod_implicit

        allocate(vordt(mx, nx, kx), divdt(mx, nx, kx), tdt(mx, nx, kx), ctmp(mx, nx, kx))
        allocate(psdt(mx, nx), trdt(mx, nx, kx, ntr))

        ! =========================================================================
        ! Compute tendencies of prognostic variables
        ! =========================================================================
        call get_tendencies(state, vordt, divdt, tdt, psdt, trdt, j2)

        ! =========================================================================
        ! Horizontal diffusion
        ! =========================================================================

        ! Diffusion of wind and temperature
        vordt = do_horizontal_diffusion(state%vor(:, :, :, 1), vordt, mod_implicit%dmp, mod_implicit%dmp1)
        divdt = do_horizontal_diffusion(state%div(:, :, :, 1), divdt, mod_implicit%dmpd, mod_implicit%dmp1d)

        do k = 1, kx
            do m = 1, mx
                do n = 1, nx
                    ctmp(m, n, k) = state%t(m, n, k, 1) + mod_implicit%tcorh(m, n) * mod_implicit%tcorv(k)
                end do
            end do
        end do

        tdt = do_horizontal_diffusion(ctmp, tdt, mod_implicit%dmp, mod_implicit%dmp1)

        ! Stratospheric diffusion and zonal wind damping
        sdrag = 1.0 / (tdrs * 3600.0)
        do n = 1, nx
            vordt(1, n, 1) = vordt(1, n, 1) - sdrag * state%vor(1, n, 1, 1)
            divdt(1, n, 1) = divdt(1, n, 1) - sdrag * state%div(1, n, 1, 1)
        end do

        vordt = do_horizontal_diffusion(state%vor(:, :, :, 1), vordt, mod_implicit%dmps, mod_implicit%dmp1s)
        divdt = do_horizontal_diffusion(state%div(:, :, :, 1), divdt, mod_implicit%dmps, mod_implicit%dmp1s)
        tdt = do_horizontal_diffusion(ctmp, tdt, mod_implicit%dmps, mod_implicit%dmp1s)

        ! Diffusion of tracers
        do k = 1, kx
            do m = 1, mx
                do n = 1, nx
                    ctmp(m, n, k) = state%tr(m, n, k, 1, 1) + mod_implicit%qcorh(m, n) * mod_implicit%qcorv(k)
                end do
            end do
        end do

        trdt(:, :, :, 1) = do_horizontal_diffusion(ctmp, trdt(:, :, :, 1), mod_implicit%dmpd, mod_implicit%dmp1d)

        if (ntr > 1) then
            do itr = 2, ntr
                !&<
                trdt(:, :, :, 1) = do_horizontal_diffusion(&
                        state%tr(:, :, :, 1, itr), &
                        trdt(:, :, :, itr), mod_implicit%dmp, mod_implicit%dmp1 &
                        )
                !&>
            end do
        end if

        ! =========================================================================
        ! Time integration with Robert filter
        ! =========================================================================

        if (j1 == 1) then
            eps = 0.0
        else
            eps = rob
        end if

        state%ps = step_field_2d(mod_spectral, j1, dt, eps, state%ps, psdt)
        state%vor = step_field_3d(mod_spectral, j1, dt, eps, state%vor, vordt)
        state%div = step_field_3d(mod_spectral, j1, dt, eps, state%div, divdt)
        state%t = step_field_3d(mod_spectral, j1, dt, eps, state%t, tdt)

        do itr = 1, ntr
            state%tr(:, :, :, :, itr) = step_field_3d(&
                    mod_spectral, j1, dt, eps, &
                    state%tr(:, :, :, :, itr), trdt(:, :, :, itr) &
                    )
        end do

        deallocate(vordt, divdt, tdt, ctmp, psdt, trdt)
    end

    ! Perform time integration of field across all model levels using tendency fdt
    function step_field_3d(mod_spectral, j1, dt, eps, input, fdt) result(output)
        class(ModSpectral_t), intent(in) :: mod_spectral
        integer, intent(in) :: j1
        real(p), intent(in) :: dt, eps
        complex(p), intent(inout) :: fdt(mx, nx, kx)
        complex(p), intent(in) :: input(mx, nx, kx, 2)
        complex(p) :: output(mx, nx, kx, 2)
        integer :: k

        do k = 1, kx
            output(:, :, k, :) = step_field_2d(mod_spectral, j1, dt, eps, input(:, :, k, :), fdt(:, :, k))
        end do
    end

    function step_field_2d(mod_spectral, j1, dt, eps, input, fdt) result(output)
        class(ModSpectral_t), intent(in) :: mod_spectral
        integer, intent(in) :: j1
        real(p), intent(in) :: dt, eps
        complex(p), intent(inout) :: fdt(mx, nx)
        complex(p), intent(in) :: input(mx, nx, 2)
        complex(p) :: output(mx, nx, 2)
        real(p) :: eps2
        complex(p) :: fnew(mx, nx)

        output = input

        eps2 = 1.0 - 2.0 * eps

        if (ix == iy * 4) then
            call mod_spectral%truncate(fdt)
        end if

        ! The actual leap frog with the Robert filter
        fnew = output(:, :, 1) + dt * fdt
        output(:, :, 1) = output(:, :, j1) + wil * eps * (output(:, :, 1) - 2 * output(:, :, j1) + fnew)

        ! Williams' innovation to the filter
        output(:, :, 2) = fnew - (1.0 - wil) * eps * (output(:, :, 1) - 2.0 * output(:, :, j1) + fnew)
    end
end module
