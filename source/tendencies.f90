module tendencies
    use types, only : p
    use params

    implicit none

    private
    public get_tendencies

contains
    subroutine get_tendencies(state, vordt, divdt, tdt, psdt, trdt, j2)
        use model_state, only : ModelState_t

        complex(p), dimension(mx, nx, kx), intent(inout) :: vordt, divdt, tdt
        complex(p), intent(inout) :: psdt(mx, nx), trdt(mx, nx, kx, ntr)
        integer, intent(in) :: j2

        type(ModelState_t), intent(inout) :: state

        ! =========================================================================
        ! Computation of grid-point tendencies (converted to spectral at the end of
        ! grtend)
        ! =========================================================================

        call get_grid_point_tendencies(state, vordt, divdt, tdt, psdt, trdt, 1, j2)

        ! =========================================================================
        ! Computation of spectral tendencies
        ! =========================================================================

        if (alph < 0.5) then
            call get_spectral_tendencies(state, divdt, tdt, psdt, j2)
        else
            call get_spectral_tendencies(state, divdt, tdt, psdt, 1)

            ! Implicit correction
            call state%mod_implicit%implicit_terms(divdt, tdt, psdt)
        end if
    end subroutine

    ! Compute non-linear tendencies in grid point space from dynamics and
    ! physical parametrizations, and convert them to spectral tendencies
    ! dF/dt = T_dyn(F(J2)) + T_phy(F(J1))
    !   Input:  j1 = time level index for physical tendencies
    !           j2 = time level index for dynamical tendencies
    !   Output: vordt = spectral tendency of vorticity
    !           divdt = spectral tendency of divergence
    !           tdt   = spectral tendency of temperature
    !           psdt  = spectral tendency of log(p_s)
    !           trdt  = spectral tendency of tracers
    subroutine get_grid_point_tendencies(state, vordt, divdt, tdt, psdt, trdt, j1, j2)
        use model_state, only : ModelState_t
        use physical_constants, only : akap, rgas
        use geometry, only : dhs, dhsr, fsgr, coriol
        use implicit, only : ModImplicit_t
        use geopotential, only : get_geopotential
        use physics, only : get_physical_tendencies
        use spectral, only : ModSpectral_t

        type(ModelState_t), intent(inout), target :: state

        !** notes ****
        ! -- TG does not have to be computed at both time levels every time step,
        !     I have left it mod_spectral way to retain parallel structure with subroutine
        !     using latitude loop
        ! -- memory can be reduced considerably eliminating TGG, computing VORG
        !     only when needed, etc -- I have not optimized mod_spectral subroutine for
        !     routine use on the YMP
        ! -- results from grtend1.F should duplicate results from grtend.F
        !                              -- Isaac
        !************

        complex(p), dimension(mx, nx, kx), intent(inout) :: vordt, divdt, tdt
        complex(p), intent(inout) :: psdt(mx, nx), trdt(mx, nx, kx, ntr)
        integer, intent(in) :: j1, j2

        ! Local vars
        complex(p), allocatable, dimension(:, :, :) :: dumc

        real(p), allocatable, dimension(:, :, :) :: utend, vtend, ttend
        real(p), allocatable, dimension(:, :, :, :) :: trtend

        real(p), allocatable, dimension(:, :, :) :: ug, vg, tg, vorg, divg, tgg, puv
        real(p), allocatable, dimension(:, :) :: px, py, umean, vmean, dmean
        real(p), allocatable, dimension(:, :, :, :) :: trg
        real(p), allocatable, dimension(:, :, :) :: sigdt, temp, sigm

        integer :: k, i, itr, j

        class(ModSpectral_t), pointer :: mod_spectral
        class(ModImplicit_t), pointer :: mod_implicit
        mod_spectral => state%mod_spectral
        mod_implicit => state%mod_implicit

        allocate(dumc(mx, nx, 2))
        allocate(utend(ix, il, kx), vtend(ix, il, kx), ttend(ix, il, kx))
        allocate(trtend(ix, il, kx, ntr))
        allocate(ug(ix, il, kx), vg(ix, il, kx), tg(ix, il, kx))
        allocate(vorg(ix, il, kx), divg(ix, il, kx), tgg(ix, il, kx), puv(ix, il, kx))
        allocate(px(ix, il), py(ix, il), umean(ix, il), vmean(ix, il), dmean(ix, il))
        allocate(trg(ix, il, kx, ntr), sigdt(ix, il, kx + 1))
        allocate(temp(ix, il, kx + 1), sigm(ix, il, kx + 1))

        ! =========================================================================
        ! Convert prognostics to grid point space
        ! =========================================================================
        do k = 1, kx
            vorg(:, :, k) = mod_spectral%spec2grid(state%vor(:, :, k, j2), 1)
            divg(:, :, k) = mod_spectral%spec2grid(state%div(:, :, k, j2), 1)
            tg(:, :, k) = mod_spectral%spec2grid(state%t(:, :, k, j2), 1)

            do itr = 1, ntr
                trg(:, :, k, itr) = mod_spectral%spec2grid(state%tr(:, :, k, j2, itr), 1)
            end do

            call mod_spectral%vort2vel(&
                    state%vor(:, :, k, j2), state%div(:, :, k, j2), &
                    dumc(:, :, 1), dumc(:, :, 2))

            vg(:, :, k) = mod_spectral%spec2grid(dumc(:, :, 2), 2)
            ug(:, :, k) = mod_spectral%spec2grid(dumc(:, :, 1), 2)

            do j = 1, il
                do i = 1, ix
                    vorg(i, j, k) = vorg(i, j, k) + coriol(j)
                end do
            end do
        end do

        umean(:, :) = 0.0
        vmean(:, :) = 0.0
        dmean(:, :) = 0.0

        do k = 1, kx
            umean(:, :) = umean(:, :) + ug(:, :, k) * dhs(k)
            vmean(:, :) = vmean(:, :) + vg(:, :, k) * dhs(k)
            dmean(:, :) = dmean(:, :) + divg(:, :, k) * dhs(k)
        end do

        ! Compute tendency of log(surface pressure)
        ! ps(1,1,j2)=zero
        call mod_spectral%gradient(state%ps(:, :, j2), dumc(:, :, 1), dumc(:, :, 2))
        px = mod_spectral%spec2grid(dumc(:, :, 1), 2)
        py = mod_spectral%spec2grid(dumc(:, :, 2), 2)

        psdt = mod_spectral%grid2spec(-umean * px - vmean * py)
        psdt(1, 1) = (0.0, 0.0)

        ! Compute "vertical" velocity
        sigdt(:, :, 1) = 0.0
        sigdt(:, :, kx + 1) = 0.0
        sigm(:, :, 1) = 0.0
        sigm(:, :, kx + 1) = 0.0

        ! (The following combination of terms is utilized later in the
        !     temperature equation)
        do k = 1, kx
            puv(:, :, k) = (ug(:, :, k) - umean) * px + (vg(:, :, k) - vmean) * py
        end do

        do k = 1, kx
            sigdt(:, :, k + 1) = sigdt(:, :, k) - dhs(k) * (puv(:, :, k) + divg(:, :, k) - dmean)
            sigm(:, :, k + 1) = sigm(:, :, k) - dhs(k) * puv(:, :, k)
        end do

        ! Subtract part of temperature field that is used as reference for
        ! implicit terms
        do k = 1, kx
            tgg(:, :, k) = tg(:, :, k) - mod_implicit%tref(k)
        end do

        ! Zonal wind tendency
        temp(:, :, 1) = 0.0
        temp(:, :, kx + 1) = 0.0

        do k = 2, kx
            temp(:, :, k) = sigdt(:, :, k) * (ug(:, :, k) - ug(:, :, k - 1))
        end do

        do k = 1, kx
            utend(:, :, k) = vg(:, :, k) * vorg(:, :, k) - tgg(:, :, k) * rgas * px &
                    & - (temp(:, :, k + 1) + temp(:, :, k)) * dhsr(k)
        end do

        ! Meridional wind tendency
        do k = 2, kx
            temp(:, :, k) = sigdt(:, :, k) * (vg(:, :, k) - vg(:, :, k - 1))
        end do

        do k = 1, kx
            vtend(:, :, k) = -ug(:, :, k) * vorg(:, :, k) - tgg(:, :, k) * rgas * py &
                    & - (temp(:, :, k + 1) + temp(:, :, k)) * dhsr(k)
        end do

        ! Temperature tendency
        do k = 2, kx
            temp(:, :, k) = sigdt(:, :, k) * (tgg(:, :, k) - tgg(:, :, k - 1)) &
                    & + sigm(:, :, k) * (mod_implicit%tref(k) - mod_implicit%tref(k - 1))
        end do

        do k = 1, kx
            ttend(:, :, k) = tgg(:, :, k) * divg(:, :, k) &
                    - (temp(:, :, k + 1) + temp(:, :, k)) * dhsr(k) &
                    + fsgr(k) * tgg(:, :, k) * (sigdt(:, :, k + 1) + sigdt(:, :, k)) &
                    + mod_implicit%tref3(k) * (sigm(:, :, k + 1) &
                            + sigm(:, :, k)) &
                    + akap * (tg(:, :, k) * puv(:, :, k) - tgg(:, :, k) * dmean(:, :))
        end do

        ! Tracer tendency
        do itr = 1, ntr
            do k = 2, kx
                temp(:, :, k) = sigdt(:, :, k) * (trg(:, :, k, itr) - trg(:, :, k - 1, itr))
            end do

            temp(:, :, 2:3) = 0.0

            do k = 1, kx
                trtend(:, :, k, itr) = trg(:, :, k, itr) * divg(:, :, k) &
                        - (temp(:, :, k + 1) + temp(:, :, k)) * dhsr(k)
            end do
        end do

        ! =========================================================================
        ! Compute physical tendencies
        ! =========================================================================

        state%phi = get_geopotential(state%t(:, :, :, j1), state%phis, state%xgeop1, state%xgeop2)

        call get_physical_tendencies(state, j1, &
                utend, vtend, ttend, trtend)

        ! =========================================================================
        ! Convert tendencies to spectral space
        ! =========================================================================

        do k = 1, kx
            !  Convert u and v tendencies to vor and div spectral tendencies
            !  mod_spectral%grid_vel2vort takes a grid u and a grid v and converts them to
            !  spectral vor and div
            call mod_spectral%grid_vel2vort(&
                    utend(:, :, k), vtend(:, :, k), vordt(:, :, k), divdt(:, :, k), 2)

            ! Divergence tendency
            ! add -lapl(0.5*(u**2+v**2)) to div tendency
            divdt(:, :, k) = divdt(:, :, k) &
                    & - mod_spectral%laplacian(&
                            mod_spectral%grid2spec(0.5 * (ug(:, :, k)**2.0 + vg(:, :, k)**2.0)))

            ! Temperature tendency
            ! and add div(vT) to spectral t tendency
            call mod_spectral%grid_vel2vort(&
                    -ug(:, :, k) * tgg(:, :, k), &
                    -vg(:, :, k) * tgg(:, :, k), &
                    dumc(:, :, 1), tdt(:, :, k), &
                    2)
            tdt(:, :, k) = tdt(:, :, k) + mod_spectral%grid2spec(ttend(:, :, k))

            ! tracer tendency
            do itr = 1, ntr
                call mod_spectral%grid_vel2vort(&
                        -ug(:, :, k) * trg(:, :, k, itr), -vg(:, :, k) * trg(:, :, k, itr), &
                        & dumc(:, :, 1), trdt(:, :, k, itr), 2)

                trdt(:, :, k, itr) = trdt(:, :, k, itr) + mod_spectral%grid2spec(trtend(:, :, k, itr))
            end do
        end do

        deallocate(dumc)
        deallocate(utend, vtend, ttend, trtend)
        deallocate(ug, vg, tg, vorg, divg, tgg, puv)
        deallocate(px, py, umean, vmean, dmean)
        deallocate(trg, sigdt, temp, sigm)

    end subroutine

    ! Compute spectral tendencies of divergence, temperature  and log(surface pressure)
    ! Input/output : divdt = divergence tendency (spectral)
    !                tdt   = temperature tendency (spectral)
    !                psdt  = tendency of log_surf.pressure (spectral)
    !                j2    = time level index (1 or 2)
    subroutine get_spectral_tendencies(state, divdt, tdt, psdt, j2)
        use model_state, only : ModelState_t
        use physical_constants, only : rgas
        use geometry, only : dhs, dhsr
        use geopotential, only : get_geopotential
        use implicit, only : ModImplicit_t
        use spectral, only : ModSpectral_t

        type(ModelState_t), intent(inout), target :: state

        complex(p), intent(inout) :: psdt(mx, nx), divdt(mx, nx, kx), tdt(mx, nx, kx)
        integer, intent(in) :: j2

        complex(p), allocatable, dimension(:,:,:) :: dumk, sigdtc
        complex(p), allocatable, dimension(:,:) :: dmeanc
        integer :: k

        class(ModSpectral_t), pointer :: mod_spectral
        class(ModImplicit_t), pointer :: mod_implicit
        mod_spectral => state%mod_spectral
        mod_implicit => state%mod_implicit

        allocate(dumk(mx, nx, kx + 1), dmeanc(mx, nx), sigdtc(mx, nx, kx + 1))

        ! Vertical mean div and pressure tendency
        dmeanc(:, :) = (0.0, 0.0)
        do k = 1, kx
            dmeanc = dmeanc + state%div(:, :, k, j2) * dhs(k)
        end do

        psdt = psdt - dmeanc
        psdt(1, 1) = (0.0, 0.0)

        ! Sigma-dot "velocity" and temperature tendency
        sigdtc(:, :, 1) = (0.0, 0.0)
        sigdtc(:, :, kx + 1) = (0.0, 0.0)

        do k = 1, kx - 1
            sigdtc(:, :, k + 1) = sigdtc(:, :, k) &
                    - dhs(k) * (state%div(:, :, k, j2) - dmeanc)

        end do

        dumk(:, :, 1) = (0.0, 0.0)
        dumk(:, :, kx + 1) = (0.0, 0.0)

        do k = 2, kx
            dumk(:, :, k) = sigdtc(:, :, k) * (mod_implicit%tref(k) - mod_implicit%tref(k - 1))
        end do

        do k = 1, kx
            tdt(:, :, k) = tdt(:, :, k) &
                    - (dumk(:, :, k + 1) + dumk(:, :, k)) * dhsr(k)&
                    + mod_implicit%tref3(k) * (sigdtc(:, :, k + 1) + sigdtc(:, :, k))&
                    - mod_implicit%tref2(k) * dmeanc
        end do

        ! Geopotential and divergence tendency
        state%phi = get_geopotential(state%t(:, :, :, j2), state%phis, state%xgeop1, state%xgeop2)

        do k = 1, kx
            divdt(:, :, k) = divdt(:, :, k) &
                    - mod_spectral%laplacian(state%phi(:, :, k) &
                            + rgas * mod_implicit%tref(k) * state%ps(:, :, j2))
        end do

        deallocate(dumk, sigdtc, dmeanc)
    end subroutine
end module
