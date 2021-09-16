!> author: Sam Hatfield, Fred Kucharski, Franco Molteni
!  date: 04/07/2019
!  For storing and initializing prognostic spectral variables for model dynamics, and geopotential.
module prognostics
    use types, only : p
    use params, only : mx, nx, kx, ntr, ix, iy, il
    use model_control, only : ControlParams_t
    use model_state, only : ModelState_t
    use spectral, only : ModSpectral_t

    implicit none

    private

    public initialize_prognostics, spectral2grid

contains

    !> Initializes all spectral variables starting from either a reference
    !  atmosphere or a restart file.
    subroutine initialize_prognostics(prognostic_vars, control_params)
        type(ModelState_t), intent(inout) :: prognostic_vars
        type(ControlParams_t), intent(in) :: control_params
        call initialize_from_rest_state(prognostic_vars, control_params)
    end subroutine

    !> Initializes all spectral variables starting from a reference atmosphere.
    subroutine initialize_from_rest_state(state, control_params)
        use dynamical_constants, only : gamma, hscale, hshum, refrh1
        use physical_constants, only : grav, rgas
        use geometry, only : fsg
        use diagnostics, only : check_diagnostics

        type(ModelState_t), intent(inout), target :: state
        type(ControlParams_t), intent(in) :: control_params

        complex(p) :: surfs(mx, nx)
        real(p) :: surfg(ix, il)
        real(p) :: gam1, esref, gam2, qexp, qref, rgam, rgamr, rlog0, tref, ttop
        integer :: i, j, k

        class(ModSpectral_t), pointer :: mod_spectral
        mod_spectral => state%mod_spectral

        gam1 = gamma / (1000.0 * grav)

        ! 1. Compute spectral surface geopotential
        state%phis = mod_spectral%grid2spec(state%phis0)

        ! 2. Start from reference atmosphere (at rest)

        ! 2.1 Set vorticity, divergence and tracers to zero
        state%vor(:, :, :, 1) = (0.0, 0.0)
        state%div(:, :, :, 1) = (0.0, 0.0)
        state%tr(:, :, :, 1, :) = (0.0, 0.0)

        ! 2.2 Set reference temperature :
        !     tropos:  T = 288 degK at z = 0, constant lapse rate
        !     stratos: T = 216 degK, lapse rate = 0
        tref = 288.0
        ttop = 216.0
        gam2 = gam1 / tref
        rgam = rgas * gam1
        rgamr = 1.0 / rgam

        ! Surface and stratospheric air temperature
        state%t(:, :, 1, 1) = (0.0, 0.0)
        state%t(:, :, 2, 1) = (0.0, 0.0)
        surfs = -gam1 * state%phis

        state%t(1, 1, 1, 1) = sqrt(2.0) * (1.0, 0.0) * ttop
        state%t(1, 1, 2, 1) = sqrt(2.0) * (1.0, 0.0) * ttop
        surfs(1, 1) = sqrt(2.0) * (1.0, 0.0) * tref - gam1 * state%phis(1, 1)

        ! Temperature at tropospheric levels
        do k = 3, kx
            state%t(:, :, k, 1) = surfs * fsg(k)**rgam
        end do

        ! 2.3 Set log(ps) consistent with temperature profile
        !     p_ref = 1013 hPa at z = 0
        rlog0 = log(1.013)

        do j = 1, il
            do i = 1, ix
                surfg(i, j) = rlog0 + rgamr * log(1.0 - gam2 * state%phis0(i, j))
            end do
        end do

        state%ps(:, :, 1) = mod_spectral%grid2spec(surfg)
        if (ix == iy * 4) call mod_spectral%truncate(state%ps)

        ! 2.4 Set tropospheric specific humidity in g/kg
        !     Qref = RHref * Qsat(288K, 1013hPa)
        esref = 17.0
        qref = refrh1 * 0.622 * esref
        qexp = hscale / hshum

        ! Specific humidity at the surface
        do j = 1, il
            do i = 1, ix
                surfg(i, j) = qref * exp(qexp * surfg(i, j))
            end do
        end do

        surfs = mod_spectral%grid2spec(surfg)
        if (ix == iy * 4) call mod_spectral%truncate(surfs)

        ! Specific humidity at tropospheric levels
        do k = 3, kx
            state%tr(:, :, k, 1, 1) = surfs * fsg(k)**qexp
        end do

        ! Print diagnostics from initial conditions
        call check_diagnostics(state%vor(:, :, :, 1), &
                state%div(:, :, :, 1), &
                state%t(:, :, :, 1), &
                0, control_params%diag_interval,&
                state%mod_spectral)

    end subroutine

    !> Transform the prognostic variables from the spectral to the grid space. 
    !  The spectral Divergence and Vorticity are transformed to U and V in the 
    !  grid space.
    subroutine spectral2grid(state)
        use physical_constants, only : grav, p0

        type(ModelState_t), intent(inout), target :: state

        complex(p), dimension(:, :), allocatable :: ucos, vcos
        integer :: k
        class(ModSpectral_t), pointer :: mod_spectral

        mod_spectral => state%mod_spectral

        allocate(ucos(mx, nx), vcos(mx, nx))

        ! Convert prognostic fields from spectral space to grid point space
        ! Transform some of the variables to more suitable units.
        do k = 1, kx

            call mod_spectral%vort2vel(&
                    state%vor(:, :, k, 1), state%div(:, :, k, 1), ucos, vcos)

            state%u_grid(:, :, k) = mod_spectral%spec2grid(ucos, 2)
            state%v_grid(:, :, k) = mod_spectral%spec2grid(vcos, 2)
            state%t_grid(:, :, k) = mod_spectral%spec2grid(state%t(:, :, k, 1), 1)
            state%q_grid(:, :, k) = mod_spectral%spec2grid(state%tr(:, :, k, 1, 1), 1) * 1.0e-3 ! kg/kg
            state%phi_grid(:, :, k) = mod_spectral%spec2grid(state%phi(:, :, k), 1) / grav ! m
        end do

        state%ps_grid = p0 * exp(mod_spectral%spec2grid(state%ps(:, :, 1), 1)) ! Pa

        deallocate(ucos)
        deallocate(vcos)

    end subroutine
end module
