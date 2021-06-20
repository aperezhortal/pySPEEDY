!> author: Sam Hatfield, Fred Kucharski, Franco Molteni
!  date: 04/07/2019
!  For storing and initializing prognostic spectral variables for model dynamics, and geopotential.
module prognostics
    use types, only: p
    use params, only: mx, nx, kx, ntr, ix, iy, il, UserParams_t

    implicit none

    private

    public PrognosticVars_t
    public initialize_prognostics
    public PrognosticVars_allocate, PrognosticVars_deallocate

    type PrognosticVars_t
        ! Prognostic spectral variables

        ! kx Number of vertical levels
        ! nx Number of total wavenumbers for spectral storage arrays
        ! mx Number of zonal wavenumbers for spectral storage arrays
        complex(p), allocatable :: vor(:, :, :, :)   !! Vorticity
        complex(p), allocatable :: div(:, :, :, :)   !! Divergence
        complex(p), allocatable :: t(:, :, :, :)     !! Absolute temperature
        complex(p), allocatable :: ps(:, :, :)       !! Log of (normalised) surface pressure (p_s/p0)
        complex(p), allocatable :: tr(:, :, :, :, :) !! Tracers (tr(1): specific humidity in g/kg)

        ! Geopotential
        complex(p), allocatable :: phi(:, :, :) !! Atmospheric geopotential
        complex(p), allocatable :: phis(:, :)  !! Surface geopotential
    end type

contains

    !> Allocate PrognosticVars attributes.
    subroutine PrognosticVars_allocate(prognostic_vars)
        type(PrognosticVars_t), intent(out) :: prognostic_vars
        ! Allocate variables
        allocate (prognostic_vars%vor(mx, nx, kx, 2))
        allocate (prognostic_vars%div(mx, nx, kx, 2))
        allocate (prognostic_vars%t(mx, nx, kx, 2))
        allocate (prognostic_vars%ps(mx, nx, 2))
        allocate (prognostic_vars%tr(mx, nx, kx, 2, ntr))

        allocate (prognostic_vars%phi(mx, nx, kx))
        allocate (prognostic_vars%phis(mx, nx))

        ! Initialize to 0 the allocated arrays
        prognostic_vars%vor = 0
        prognostic_vars%div = 0
        prognostic_vars%t = 0
        prognostic_vars%ps=0
        prognostic_vars%tr=0
        prognostic_vars%phi=0
        prognostic_vars%phis=0        
    end subroutine

    !> Deallocate the memory reserved for the prognostic variables.
    subroutine PrognosticVars_deallocate(prognostic_vars)
        type(PrognosticVars_t), intent(inout) :: prognostic_vars
        ! Deallocate variables
        deallocate (prognostic_vars%vor)
        deallocate (prognostic_vars%div)
        deallocate (prognostic_vars%t)
        deallocate (prognostic_vars%ps)
        deallocate (prognostic_vars%tr)
        deallocate (prognostic_vars%phi)
        deallocate (prognostic_vars%phis)
    end subroutine

    !> Initializes all spectral variables starting from either a reference
    !  atmosphere or a restart file.
    subroutine initialize_prognostics(prognostic_vars, user_params)
        type(PrognosticVars_t), intent(inout) :: prognostic_vars
        type(UserParams_t), intent(in) :: user_params
        call initialize_from_rest_state(prognostic_vars, user_params)
    end subroutine

    !> Initializes all spectral variables starting from a reference atmosphere.
    subroutine initialize_from_rest_state(prognostic_vars, user_params)
        use dynamical_constants, only: gamma, hscale, hshum, refrh1
        use physical_constants, only: grav, rgas
        use geometry, only: fsg
        use boundaries, only: phis0
        use diagnostics, only: check_diagnostics
        use spectral, only: grid_to_spec, trunct
        use input_output, only: output

        type(PrognosticVars_t), intent(inout) :: prognostic_vars
        type(UserParams_t), intent(in) :: user_params

        complex(p) :: surfs(mx, nx)
        real(p) :: surfg(ix, il)
        real(p) :: gam1, esref, gam2, qexp, qref, rgam, rgamr, rlog0, tref, ttop
        integer :: i, j, k

        gam1 = gamma/(1000.0*grav)

        ! 1. Compute spectral surface geopotential
        prognostic_vars%phis = grid_to_spec(phis0)

        ! 2. Start from reference atmosphere (at rest)
        write (*, '(A)') 'Starting from rest'

        ! 2.1 Set vorticity, divergence and tracers to zero
        prognostic_vars%vor(:, :, :, 1) = (0.0, 0.0)
        prognostic_vars%div(:, :, :, 1) = (0.0, 0.0)
        prognostic_vars%tr(:, :, :, 1, :) = (0.0, 0.0)

        ! 2.2 Set reference temperature :
        !     tropos:  T = 288 degK at z = 0, constant lapse rate
        !     stratos: T = 216 degK, lapse rate = 0
        tref = 288.0
        ttop = 216.0
        gam2 = gam1/tref
        rgam = rgas*gam1
        rgamr = 1.0/rgam

        ! Surface and stratospheric air temperature
        prognostic_vars%t(:, :, 1, 1) = (0.0, 0.0)
        prognostic_vars%t(:, :, 2, 1) = (0.0, 0.0)
        surfs = -gam1*prognostic_vars%phis

        prognostic_vars%t(1, 1, 1, 1) = sqrt(2.0)*(1.0, 0.0)*ttop
        prognostic_vars%t(1, 1, 2, 1) = sqrt(2.0)*(1.0, 0.0)*ttop
        surfs(1, 1) = sqrt(2.0)*(1.0, 0.0)*tref - gam1*prognostic_vars%phis(1, 1)

        ! Temperature at tropospheric levels
        do k = 3, kx
            prognostic_vars%t(:, :, k, 1) = surfs*fsg(k)**rgam
        end do

        ! 2.3 Set log(ps) consistent with temperature profile
        !     p_ref = 1013 hPa at z = 0
        rlog0 = log(1.013)

        do j = 1, il
            do i = 1, ix
                surfg(i, j) = rlog0 + rgamr*log(1.0 - gam2*phis0(i, j))
            end do
        end do

        prognostic_vars%ps(:, :, 1) = grid_to_spec(surfg)
        if (ix == iy*4) call trunct(prognostic_vars%ps)

        ! 2.4 Set tropospheric specific humidity in g/kg
        !     Qref = RHref * Qsat(288K, 1013hPa)
        esref = 17.0
        qref = refrh1*0.622*esref
        qexp = hscale/hshum

        ! Specific humidity at the surface
        do j = 1, il
            do i = 1, ix
                surfg(i, j) = qref*exp(qexp*surfg(i, j))
            end do
        end do

        surfs = grid_to_spec(surfg)
        if (ix == iy*4) call trunct(surfs)

        ! Specific humidity at tropospheric levels
        do k = 3, kx
            prognostic_vars%tr(:, :, k, 1, 1) = surfs*fsg(k)**qexp
        end do

        ! Print diagnostics from initial conditions
        call check_diagnostics(prognostic_vars%vor(:, :, :, 1), &
                               prognostic_vars%div(:, :, :, 1), &
                               prognostic_vars%t(:, :, :, 1), &
                               0, user_params%nstdia)

        ! Write initial data
        call output(0, prognostic_vars%vor, prognostic_vars%div, prognostic_vars%t, &
                    prognostic_vars%ps, prognostic_vars%tr, prognostic_vars%phi)
    end subroutine
end module
