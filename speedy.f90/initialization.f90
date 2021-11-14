!> author: Sam Hatfield, Fred Kucharski, Franco Molteni
!  date: 08/05/2019
!  The master initialization module.
module initialization
    implicit none

    private
    public initialize_state

contains

    !> Initializes everything.
    subroutine initialize_state(state, control_params, error_code)
        use model_control, only : ControlParams_t
        use coupler, only : initialize_coupler
        use sea_model, only : sea_coupling_flag
        use time_stepping, only : first_step
        use boundaries, only : initialize_boundaries
        use model_state, only : ModelState_t
        use prognostics, only : initialize_prognostics
        use geopotential, only : initialize_geopotential
        use forcing, only : set_forcing
        use params, only : ix, il
        use error_codes, only: SUCCESS

        ! =========================================================================
        ! Subroutine definitions
        ! =========================================================================
        type(ModelState_t), intent(inout) :: state
        type(ControlParams_t), intent(out) :: control_params
        integer, intent(out) :: error_code

        integer :: k

        ! call print_speedy_title
        state%current_step = 0

        ! =========================================================================
        ! Module instances initialization
        ! =========================================================================

        ! IMPORTANT: This module need to be intialized first!
        call state%mod_geometry%initialize()

        ! Initialize spectral transforms module
        call state%mod_spectral%initialize(state%mod_geometry)

        ! Initialize implicit module
        call state%mod_implicit%initialize(state%mod_geometry)

        call initialize_geopotential(state)

        ! Check consistency of coupling and prescribed SST anomaly flags
        if (sea_coupling_flag >= 4) state%sst_anomaly_coupling_flag = .true.

        ! =========================================================================
        ! Initialization of atmospheric model constants and variables
        ! =========================================================================

        ! Initialize boundary conditions (land-sea mask, sea ice etc.)
        call initialize_boundaries(state)

        ! Initialize model variables
        call initialize_prognostics(state, error_code)

        if (error_code /= SUCCESS) then
            return
        end if

        ! =========================================================================
        ! Initialization of coupled modules (land, sea, ice)
        ! =========================================================================
        call initialize_coupler(state, control_params)

        ! =========================================================================
        ! Initialization of first time step
        ! =========================================================================
        ! Set up the forcing fields for the first time step
        call set_forcing(state, 0, control_params%model_datetime, control_params%tyear)

        ! Do the initial (2nd-order) time step, initialize the semi-implicit scheme
        call first_step(state)

        ! Initialize coordinates
        state%lev(:) = real(state%mod_geometry%fsg(:))
        state%lon(:) = (/(3.75 * k, k = 0, ix - 1)/)
        state%lat(:) = (/(real(state%mod_geometry%radang(k)) * 90.0 / asin(1.0), k = 1, il)/)

        state%initialized = .true.

    end subroutine

end module
