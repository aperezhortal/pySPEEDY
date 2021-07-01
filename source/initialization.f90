!> author: Sam Hatfield, Fred Kucharski, Franco Molteni
!  date: 08/05/2019
!  The master initialization module.
module initialization
    implicit none

    private
    public initialize_state, initialize_modules, deinitialize_modules

    logical, save :: modules_initialized_flag = .false.

contains

    ! Intialize global variables in the modules. This is done only once.
    subroutine initialize_modules()
        use geometry, only: initialize_geometry
        use spectral, only: initialize_spectral
        use geopotential, only: initialize_geopotential
        use horizontal_diffusion, only: initialize_horizontal_diffusion

        if (modules_initialized_flag) then
            !Do nothing, the module is already initialized.
            return
        end if 
        
        ! Initialize model geometry
        call initialize_geometry

        ! Initialize spectral transforms
        call initialize_spectral

        ! Initialize geopotential calculations
        call initialize_geopotential

        ! Initialize horizontal diffusion
        call initialize_horizontal_diffusion

        modules_initialized_flag = .true.
    end subroutine

    ! Deinitialize the allocatable global variables in the different modules.
    subroutine deinitialize_modules()
        use spectral, only: deinitialize_spectral
        use horizontal_diffusion, only: deinitialize_horizontal_diffusion

        call deinitialize_spectral
        call deinitialize_horizontal_diffusion

        modules_initialized_flag = .false.
    end subroutine

    !> Initializes everything.
    subroutine initialize_state(state, user_params, control_params)
        use params, only: issty0, UserParams_t
        use date, only: initialize_date, ControlParams_t
        use coupler, only: initialize_coupler
        use sea_model, only: sea_coupling_flag, sst_anomaly_coupling_flag
        use input_output, only: output
        use time_stepping, only: first_step
        use boundaries, only: initialize_boundaries
        use model_state, only: ModelState_t
        use prognostics, only: initialize_prognostics
        use forcing, only: set_forcing

        ! =========================================================================
        ! Subroutine definitions
        ! =========================================================================
        type(ModelState_t), intent(inout) :: state
        type(UserParams_t), intent(out) :: user_params
        type(ControlParams_t), intent(out)  :: control_params

        call print_speedy_title
        
        ! Intialize modules if they were not initialized.
        call initialize_modules()

        ! Initialize date
        call initialize_date(control_params)

        ! Initialize month index for reading SST anomaly file
        control_params%isst0 = (control_params%start_datetime%year - issty0)*12 &
                               + control_params%start_datetime%month

        ! Check consistency of coupling and prescribed SST anomaly flags
        if (sea_coupling_flag >= 4) sst_anomaly_coupling_flag = 1

        ! =========================================================================
        ! Initialization of atmospheric model constants and variables
        ! =========================================================================

        ! Initialize boundary conditions (land-sea mask, sea ice etc.)
        call initialize_boundaries(state)

        ! Initialize model variables
        call initialize_prognostics(state, user_params, control_params)

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
    end subroutine

    !> Prints SPEEDY.f90 banner.
    subroutine print_speedy_title
        write (*, '(A)') ''
        write (*, '(A)') '  _____ ______  _____  _____ ______ __   __     __  _____  _____'
        write (*, '(A)') ' /  ___|| ___ \|  ___||  ___||  _  \\ \ / /    / _||  _  ||  _  |'
        write (*, '(A)') ' \ `--. | |_/ /| |__  | |__  | | | | \ V /    | |_ | |_| || |/  |'
        write (*, '(A)') '  `--. \|  __/ |  __| |  __| | | | |  \ /     |  _|\____ ||  /| |'
        write (*, '(A)') ' /\__/ /| |    | |___ | |___ | |/ /   | |   _ | |  .___/ /\ |_/ /'
        write (*, '(A)') ' \____/ \_|    \____/ \____/ |___/    \_/  (_)|_|  \____/  \___/'
        write (*, '(A)') ''
    end subroutine
end module
