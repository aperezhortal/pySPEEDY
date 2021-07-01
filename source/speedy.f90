!> author: Sam Hatfield, Fred Kucharski, Franco Molteni
!> date: 29/04/2019
!> The top-level program. Here we initialize the model and run the main loop
!> until the (continually updated) model datetime (`model_datetime`) equals the
!> final datetime (`end_datetime`).
module speedy
    use params, only: UserParams_t
    use date, only: ControlParams_t
    use model_state, only: ModelState_t

    implicit none

    private
    public run_speedy, deinitialize_speedy

    !> Structure to represent the entire model state at a given time.
    type model_state
        type(UserParams_t)    :: user_params
        type(ControlParams_t) :: control_params
        type(ModelState_t)     :: state
    end type

    ! !> Structure to hold the entire model state.
    ! type model_state

    ! model_config
    !     ! implicit
    !     ! land_model
    !     ! legendre
    !     ! mod_radcon
    !     ! sea_model
    !     ! shortwave_radiation
    !     ! spectral
    !     ! sppt
    !     ! surface_fluxes
    !     ! vertical_diffusion
    ! end type

contains

    subroutine run_speedy(state)   
        ! For this function, we explicity pass all the variables that needs to be saved
        ! to facilitate the python-fortran interface.

        use types, only: p
        use params, only: nsteps, delt, nsteps, nstrad, UserParams_t
        use date, only: advance_date, datetime_equal, ControlParams_t
        use shortwave_radiation, only: compute_shortwave
        use input_output, only: output
        use coupler, only: couple_sea_land
        use initialization, only: initialize
        use time_stepping, only: step
        use diagnostics, only: check_diagnostics
        use forcing, only: set_forcing
        use model_state, only: ModelState_t, ModelState_deallocate, ModelState_allocate
        use spectral, only: spec_to_grid
        implicit none

        !> The model state needs to be initilialized before calling this function.
        type(ModelState_t), intent(inout) :: state

        ! integer, intent(inout) :: nstdia     !! Period (number of steps) for diagnostic print-out
        ! integer, intent(inout) :: nsteps_out !! Number of time steps between outputs
        !===============================================================================
        type(UserParams_t)     :: user_params
        type(ControlParams_t)  :: control_params


        ! Time step counter
        integer :: model_step = 1

        !===============================================================================
        ! user_params%nstdia=nstdia
        ! user_params%nsteps_out=nsteps_out

        ! Initialization
        ! call ModelState_allocate(state)
        call initialize(state, user_params, control_params)

        ! Model main loop
        do while (.not. datetime_equal(control_params%model_datetime, control_params%end_datetime))

            ! Daily tasks
            if (mod(model_step - 1, nsteps) == 0) then
                ! Set forcing terms according to date
                call set_forcing(state, 1, control_params%model_datetime, control_params%tyear)
            end if

            ! Determine whether to compute shortwave radiation on this time step
            compute_shortwave = mod(model_step, nstrad) == 1

            ! Perform one leapfrog time step
            call step(state, 2, 2, 2*delt)

            ! Check model diagnostics
            call check_diagnostics(state%vor(:, :, :, 2), &
                                   state%div(:, :, :, 2), &
                                   state%t(:, :, :, 2), &
                                   model_step, user_params%nstdia)

            ! Increment time step counter
            model_step = model_step + 1

            ! Increment model datetime
            call advance_date(control_params)

            ! Output
            if (mod(model_step - 1, user_params%nsteps_out) == 0) then
                call output(model_step - 1, control_params, &
                            state%vor, state%div, &
                            state%t, &
                            state%ps, state%tr, &
                            state%phi)
            end if

            ! Exchange data with coupler
            call couple_sea_land(state, 1 + model_step/nsteps, control_params)

            write (*, '(A12,I4,A,I0.2,A,I0.2,A,I0.2,A,I0.2)') 'model_datetime: ', &
            & control_params%model_datetime%year, '/', control_params%model_datetime%month, &
            '/', control_params%model_datetime%day, ' ', &
            & control_params%model_datetime%hour, ':', control_params%model_datetime%minute

            write (*, '(A12,I4,A,I0.2,A,I0.2,A,I0.2,A,I0.2)') 'End date: ', &
            & control_params%end_datetime%year, '/', control_params%end_datetime%month, '/', &
             control_params%end_datetime%day, ' ', &
            & control_params%end_datetime%hour, ':', control_params%end_datetime%minute

        end do

        ! call ModelState_deallocate(state)
    end subroutine


    subroutine deinitialize_speedy()
        use horizontal_diffusion, only: deinitialize_horizontal_diffusion
        call deinitialize_horizontal_diffusion()
    end subroutine
end module
