!> author: Sam Hatfield, Fred Kucharski, Franco Molteni
!> date: 29/04/2019
!> The top-level program. Here we initialize the model and run the main loop
!> until the (continually updated) model datetime (`model_datetime`) equals the
!> final datetime (`end_datetime`).
module speedy

    implicit none

    private
    public run_speedy

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

    subroutine run_speedy(state, history_interval, diagnostic_interval)
        ! For this function, we explicity pass all the variables that needs to be saved
        ! to facilitate the python-fortran interface.

        use types, only: p
        use params, only: nsteps, delt, nsteps, nstrad
        use date, only: advance_date, datetime_equal, ControlParams_t
        use shortwave_radiation, only: compute_shortwave
        use input_output, only: output
        use coupler, only: couple_sea_land
        use initialization, only: initialize_state
        use time_stepping, only: step
        use diagnostics, only: check_diagnostics
        use forcing, only: set_forcing
        use model_state, only: ModelState_t, ModelState_deallocate, ModelState_allocate
        use spectral, only: spec_to_grid
        implicit none

        !> The model state needs to be initilialized before calling this function.
        type(ModelState_t), intent(inout) :: state
        integer, intent(in) :: history_interval, diagnostic_interval
        type(ControlParams_t)  :: control_params

        ! Time step counter
        integer :: model_step = 1
        
        !===============================================================================
        control_params%nstdia=diagnostic_interval
        control_params%nsteps_out=history_interval

        ! Initialization
        call initialize_state(state, control_params)

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
                                   model_step, control_params%nstdia)

            ! Increment time step counter
            model_step = model_step + 1

            ! Increment model datetime
            call advance_date(control_params)

            ! Output
            if (mod(model_step - 1, control_params%nsteps_out) == 0) then
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

    end subroutine

end module
