!> authors: Sam Hatfield, Fred Kucharski, Franco Molteni
!> date: 29/04/2019
!> The top-level program. Here we initialize the model and run the main loop
!> until the (continually updated) model datetime (`model_datetime`) equals the
!> final datetime (`end_datetime`)
!
! Changelog:
!
!> 04/07/2021: Expose a single step integrator for the python bridge (A. Perez Hortal).

module speedy

    implicit none

    private
    public do_single_step

contains

    subroutine do_single_step(state, control_params, error_code)
        use types, only : p
        use params, only : nsteps, delt, nsteps, nstrad
        use model_control, only : advance_date, datetime_equal, ControlParams_t
        use shortwave_radiation, only : compute_shortwave
        use coupler, only : couple_sea_land
        use initialization, only : initialize_state
        use time_stepping, only : step
        use diagnostics, only : check_diagnostics
        use forcing, only : set_forcing
        use model_state, only : ModelState_t, ModelState_deallocate, ModelState_allocate
        use spectral
        use error_codes
        implicit none

        !> The model state needs to be initilialized before calling this function.
        type(ModelState_t), intent(inout) :: state
        type(ControlParams_t), intent(inout) :: control_params
        integer, intent(out) :: error_code

        error_code = 0
        ! Check if model was initialized
        if (.not. state%initialized) then
            error_code = E_STATE_NOT_INITIALIZED
            return
        end if

        ! Daily tasks
        if (mod(state%current_step, nsteps) == 0) then
            ! Set forcing terms according to date
            call set_forcing(state, 1, control_params%model_datetime, control_params%tyear)
        end if

        ! Determine whether to compute shortwave radiation on this time step
        compute_shortwave = mod(state%current_step, nstrad) == 0

        ! Perform one leapfrog time step
        call step(state, 2, 2, 2 * delt)

        ! Check model diagnostics
        call check_diagnostics(state%vor(:, :, :, 2), &
                state%div(:, :, :, 2), &
                state%t(:, :, :, 2), &
                state%current_step + 1, &
                control_params%diag_interval)

        ! Increment time step counter
        state%current_step = state%current_step + 1

        ! Increment model datetime
        call advance_date(control_params)

        ! Exchange data with coupler
        call couple_sea_land(state, 1 + state%current_step / nsteps, control_params)

    end subroutine

end module
