!> author: Sam Hatfield, Fred Kucharski, Franco Molteni
!> date: 29/04/2019
!> The top-level program. Here we initialize the model and run the main loop
!> until the (continually updated) model datetime (`model_datetime`) equals the
!> final datetime (`end_datetime`).
program speedy
    use params, only: nsteps, delt, nsteps, nstrad, UserParams_t
    use date, only: model_datetime, end_datetime, newdate, datetime_equal
    use shortwave_radiation, only: compute_shortwave
    use input_output, only: output
    use coupler, only: couple_sea_land
    use initialization, only: initialize
    use time_stepping, only: step
    use diagnostics, only: check_diagnostics
    use prognostics, only: PrognosticVars_t, PrognosticVars_deallocate, PrognosticVars_allocate
    use forcing, only: set_forcing

    implicit none

    ! Time step counter
    integer :: model_step = 1

    type(UserParams_t) :: user_params
    type(PrognosticVars_t) :: prognostic_vars

    ! Initialization
    call PrognosticVars_allocate(prognostic_vars)
    call initialize(prognostic_vars, user_params)

    ! Model main loop
    do while (.not. datetime_equal(model_datetime, end_datetime))

        ! Daily tasks
        if (mod(model_step - 1, nsteps) == 0) then
            ! Set forcing terms according to date
            call set_forcing(1)
        end if

        ! Determine whether to compute shortwave radiation on this time step
        compute_shortwave = mod(model_step, nstrad) == 1

        ! Perform one leapfrog time step
        call step(prognostic_vars, 2, 2, 2*delt)

        ! Check model diagnostics
        call check_diagnostics(prognostic_vars%vor(:, :, :, 2), &
                               prognostic_vars%div(:, :, :, 2), &
                               prognostic_vars%t(:, :, :, 2), &
                               model_step, user_params%nstdia)

        ! Increment time step counter
        model_step = model_step + 1

        ! Increment model datetime
        call newdate

        ! Output
        if (mod(model_step - 1, user_params%nsteps_out) == 0) then
            call output(model_step - 1, prognostic_vars%vor, prognostic_vars%div, &
                        prognostic_vars%t, prognostic_vars%ps, prognostic_vars%tr, &
                        prognostic_vars%phi)
        end if

        ! Exchange data with coupler
        call couple_sea_land(1 + model_step/nsteps)
    end do

    call PrognosticVars_deallocate(prognostic_vars)
end
