!> authors: Andres Perez Hortal,Sam Hatfield, Fred Kucharski, Franco Molteni
!
!> date 04/07/2021: Expose a single step integrator for the python bridge.
!> date 29/04/2019: Speedy.f90 original version:
module speedy

    implicit none

    private
    public do_single_step

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

    subroutine do_single_step(state, control_params, error_code)
        use types, only: p
        use params, only: nsteps, delt, nsteps, nstrad
        use model_control, only: advance_date, datetime_equal, ControlParams_t
        use shortwave_radiation, only: compute_shortwave
        use input_output, only: output
        use coupler, only: couple_sea_land
        use initialization, only: initialize_state
        use time_stepping, only: step
        use diagnostics, only: check_diagnostics
        use forcing, only: set_forcing
        use model_state, only: ModelState_t, ModelState_deallocate, ModelState_allocate
        use spectral, only: spec_to_grid
        use error_codes
        implicit none

        !> The model state needs to be initilialized before calling this function.
        type(ModelState_t), intent(inout) :: state
        type(ControlParams_t), intent(inout)  :: control_params
        integer, intent(out) :: error_code

        error_code = 0
        ! Check if model was initialized
        if (.not. state%initialized) then
            error_code = E_STATE_NOT_INITIALIZED
            return
        end if

        ! Daily tasks
        if (mod(control_params%model_step - 1, nsteps) == 0) then
            ! Set forcing terms according to date
            call set_forcing(state, 1, control_params%model_datetime, control_params%tyear)
        end if

        ! Determine whether to compute shortwave radiation on this time step
        compute_shortwave = mod(control_params%model_step, nstrad) == 1

        ! Perform one leapfrog time step
        call step(state, 2, 2, 2*delt)

        ! Check model diagnostics
        call check_diagnostics(state%vor(:, :, :, 2), &
                               state%div(:, :, :, 2), &
                               state%t(:, :, :, 2), &
                               control_params%model_step, &
                               control_params%diag_interval)

        ! Increment time step counter
        control_params%model_step = control_params%model_step + 1

        ! Increment model datetime
        call advance_date(control_params)

        ! Output
        if (mod(control_params%model_step - 1, control_params%history_interval) == 0) then
            call output(control_params%model_step - 1, control_params, &
                        state%vor, state%div, &
                        state%t, &
                        state%ps, state%tr, &
                        state%phi)
        end if

        ! Exchange data with coupler
        call couple_sea_land(state, 1 + control_params%model_step/nsteps, control_params)

    end subroutine

end module
