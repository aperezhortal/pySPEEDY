!> author: Sam Hatfield, Fred Kucharski, Franco Molteni
!> date: 29/04/2019
!> The top-level program. Here we initialize the model and run the main loop
!> until the (continually updated) model datetime (`model_datetime`) equals the
!> final datetime (`end_datetime`).
module speedy
    implicit none

    private
    public run_speedy

contains

    subroutine run_speedy( &
        vor, div, t, ps, tr, phi, phis, &
        precnv, &
        precls, &
        snowcv, &
        snowls, &
        cbmf, &
        tsr, &
        ssrd, &
        ssr, &
        slrd, &
        slr, &
        olr, &
        slru, &
        ustr, &
        vstr, &
        shf, &
        evap, &
        hfluxn, &
        mx, nx, kx, ntr, ix, iy, il &  ! Shape of the arrays. Needed for the f2py interface.
        )

        ! For this function, we explicity add all the variables that needs to be saved
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
        use model_variables, only: ModelVars_t, ModelVars_deallocate

        implicit none

        !============ INPUT VARIABLES ==================================================
        ! Prognostic variables
        complex(p), target, intent(inout) :: vor(mx, nx, kx, 2)   !! Vorticity
        complex(p), target, intent(inout) :: div(mx, nx, kx, 2)   !! Divergence
        complex(p), target, intent(inout) :: t(mx, nx, kx, 2)     !! Absolute temperature
        complex(p), target, intent(inout) :: ps(mx, nx, 2)       !! Log of (normalised) surface pressure (p_s/p0)
        complex(p), target, intent(inout) :: tr(mx, nx, kx, 2, ntr) !! Tracers (tr(1): specific humidity in g/kg)
        complex(p), target, intent(inout) :: phi(mx, nx, kx)      !! Atmospheric geopotential
        complex(p), target, intent(inout) :: phis(mx, nx)        !! Surface geopotential

        ! Auxiliary variables
        real(p), target, intent(inout) :: precnv(ix, il) !! Convective precipitation  [g/(m^2 s)], total
        real(p), target, intent(inout) :: precls(ix, il) !! Large-scale precipitation [g/(m^2 s)], total
        real(p), target, intent(inout) :: snowcv(ix, il) !! Convective precipitation  [g/(m^2 s)], snow only
        real(p), target, intent(inout) :: snowls(ix, il) !! Large-scale precipitation [g/(m^2 s)], snow only
        real(p), target, intent(inout) :: cbmf(ix, il)   !! Cloud-base mass flux
        real(p), target, intent(inout) :: tsr(ix, il)    !! Top-of-atmosphere shortwave radiation (downward)
        real(p), target, intent(inout) :: ssrd(ix, il)   !! Surface shortwave radiation (downward-only)
        real(p), target, intent(inout) :: ssr(ix, il)    !! Surface shortwave radiation (net downward)
        real(p), target, intent(inout) :: slrd(ix, il)   !! Surface longwave radiation (downward-only)
        real(p), target, intent(inout) :: slr(ix, il)    !! Surface longwave radiation (net upward)
        real(p), target, intent(inout) :: olr(ix, il)    !! Outgoing longwave radiation (upward)
        real(p), target, intent(inout) :: slru(ix, il, 3) !! Surface longwave emission (upward)

        ! Third dimension -> 1:land, 2:sea, 3: weighted average
        real(p), target, intent(inout) :: ustr(ix, il, 3)   !! U-stress
        real(p), target, intent(inout) :: vstr(ix, il, 3)   !! V-stress
        real(p), target, intent(inout) :: shf(ix, il, 3)    !! Sensible heat flux
        real(p), target, intent(inout) :: evap(ix, il, 3)   !! Evaporation [g/(m^2 s)]
        real(p), target, intent(inout) :: hfluxn(ix, il, 3) !! Net heat flux into surface

        ! variables dimensions
        integer, intent(in) :: mx, nx, kx, ntr, ix, iy, il

        ! integer, intent(inout) :: nstdia     !! Period (number of steps) for diagnostic print-out
        ! integer, intent(inout) :: nsteps_out !! Number of time steps between outputs
        !===============================================================================
        type(UserParams_t)     :: user_params
        type(ControlParams_t)  :: control_params
        type(ModelVars_t) :: model_variables

        ! Time step counter
        integer :: model_step = 1

        !===============================================================================
        ! Step 0: Initialize the grid_t structure with the input data.
        !===============================================================================
        model_variables%vor => vor
        model_variables%div => div
        model_variables%t => t
        model_variables%ps => ps
        model_variables%tr => tr
        model_variables%phi => phi
        model_variables%phis => phis

        model_variables%precnv => precnv
        model_variables%precls => precls
        model_variables%snowcv => snowcv
        model_variables%snowls => snowls
        model_variables%cbmf => cbmf
        model_variables%tsr => tsr
        model_variables%ssrd => ssrd
        model_variables%ssr => ssr
        model_variables%slrd => slrd
        model_variables%slr => slr
        model_variables%olr => olr
        model_variables%slru => slru
        model_variables%ustr => ustr
        model_variables%vstr => vstr
        model_variables%shf => shf
        model_variables%evap => evap
        model_variables%hfluxn => hfluxn

        !===============================================================================
        ! user_params%nstdia=nstdia
        ! user_params%nsteps_out=nsteps_out

        ! Initialization
        call initialize(model_variables, user_params, control_params)

        ! Model main loop
        do while (.not. datetime_equal(control_params%model_datetime, control_params%end_datetime))

            ! Daily tasks
            if (mod(model_step - 1, nsteps) == 0) then
                ! Set forcing terms according to date
                call set_forcing(1, control_params%model_datetime, control_params%tyear)
            end if

            ! Determine whether to compute shortwave radiation on this time step
            compute_shortwave = mod(model_step, nstrad) == 1

            ! Perform one leapfrog time step
            call step(model_variables, 2, 2, 2*delt)

            ! Check model diagnostics
            call check_diagnostics(model_variables%vor(:, :, :, 2), &
                                   model_variables%div(:, :, :, 2), &
                                   model_variables%t(:, :, :, 2), &
                                   model_step, user_params%nstdia)

            ! Increment time step counter
            model_step = model_step + 1

            ! Increment model datetime
            call advance_date(control_params)

            ! Output
            if (mod(model_step - 1, user_params%nsteps_out) == 0) then
                call output(model_step - 1, control_params, &
                            model_variables%vor, model_variables%div, &
                            model_variables%t, &
                            model_variables%ps, model_variables%tr, &
                            model_variables%phi)
            end if

            ! Exchange data with coupler
            call couple_sea_land(model_variables, 1 + model_step/nsteps, control_params)

            write (*, '(A12,I4,A,I0.2,A,I0.2,A,I0.2,A,I0.2)') 'model_datetime: ', &
            & control_params%model_datetime%year, '/', control_params%model_datetime%month, &
            '/', control_params%model_datetime%day, ' ', &
            & control_params%model_datetime%hour, ':', control_params%model_datetime%minute

            write (*, '(A12,I4,A,I0.2,A,I0.2,A,I0.2,A,I0.2)') 'End date: ', &
            & control_params%end_datetime%year, '/', control_params%end_datetime%month, '/', &
             control_params%end_datetime%day, ' ', &
            & control_params%end_datetime%hour, ':', control_params%end_datetime%minute

        end do

        call ModelVars_deallocate(model_variables)
    end subroutine
end module
