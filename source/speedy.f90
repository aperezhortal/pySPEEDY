!> author: Sam Hatfield, Fred Kucharski, Franco Molteni
!> date: 29/04/2019
!> The top-level program. Here we initialize the model and run the main loop
!> until the (continually updated) model datetime (`model_datetime`) equals the
!> final datetime (`end_datetime`).
module speedy
    use types, only: p
    use params, only: UserParams_t
    use date, only: ControlParams_t
    use model_state, only: ModelState_t

    implicit none

    private
    public run_speedy

    !> Structure to represent the entire model state at a given time.
    ! type model_state
    !     type(UserParams_t)    :: user_params
    !     type(ControlParams_t) :: control_params
    !     type(ModelState_t)     :: state_vars
    ! end type

    !> Model input fields
    ! type ModelInput_t
    !     real(p), allocatable :: orography(:, :)       !! Unfiltered surface geopotential
    !     real(p), allocatable :: land_sea_mask(:, :)   !! Spectrally-filtered surface geopotential
    !     real(p), allocatable :: albedo(:, :)          !! Bare-land annual-mean albedo

    !     real(p), allocatable :: land_sfc_temp(:, :)   !! Bare-land annual-mean albedo
    !     real(p), allocatable :: snow_depth(:, :)      !! Bare-land annual-mean albedo
    !     real(p), allocatable :: vegetation_low(:, :)  !! Bare-land annual-mean albedo
    !     real(p), allocatable :: vegetation_high(:, :) !! Bare-land annual-mean albedo

    !     real(p), allocatable :: soil_wetness_l1(:, :) !! Bare-land annual-mean albedo
    !     real(p), allocatable :: soil_wetness_l2(:, :) !! Bare-land annual-mean albedo
    !     real(p), allocatable :: soil_wetness_l3(:, :) !! Bare-land annual-mean albedo
    !     real(p), allocatable :: sea_ice_conc(:, :)    !! Bare-land annual-mean albedo
    !     real(p), allocatable :: sea_surf_temp(:, :)   !! Bare-land annual-mean albedo
    ! end type

    ! !> Structure to hold the entire model state.
    ! type model_state

    ! model_config

    !     ! geometry
    !     ! geopotential constants
    !     ! horizontal_diffusion
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

    ! 2d_vars
    ! 3d_vars
    ! 4d_vars
    subroutine run_speedy( &
        vor, div, t, ps, tr, phi, &
        orography, land_sea_mask, albedo, &
        ix, il, kx, ntr)
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

        use physical_constants, only: grav
        implicit none

        !============ INPUT VARIABLES ==================================================
        ! Prognostic variables
        integer, intent(in)    :: kx, ntr, ix, il
        real(8), intent(inout) :: vor(ix, il, kx)     !! Vorticity
        real(8), intent(inout) :: div(ix, il, kx)     !! Divergence
        real(8), intent(inout) :: t(ix, il, kx)       !! Absolute temperature
        real(8), intent(inout) :: ps(ix, il)          !! Log of (normalised) surface pressure (p_s/p0)
        real(8), intent(inout) :: tr(ix, il, kx, ntr) !! Tracers (tr(1): specific humidity in g/kg)
        real(8), intent(inout) :: phi(ix, il, kx)     !! Atmospheric geopotential

        real(8), intent(in) :: orography(ix, il)
        real(8), intent(in) :: land_sea_mask(ix, il)
        real(8), intent(in) :: albedo(ix, il)

        ! integer, intent(inout) :: nstdia     !! Period (number of steps) for diagnostic print-out
        ! integer, intent(inout) :: nsteps_out !! Number of time steps between outputs
        !===============================================================================
        type(UserParams_t)     :: user_params
        type(ControlParams_t)  :: control_params
        type(ModelState_t)     :: state

        ! Time step counter
        integer :: model_step = 1
        integer :: n, k
        !===============================================================================
        ! Step 0: Initialize the grid_t structure with the input data.
        !===============================================================================

        !===============================================================================
        ! user_params%nstdia=nstdia
        ! user_params%nsteps_out=nsteps_out

        ! Initialization
        call ModelState_allocate(state)
        call initialize(state, user_params, control_params)

        ! phi0(:,:) = grav*orography(:,:)
        ! fmask_orig(:,:) = land_sea_mask(:,:)
        ! alb0(:,:) = albedo(:,:)

        ! Model main loop
        do while (.not. datetime_equal(control_params%model_datetime, control_params%end_datetime))

            ! Daily tasks
            if (mod(model_step - 1, nsteps) == 0) then
                ! Set forcing terms according to date
                call set_forcing(state, 1, control_params%model_datetime, &
                                 control_params%tyear)
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
                call output(state, model_step - 1, control_params)
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

        ! ! Export
        ! do k = 1, kx
        !     vor(:, :, k) = spec_to_grid(state%vor(:, :, k, 1), 0, state%cosgr)
        !     div(:, :, k) = spec_to_grid(state%div(:, :, k, 1), 0, state%cosgr)
        !     t(:, :, k) = spec_to_grid(state%t(:, :, k, 1), 1, state%cosgr)
        !     phi(:, :, k) = spec_to_grid(state%phi(:, :, k), 1, state%cosgr)

        !     do n = 1, ntr
        !         tr(:, :, k, n) = spec_to_grid(state%tr(:, :, k, 1, n), 1, state%cosgr)
        !     end do
        ! end do
        ! ps(:, :) = spec_to_grid(state%ps(:, :, 1), 1, state%cosgr)

        call ModelState_deallocate(state)
    end subroutine
end module
