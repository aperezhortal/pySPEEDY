!> author: Sam Hatfield, Fred Kucharski, Franco Molteni
!> date: 29/04/2019
!> The top-level program. Here we initialize the model and run the main loop
!> until the (continually updated) model datetime (`model_datetime`) equals the
!> final datetime (`end_datetime`).
module speedy
    use params, only: UserParams_t
    use date, only: ControlParams_t
    use model_vars, only: ModelVars_t

    implicit none

    private
    public run_speedy

    !> Structure to represent the entire model state at a given time.
    type model_state
        type(UserParams_t)    :: user_params
        type(ControlParams_t) :: control_params
        type(ModelVars_t)     :: state_vars
    end type

    ! !> Structure to hold the entire model state.
    ! type model_state

    ! model_config

    !     type(UserParams_t)    :: user_params
    !     type(ControlParams_t) :: control_params
    !     type(ModelVars_t)     :: state_vars
    !     type(ModelInputs_t)   :: inputs
    !     type(ModelVars_t)     :: invariant
    !     ! boundaries
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
    subroutine run_speedy( vor, div, t, ps, tr, phi, ix, il, kx, ntr)   
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
        use model_vars, only: ModelVars_t, ModelVars_deallocate, ModelVars_allocate
        use spectral, only: spec_to_grid
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

        ! ! Auxiliary variables
        ! real(p), allocatable, intent(out) :: precnv(:,:) !! Convective precipitation  [g/(m^2 s)], total
        ! real(p), allocatable, intent(out) :: precls(:,:) !! Large-scale precipitation [g/(m^2 s)], total
        ! real(p), allocatable, intent(out) :: snowcv(:,:) !! Convective precipitation  [g/(m^2 s)], snow only
        ! real(p), allocatable, intent(out) :: snowls(:,:) !! Large-scale precipitation [g/(m^2 s)], snow only
        ! real(p), allocatable, intent(out) :: cbmf(:,:)   !! Cloud-base mass flux
        ! real(p), allocatable, intent(out) :: tsr(:,:)    !! Top-of-atmosphere shortwave radiation (downward)
        ! real(p), allocatable, intent(out) :: ssrd(:,:)   !! Surface shortwave radiation (downward-only)
        ! real(p), allocatable, intent(out) :: ssr(:,:)    !! Surface shortwave radiation (net downward)
        ! real(p), allocatable, intent(out) :: slrd(:,:)   !! Surface longwave radiation (downward-only)
        ! real(p), allocatable, intent(out) :: slr(:,:)    !! Surface longwave radiation (net upward)
        ! real(p), allocatable, intent(out) :: olr(:,:)    !! Outgoing longwave radiation (upward)
        ! real(p), allocatable, intent(out) :: slru(:,:,:) !! Surface longwave emission (upward)

        ! ! Third dimension -> 1:land, 2:sea, 3: weighted average
        ! real(p), allocatable, intent(out) :: ustr(:,:,:)   !! U-stress
        ! real(p), allocatable, intent(out) :: vstr(:,:,:)   !! V-stress
        ! real(p), allocatable, intent(out) :: shf(:,:,:)    !! Sensible heat flux
        ! real(p), allocatable, intent(out) :: evap(:,:,:)   !! Evaporation [g/(m^2 s)]
        ! real(p), allocatable, intent(out) :: hfluxn(:,:,:) !! Net heat flux into surface

        ! ! variables dimensions
        ! integer, intent(in) :: kx, ntr, ix, il

        ! integer, intent(inout) :: nstdia     !! Period (number of steps) for diagnostic print-out
        ! integer, intent(inout) :: nsteps_out !! Number of time steps between outputs
        !===============================================================================
        type(UserParams_t)     :: user_params
        type(ControlParams_t)  :: control_params
        type(ModelVars_t) :: state_vars

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
        call ModelVars_allocate(state_vars)
        call initialize(state_vars, user_params, control_params)

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
            call step(state_vars, 2, 2, 2*delt)

            ! Check model diagnostics
            call check_diagnostics(state_vars%vor(:, :, :, 2), &
                                   state_vars%div(:, :, :, 2), &
                                   state_vars%t(:, :, :, 2), &
                                   model_step, user_params%nstdia)

            ! Increment time step counter
            model_step = model_step + 1

            ! Increment model datetime
            call advance_date(control_params)

            ! Output
            if (mod(model_step - 1, user_params%nsteps_out) == 0) then
                call output(model_step - 1, control_params, &
                            state_vars%vor, state_vars%div, &
                            state_vars%t, &
                            state_vars%ps, state_vars%tr, &
                            state_vars%phi)
            end if

            ! Exchange data with coupler
            call couple_sea_land(state_vars, 1 + model_step/nsteps, control_params)

            write (*, '(A12,I4,A,I0.2,A,I0.2,A,I0.2,A,I0.2)') 'model_datetime: ', &
            & control_params%model_datetime%year, '/', control_params%model_datetime%month, &
            '/', control_params%model_datetime%day, ' ', &
            & control_params%model_datetime%hour, ':', control_params%model_datetime%minute

            write (*, '(A12,I4,A,I0.2,A,I0.2,A,I0.2,A,I0.2)') 'End date: ', &
            & control_params%end_datetime%year, '/', control_params%end_datetime%month, '/', &
             control_params%end_datetime%day, ' ', &
            & control_params%end_datetime%hour, ':', control_params%end_datetime%minute

        end do

        ! Export
        do k = 1, kx
            vor(:, :, k) = spec_to_grid(state_vars%vor(:,:,k,1), 0)
            div(:, :, k) = spec_to_grid(state_vars%div(:,:,k,1), 0)
            t(:, :, k) = spec_to_grid(state_vars%t(:,:,k,1), 1)
            phi(:, :, k) = spec_to_grid(state_vars%phi(:, :, k), 1)

            do n=1,ntr
                tr(:, :, k,n) = spec_to_grid(state_vars%tr(:, :, k,1, n), 1)
            end do
        end do
        ps(:,:) = spec_to_grid(state_vars%ps(:, :, 1), 1)

        call ModelVars_deallocate(state_vars)
    end subroutine
end module
