!> author: Sam Hatfield, Fred Kucharski, Franco Molteni
!  date: 09/05/2019
!  For running the land-surface model.
module land_model
    use types, only : p
    use params

    implicit none

    private
    public land_model_init, couple_land_atm
    public snow_depth2cover

    real(p), parameter :: snow_depth2cover = 60.0 !! Snow depth (mm water) corresponding to snow cover = 1

contains
    !> Initializes land model.
    subroutine land_model_init(state)
        use boundaries, only : check_surface_fields, fill_missing_values
        use model_state, only : ModelState_t

        type(ModelState_t), intent(inout) :: state

        ! Auxiliary variables
        integer :: i, j, month
        real(p), allocatable, dimension(:, :) :: dmask ! domain mask
        real(p) :: depth_soil, depth_lice, tdland, hcapl, hcapli, flandmin

        ! Soil moisture parameters
        ! Soil wetness at field capacity (volume fraction)
        real(p) :: swcap = 0.30

        ! Soil wetness at wilting point  (volume fraction)
        real(p) :: swwil = 0.17

        ! Threshold for land-sea mask definition (i.e. minimum fraction of
        ! either land or sea)
        real(p) :: thrsh = 0.1

        real(p) :: rsw, sdep1, sdep2, swroot, swwil2
        real(p), allocatable, dimension(:, :) :: veg
        integer :: idep2

        allocate (veg(ix, il), dmask(ix, il))

        ! =========================================================================
        ! Initialize land-surface boundary conditions
        ! =========================================================================

        ! Fractional and binary land masks
        state%fmask_land = state%fmask_orig
        do j = 1, il
            do i = 1, ix
                if (state%fmask_land(i, j) >= thrsh) then
                    state%bmask_land(i, j) = 1.0
                    if (state%fmask_orig(i, j) > (1.0 - thrsh)) state%fmask_land(i, j) = 1.0
                else
                    state%bmask_land(i, j) = 0.0
                    state%fmask_land(i, j) = 0.0
                end if
            end do
        end do

        ! Land-surface temperature
        do month = 1, 12
            call fill_missing_values(state%stl12(:, :, month), 0.0_p)
        end do

        call check_surface_fields(state%bmask_land, 12, 0.0_p, 400.0_p, 273.0_p, state%stl12)

        call check_surface_fields(state%bmask_land, 12, 0.0_p, 20000.0_p, 0.0_p, state%snowd12)

        ! Read soil moisture and compute soil water availability using vegetation fraction
        ! Read vegetation fraction

        ! Combine high and low vegetation fractions
        veg = max(0.0, state%veg_high + 0.8 * state%veg_low)

        ! Read soil moisture
        sdep1 = 70.0
        idep2 = 3
        sdep2 = idep2 * sdep1

        swwil2 = idep2 * swwil
        rsw = 1.0 / (swcap + idep2 * (swcap - swwil))

        do month = 1, 12
            ! Combine soil water content from two top layers
            do j = 1, il
                do i = 1, ix
                    swroot = idep2 * state%soil_wc_l2(i, j, month)
                    state%soilw12(i, j, month) = min(&
                            & 1.0, rsw * (state%soil_wc_l1(i, j, month) &
                                    + veg(i, j) * max(0.0, swroot - swwil2)) &
                            )
                end do
            end do
        end do

        call check_surface_fields(state%bmask_land, 12, 0.0_p, 10.0_p, 0.0_p, state%soilw12)

        ! =========================================================================
        ! Set heat capacities and dissipation times for soil and ice-sheet layers
        ! =========================================================================

        ! Model parameters (default values)

        ! Soil layer depth (m)
        depth_soil = 1.0

        ! Land-ice depth (m)
        depth_lice = 5.0

        ! Dissipation time (days) for land-surface temp. anomalies
        tdland = 40.

        ! Minimum fraction of land for the definition of anomalies
        flandmin = 1. / 3.

        ! Heat capacities per m^2 (depth*heat_cap/m^3)
        hcapl = depth_soil * 2.50e+6
        hcapli = depth_lice * 1.93e+6

        ! 2. Compute constant fields
        ! Set domain mask (blank out sea points)
        dmask(:, :) = 1.

        do j = 1, il
            do i = 1, ix
                if (state%fmask_land(i, j) .lt. flandmin) dmask(i, j) = 0
            end do
        end do

        ! Set time_step/heat_capacity and dissipation fields
        do j = 1, il
            do i = 1, ix
                if (state%alb0(i, j) .lt. 0.4) then
                    state%rhcapl(i, j) = delt / hcapl
                else
                    state%rhcapl(i, j) = delt / hcapli
                end if
            end do
        end do

        state%cdland(:, :) = dmask(:, :) * tdland / (1. + dmask(:, :) * tdland)

        deallocate (veg, dmask)
    end subroutine

    !> Exchanges fluxes between land and atmosphere.
    subroutine couple_land_atm(state, day, imont1, tmonth)
        use interpolation, only : forin5, forint
        use model_state, only : ModelState_t

        type(ModelState_t) :: state

        integer, intent(in) :: day    !! The day (starting at 0 for the first time step)
        real(p), intent(in) :: tmonth !! The fraction of the current month elapsed
        integer, intent(in) :: imont1 !! The month used for computing seasonal forcing fields

        ! Interpolate climatological fields to actual date

        ! Climatological land surface temperature
        call forin5(imont1, state%stl12, state%stlcl_obs, tmonth)

        ! Climatological snow depth
        call forint(imont1, state%snowd12, state%snowdcl_obs, tmonth)

        ! Climatological soil water availability
        call forint(imont1, state%soilw12, state%soilwcl_obs, tmonth)

        ! If it's the first day then initialise the land surface
        ! temperature from climatology
        if (day == 0) then
            state%stl_lm = state%stlcl_obs
            state%land_temp = state%stlcl_obs
        else
            ! Run the land model if the land model flags is switched on
            if (state%land_coupling_flag) then
                call run_land_model(state)

                state%land_temp = state%stl_lm
                ! Otherwise get the land surface from climatology
            else
                state%land_temp = state%stlcl_obs
            end if
        end if

        ! Always get snow depth and soil water availability from climatology
        state%snow_depth = state%snowdcl_obs
        state%soil_avail_water = state%soilwcl_obs
    end subroutine

    !> Integrates slab land-surface model for one day.
    subroutine run_land_model(state)
        use model_state, only : ModelState_t

        type(ModelState_t), intent(inout) :: state

        ! Surface temperature anomaly
        real(p), allocatable, dimension(:, :) :: tanom

        allocate(tanom(ix, il))

        ! Land-surface (soil/ice-sheet) layer
        ! Anomaly w.r.t. final-time climatological temperature
        tanom = state%stl_lm - state%stlcl_obs

        ! Time evolution of temperature anomaly
        tanom = state%cdland * (tanom + state%rhcapl * state%hfluxn(:, :, 1))

        ! Full surface temperature at final time
        state%stl_lm = tanom + state%stlcl_obs

        deallocate(tanom)
    end subroutine
end module
