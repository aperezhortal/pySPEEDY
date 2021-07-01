!> author: Sam Hatfield, Fred Kucharski, Franco Molteni
!  date: 09/05/2019
!  For running the land-surface model.
module land_model
    use types, only: p
    use params

    implicit none

    private
    public stl_am, snowd_am, soilw_am
    public land_model_init, couple_land_atm
    public fmask_l
    public land_coupling_flag
    public sd2sc

    !TODO: Make these variables allocatable.
    real(p), save :: rhcapl(ix, il) !! 1/heat capacity (land)
    real(p), save :: cdland(ix, il) !! 1/dissipation time (land)

    ! Daily observed climatological fields over land
    real(p), save :: stlcl_ob(ix, il)   !! Climatological land surface temperature
    real(p), save :: snowdcl_ob(ix, il) !! Climatological snow depth (water equivalent)
    real(p), save :: soilwcl_ob(ix, il) !! Climatological soil water availability

    ! Land surface fields used by atmospheric model
    real(p), save :: stl_am(ix, il)   !! Land surface temperature
    real(p), save :: snowd_am(ix, il) !! Snow depth (water equivalent)
    real(p), save :: soilw_am(ix, il) !! Soil water availability

    ! Land surface fields from land model
    real(p), save :: stl_lm(ix, il) !! Land-model surface temperature

    ! Land masks
    real(p), save :: fmask_l(ix, il) !! Fraction of land
    real(p), save :: bmask_l(ix, il) !! Binary land mask

    integer :: land_coupling_flag = 1 !! Flag for land-coupling (0: off, 1: on)

    real(p), parameter :: sd2sc = 60.0 !! Snow depth (mm water) corresponding to snow cover = 1

contains
    !> Initializes land model.
    subroutine land_model_init(state)
        use boundaries, only: check_surface_fields, fill_missing_values
        use model_state, only: ModelState_t

        type(ModelState_t), intent(inout) :: state

        ! Auxiliary variables
        integer :: i, j, month
        real(p) :: dmask(ix, il) ! domain mask
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

        allocate (veg(ix, il))

        ! =========================================================================
        ! Initialize land-surface boundary conditions
        ! =========================================================================

        ! Fractional and binary land masks
        fmask_l = state%fmask_orig
        do j = 1, il
            do i = 1, ix
                if (fmask_l(i, j) >= thrsh) then
                    bmask_l(i, j) = 1.0
                    if (state%fmask_orig(i, j) > (1.0 - thrsh)) fmask_l(i, j) = 1.0
                else
                    bmask_l(i, j) = 0.0
                    fmask_l(i, j) = 0.0
                end if
            end do
        end do

        ! Land-surface temperature
        do month = 1, 12
            call fill_missing_values(state%stl12(:, :, month), 0.0_p)
        end do

        call check_surface_fields(bmask_l, 12, 0.0_p, 400.0_p, 273.0_p, state%stl12)

        call check_surface_fields(bmask_l, 12, 0.0_p, 20000.0_p, 0.0_p, state%snowd12)

        ! Read soil moisture and compute soil water availability using vegetation fraction
        ! Read vegetation fraction

        ! Combine high and low vegetation fractions
        veg = max(0.0, state%veg_high + 0.8*state%veg_low)

        ! Read soil moisture
        sdep1 = 70.0
        idep2 = 3
        sdep2 = idep2*sdep1

        swwil2 = idep2*swwil
        rsw = 1.0/(swcap + idep2*(swcap - swwil))

        do month = 1, 12
            ! Combine soil water content from two top layers
            do j = 1, il
                do i = 1, ix
                    swroot = idep2*state%soil_wc_l2(i, j, month)
                    state%soilw12(i, j, month) = min( &
                        & 1.0, rsw*(state%soil_wc_l1(i, j,month) &
                        + veg(i, j)*max(0.0, swroot - swwil2)) &
                    )
                end do
            end do
        end do

        call check_surface_fields(bmask_l, 12, 0.0_p, 10.0_p, 0.0_p, state%soilw12)

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
        flandmin = 1./3.

        ! Heat capacities per m^2 (depth*heat_cap/m^3)
        hcapl = depth_soil*2.50e+6
        hcapli = depth_lice*1.93e+6

        ! 2. Compute constant fields
        ! Set domain mask (blank out sea points)
        dmask(:, :) = 1.

        do j = 1, il
            do i = 1, ix
                if (fmask_l(i, j) .lt. flandmin) dmask(i, j) = 0
            end do
        end do

        ! Set time_step/heat_capacity and dissipation fields
        do j = 1, il
            do i = 1, ix
                if (state%alb0(i, j) .lt. 0.4) then
                    rhcapl(i, j) = delt/hcapl
                else
                    rhcapl(i, j) = delt/hcapli
                end if
            end do
        end do

        cdland(:, :) = dmask(:, :)*tdland/(1.+dmask(:, :)*tdland)

        deallocate (veg)
    end subroutine

    !> Exchanges fluxes between land and atmosphere.
    subroutine couple_land_atm(state, day, imont1, tmonth)
        use interpolation, only: forin5, forint
        use model_state, only: ModelState_t

        type(ModelState_t) :: state

        integer, intent(in) :: day    !! The day (starting at 0 for the first time step)
        real(p), intent(in) :: tmonth !! The fraction of the current month elapsed
        integer, intent(in) :: imont1 !! The month used for computing seasonal forcing fields

        ! Interpolate climatological fields to actual date

        ! Climatological land surface temperature
        call forin5(imont1, state%stl12, stlcl_ob, tmonth)

        ! Climatological snow depth
        call forint(imont1, state%snowd12, snowdcl_ob, tmonth)

        ! Climatological soil water availability
        call forint(imont1, state%soilw12, soilwcl_ob, tmonth)

        ! If it's the first day then initialise the land surface
        ! temperature from climatology
        if (day == 0) then
            stl_lm = stlcl_ob
            stl_am = stlcl_ob
        else
            ! Run the land model if the land model flags is switched on
            if (land_coupling_flag == 1) then
                call run_land_model(state)

                stl_am = stl_lm
                ! Otherwise get the land surface from climatology
            else
                stl_am = stlcl_ob
            end if
        end if

        ! Always get snow depth and soil water availability from climatology
        snowd_am = snowdcl_ob
        soilw_am = soilwcl_ob
    end subroutine

    !> Integrates slab land-surface model for one day.
    subroutine run_land_model(state)
        use model_state, only: ModelState_t

        type(ModelState_t), intent(inout) :: state

        ! Surface temperature anomaly
        real(p) :: tanom(ix, il)

        ! Land-surface (soil/ice-sheet) layer
        ! Anomaly w.r.t. final-time climatological temperature
        tanom = stl_lm - stlcl_ob

        ! Time evolution of temperature anomaly
        tanom = cdland*(tanom + rhcapl*state%hfluxn(:, :, 1))

        ! Full surface temperature at final time
        stl_lm = tanom + stlcl_ob
    end subroutine
end module
