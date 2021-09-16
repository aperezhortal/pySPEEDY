module sea_model
    use types, only : p
    use params

    implicit none

    private
    public sea_model_init, couple_sea_atm
    public sea_coupling_flag

    !> Heat flux coefficient at sea/ice interface [(W/m^2)/deg]
    real(p), parameter :: beta = 1.0

    !> sea_coupling_flag: Flag for sea-surface temperature coupling
    ! 0 = precribed SST, no coupling
    ! 1 = precribed SST, ocean model forced by atmosphere (not supoprted!)
    ! 2 = full (uncorrected) SST from coupled ocean model (not supoprted!)
    ! 3 = SST anomaly from coupled ocean model + observed SST climatology (not supoprted!)
    ! 4 = as 3 with prescribed SST anomaly in ElNino region (not supoprted!)
    integer, parameter :: sea_coupling_flag = 0

    !> ice_coupling_flag: Flag for sea-ice coupling
    integer, parameter :: ice_coupling_flag = 1

    !> sst_anomaly_coupling_flag: Flag for observed SST anomaly
    ! .false. = climatological SST
    ! .true. = observed anomaly
    ! (active if sea_coupling_flag = 0, 1; set to 1 if sea_coupling_flag = 4)


contains
    ! Initialization of sea model
    subroutine sea_model_init(state)
        use boundaries, only : fill_missing_values, check_surface_fields
        use geometry, only : radang
        use model_state, only : ModelState_t

        type(ModelState_t), intent(inout) :: state

        ! Domain mask
        real(p), allocatable, dimension(:, :) :: dmask

        ! Domain flags
        logical :: l_globe, l_northe, l_natlan, l_npacif, l_tropic, l_indian

        ! Heat capacities of mixed-layer and sea-ice
        real(p) :: hcaps(il)
        real(p) :: hcapi(il)

        integer :: i, j, month
        real(p) :: coslat, crad

        ! 1. Set geographical domain, heat capacities and dissipation times
        !    for sea (mixed layer) and sea-ice

        ! Model parameters (default values)

        ! ocean mixed layer depth: d + (d0-d)*(cos_lat)^3
        real(p) :: depth_ml = 60.               ! High-latitude depth
        real(p) :: dept0_ml = 40.               ! Minimum depth (tropics)

        ! sea-ice depth : d + (d0-d)*(cos_lat)^2
        real(p) :: depth_ice = 2.5              ! High-latitude depth
        real(p) :: dept0_ice = 1.5              ! Minimum depth

        ! Dissipation time (days) for sea-surface temp. anomalies
        real(p) :: tdsst = 90.

        ! Minimum fraction of sea for the definition of anomalies
        real(p) :: fseamin = 1. / 3.

        ! Dissipation time (days) for sea-ice temp. anomalies
        real(p) :: tdice = 30.0

        ! Threshold for land-sea mask definition (i.e. minimum fraction of
        ! either land or sea)
        real(p) :: thrsh = 0.1

        ! Geographical domain
        ! note : more than one regional domain may be set .true.
        l_globe = .true.         ! global domain
        l_northe = .false.         ! Northern hem. oceans (lat > 20N)
        l_natlan = .false.         ! N. Atlantic (lat 20-80N, lon 100W-45E)
        l_npacif = .false.         ! N. Pacific  (lat 20-80N, lon 100E-100W)
        l_tropic = .false.         ! Tropics (lat 30S-30N)
        l_indian = .false.         ! Indian Ocean (lat 30S-30N, lon 30-120E)

        allocate(dmask(ix, il))
        ! =========================================================================
        ! Initialize sea-surface boundary conditions
        ! =========================================================================

        ! Fractional and binary sea masks
        do j = 1, il
            do i = 1, ix
                state%fmask_sea(i, j) = 1.0 - state%fmask_orig(i, j)

                if (state%fmask_sea(i, j) >= thrsh) then
                    state%bmask_sea(i, j) = 1.0
                    if (state%fmask_sea(i, j) > (1.0 - thrsh)) state%fmask_sea(i, j) = 1.0
                else
                    state%bmask_sea(i, j) = 0.0
                    state%fmask_sea(i, j) = 0.0
                end if
            end do
        end do

        ! Grid latitudes for sea-surface variables
        state%deglat_s = radang * 90.0 / asin(1.0)

        ! SST
        do month = 1, 12
            call fill_missing_values(state%sst12(:, :, month), 0.0_p)
        end do

        call check_surface_fields(state%bmask_sea, 12, 100.0_p, 400.0_p, 273.0_p, state%sst12)

        ! Sea ice concentration
        state%sea_ice_frac12 = max(state%sea_ice_frac12, 0.0_p)

        call check_surface_fields(state%bmask_sea, 12, 0.0_p, 1.0_p, 0.0_p, &
                state%sea_ice_frac12)

        call check_surface_fields(state%bmask_sea, 3, -50.0_p, 50.0_p, 0.0_p, state%sst_anom)

        ! Climatological fields for the ocean model (TO BE RECODED)
        ! Annual-mean heat flux into sea-surface
        state%hfseacl = 0.0

        !        if (sea_coupling_flag >= 1) then
        !            stop "Model behaviour when sea_coupling_flag >= 1 not implemented yet"
        !        end if

        ! Ocean model SST climatology:
        ! defined by adding SST model bias to observed climatology
        ! (bias may be defined in a different period from climatology)

        !        if (sea_coupling_flag >= 3) then
        !            stop "Model behaviour when sea_coupling_flag >= 3 not implemented yet"
        !        end if

        ! =========================================================================
        ! Compute heat capacities
        ! =========================================================================

        ! Heat capacities per m^2 (depth*heat_cap/m^3)
        crad = asin(1.) / 90.
        do j = 1, il
            coslat = cos(crad * state%deglat_s(j))
            hcaps(j) = 4.18e+6 * (depth_ml + (dept0_ml - depth_ml) * coslat**3)
            hcapi(j) = 1.93e+6 * (depth_ice + (dept0_ice - depth_ice) * coslat**2)
        end do

        ! =========================================================================
        ! Compute constant parameters and fields
        ! =========================================================================

        ! Set domain mask
        if (l_globe) then
            dmask(:, :) = 1.
        else
            dmask(:, :) = 0.
            if (l_northe) call sea_domain('northe', dmask, state%deglat_s)
            if (l_natlan) call sea_domain('natlan', dmask, state%deglat_s)
            if (l_npacif) call sea_domain('npacif', dmask, state%deglat_s)
            if (l_tropic) call sea_domain('tropic', dmask, state%deglat_s)
            if (l_indian) call sea_domain('indian', dmask, state%deglat_s)
        end if

        ! Smooth latitudinal boundaries and blank out land points
        do j = 2, il - 1
            state%rhcaps(:, j) = 0.25 * (dmask(:, j - 1) + 2 * dmask(:, j) + dmask(:, j + 1))
        end do
        dmask(:, 2:il - 1) = state%rhcaps(:, 2:il - 1)

        do j = 1, il
            do i = 1, ix
                if (state%fmask_sea(i, j) < fseamin) dmask(i, j) = 0
            end do
        end do

        ! Set heat capacity and dissipation time over selected domain
        do j = 1, il
            state%rhcaps(:, j) = delt / hcaps(j)
            state%rhcapi(:, j) = delt / hcapi(j)
        end do

        state%cdsea = dmask * tdsst / (1. + dmask * tdsst)
        state%cdice = dmask * tdice / (1. + dmask * tdice)

        deallocate(dmask)
    end

    subroutine couple_sea_atm(state, day, control_params)
        use model_control, only :
        use interpolation, only : forin5, forint, monthly_interp
        use model_control, only : Datetime_t
        use model_state, only : ModelState_t
        use model_control, only : ControlParams_t

        type(ControlParams_t), intent(in) :: control_params
        type(ModelState_t), intent(inout) :: state
        integer, intent(in) :: day

        integer :: i, j
        real(p) :: sstcl0, sstfr

        integer :: sst_anom_shape(3)
        ! 1. Interpolate climatological fields and obs. SST anomaly
        !    to actual date

        ! Climatological SST
        call forin5(control_params%imont1, state%sst12, state%sstcl_ob, control_params%tmonth)

        ! Climatological sea ice fraction
        call forint(control_params%imont1, state%sea_ice_frac12, state%sicecl_ob, control_params%tmonth)

        ! SST anomaly
        if (state%sst_anomaly_coupling_flag) then
            sst_anom_shape = shape(state%sst_anom)
            call monthly_interp(control_params%month_idx, state%sst_anom, &
                    state%sstan_ob, control_params%tmonth, sst_anom_shape(3))
        end if

        ! Ocean model climatological SST
        if (sea_coupling_flag >= 3) then
            call forin5(control_params%imont1, state%sstom12, state%sstcl_om, control_params%tmonth)
        end if

        ! Adjust climatological fields over sea ice

        ! SST at freezing point
        sstfr = 273.2 - 1.8

        do i = 1, ix
            do j = 1, il
                sstcl0 = state%sstcl_ob(i, j)

                if (state%sstcl_ob(i, j) > sstfr) then
                    state%sicecl_ob(i, j) = min(0.5, state%sicecl_ob(i, j))
                    state%ticecl_ob(i, j) = sstfr
                    if (state%sicecl_ob(i, j) > 0.0) then
                        state%sstcl_ob(i, j) = sstfr + (state%sstcl_ob(i, j) - sstfr) / (1.0 - state%sicecl_ob(i, j))
                    end if
                else
                    state%sicecl_ob(i, j) = max(0.5, state%sicecl_ob(i, j))
                    state%ticecl_ob(i, j) = sstfr + (state%sstcl_ob(i, j) - sstfr) / state%sicecl_ob(i, j)
                    state%sstcl_ob(i, j) = sstfr
                end if

                if (sea_coupling_flag >= 3) state%sstcl_om(i, j) = state%sstcl_om(i, j) + (state%sstcl_ob(i, j) - sstcl0)
            end do
        end do

        if (day == 0) then
            ! 2. Initialize prognostic variables of ocean/ice model
            !    in case of no restart or no coupling
            state%sst_om = state%sstcl_ob      ! SST
            state%tice_om = state%ticecl_ob     ! sea ice temperature
            state%sice_om = state%sicecl_ob     ! sea ice fraction

            if (sea_coupling_flag <= 0) state%sst_om = 0.0

            ! 3. Compute additional sea/ice variables
            state%wsst_ob = 0.
            if (sea_coupling_flag >= 4) call sea_domain('elnino', state%wsst_ob, state%deglat_s)
        else
            if (sea_coupling_flag > 0 .or. ice_coupling_flag > 0) then
                ! 1. Run ocean mixed layer or
                !    call message-passing routines to receive data from ocean model
                call run_sea_model(state)
            end if
        end if

        ! 3. Compute sea-sfc. anomalies and full fields for atm. model
        ! 3.1 SST
        state%sstan_am = 0.0

        if (sea_coupling_flag <= 1) then
            if (state%sst_anomaly_coupling_flag) state%sstan_am = state%sstan_ob

            ! Use observed SST (climatological or full field)
            state%sst_am = state%sstcl_ob + state%sstan_am
        else if (sea_coupling_flag == 2) then
            ! Use full ocean model SST
            state%sst_am = state%sst_om
        else if (sea_coupling_flag >= 3) then
            ! Define SST anomaly from ocean model ouput and climatology
            state%sstan_am = state%sst_om - state%sstcl_om

            ! Merge with observed SST anomaly in selected area
            if (sea_coupling_flag >= 4) then
                state%sstan_am = state%sstan_am + state%wsst_ob * (state%sstan_ob - state%sstan_am)
            end if

            ! Add observed SST climatology to model SST anomaly
            state%sst_am = state%sstcl_ob + state%sstan_am
        end if

        ! 3.2 Sea ice fraction and temperature
        if (ice_coupling_flag > 0) then
            state%sice_am = state%sice_om
            state%tice_am = state%tice_om
        else
            state%sice_am = state%sicecl_ob
            state%tice_am = state%ticecl_ob
        end if

        state%sst_am = state%sst_am + state%sice_am * (state%tice_am - state%sst_am)
        state%ssti_om = state%sst_om + state%sice_am * (state%tice_am - state%sst_om)
    end subroutine

    ! Purpose : Integrate slab ocean and sea-ice models for one day
    subroutine run_sea_model(state)
        use mod_radcon, only : albsea, albice, emisfc
        use physical_constants, only : alhc, sbc
        use model_state, only : ModelState_t

        type(ModelState_t), intent(inout) :: state

        real(p), allocatable, dimension(:, :) :: &
                hflux, &  ! net sfc. heat flux
                tanom, &   ! sfc. temperature anomaly
                cdis, &   ! dissipation ceofficient
                difice, &  ! Difference in net (downw.) heat flux between ice and sea surface
                hflux_i ! Net heat flux into sea-ice surface

        real(p) :: anom0, sstfr

        ! Allocate vars
        allocate(hflux(ix, il), tanom(ix, il), cdis(ix, il), &
                difice(ix, il), hflux_i(ix, il))

        sstfr = 273.2 - 1.8       ! SST at freezing point



        ! 1. Ocean mixed layer

        ! Difference in heat flux between ice and sea surface
        difice = (albsea - albice) * state%ssrd &
                + emisfc * sbc * (sstfr**4.0 - state%tice_am**4.0) &
                + state%shf(:, :, 2) + &
                & state%evap(:, :, 2) * alhc

        ! Net heat flux into sea-ice surface
        hflux_i = state%hfluxn(:, :, 2) + difice * (1.0 - state%sice_am)

        ! Net heat flux
        hflux = state%hfluxn(:, :, 2) - state%hfseacl - state%sicecl_ob * (hflux_i + beta * (sstfr - state%tice_om))

        ! Anomaly at t0 minus climatological temp. tendency
        tanom = state%sst_om - state%sstcl_ob

        ! Time evoloution of temp. anomaly
        tanom = state%cdsea * (tanom + state%rhcaps * hflux)

        ! Full SST at final time
        state%sst_om = tanom + state%sstcl_ob

        ! 2. Sea-ice slab model

        ! Net heat flux
        hflux = hflux_i + beta * (sstfr - state%tice_om)

        ! Anomaly w.r.t final-time climatological temp.
        tanom = state%tice_om - state%ticecl_ob

        ! Definition of non-linear damping coefficient
        anom0 = 20.
        cdis = state%cdice * (anom0 / (anom0 + abs(tanom)))
        !cdis(:,:) = state%cdice(:,:)

        ! Time evolution of temp. anomaly
        tanom = cdis * (tanom + state%rhcapi * hflux)

        ! Full ice temperature at final time
        state%tice_om = tanom + state%ticecl_ob

        ! Persistence of sea ice fraction
        state%sice_om = state%sicecl_ob

        deallocate(hflux, tanom, cdis, difice, hflux_i)
    end

    ! Definition of ocean domains
    subroutine sea_domain(cdomain, dmask, deglat_s)
        character(len = 6), intent(in) :: cdomain ! domain name

        ! Output variables (initialized by calling routine)
        real(p), intent(inout) :: dmask(ix, il)
        real(p), intent(in) :: deglat_s(il)

        integer :: i, j
        real(p) :: arlat, dlon, rlon, rlonw, wlat

        print *, 'sea domain : ', cdomain

        dlon = 360. / float(ix)

        if (cdomain == 'northe') then
            do j = 1, il
                if (deglat_s(j) > 20.0) dmask(:, j) = 1.
            end do
        end if

        if (cdomain == 'natlan') then
            do j = 1, il
                if (deglat_s(j) > 20.0 .and. deglat_s(j) < 80.0) then
                    do i = 1, ix
                        rlon = (i - 1) * dlon
                        if (rlon < 45.0 .or. rlon > 260.0) dmask(i, j) = 1.
                    end do
                end if
            end do
        end if

        if (cdomain == 'npacif') then
            do j = 1, il
                if (deglat_s(j) > 20.0 .and. deglat_s(j) < 65.0) then
                    do i = 1, ix
                        rlon = (i - 1) * dlon
                        if (rlon > 120.0 .and. rlon < 260.0) dmask(i, j) = 1.
                    end do
                end if
            end do
        end if

        if (cdomain == 'tropic') then
            do j = 1, il
                if (deglat_s(j) > -30.0 .and. deglat_s(j) < 30.0) dmask(:, j) = 1.
            end do
        end if

        if (cdomain == 'indian') then
            do j = 1, il
                if (deglat_s(j) > -30.0 .and. deglat_s(j) < 30.0) then
                    do i = 1, ix
                        rlon = (i - 1) * dlon
                        if (rlon > 30.0 .and. rlon < 120.0) dmask(i, j) = 1.
                    end do
                end if
            end do
        end if

        if (cdomain == 'elnino') then
            do j = 1, il
                arlat = abs(deglat_s(j))
                if (arlat < 25.0) then
                    wlat = 1.
                    if (arlat > 15.0) wlat = (0.1 * (25. - arlat))**2
                    rlonw = 300. - 2 * max(deglat_s(j), 0.)
                    do i = 1, ix
                        rlon = (i - 1) * dlon
                        if ((rlon > 165.0) .and. (rlon < rlonw)) then
                            dmask(i, j) = wlat
                        else if ((rlon > 155.0) .and. (rlon < 165.0)) then
                            dmask(i, j) = wlat * 0.1 * (rlon - 155.)
                        end if
                    end do
                end if
            end do
        end if
    end
end module
