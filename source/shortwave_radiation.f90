!> Parametrization of short-wave radiation
module shortwave_radiation
    use types, only : p
    use params

    implicit none

    private
    public get_shortwave_rad_fluxes, get_zonal_average_fields, clouds

    ! Shortwave radiation and cloud constants
    real(p), parameter :: solc = 342.0 !! Solar constant (area averaged) in W/m^2
    real(p), parameter :: rhcl1 = 0.30  !! Relative humidity threshold corresponding to
    !! cloud cover = 0
    real(p), parameter :: rhcl2 = 1.00  !! Relative humidity correponding to cloud cover = 1
    real(p), parameter :: qacl = 0.20  !! Specific humidity threshold for cloud cover
    real(p), parameter :: wpcl = 0.2   !! Cloud cover weight for the square-root of precipitation
    !! (for p = 1 mm/day)
    real(p), parameter :: pmaxcl = 10.0  !! Maximum value of precipitation (mm/day) contributing to cloud cover
    real(p), parameter :: clsmax = 0.60  !! Maximum stratiform cloud cover
    real(p), parameter :: clsminl = 0.15  !! Minimum stratiform cloud cover over land (for RH = 1)
    real(p), parameter :: gse_s0 = 0.25  !! Gradient of dry static energy corresponding to stratiform cloud cover = 0
    real(p), parameter :: gse_s1 = 0.40  !! Gradient of dry static energy corresponding to
    !! stratiform cloud cover = 1
    real(p), parameter :: albcl = 0.43  !! Cloud albedo (for cloud cover = 1)
    real(p), parameter :: albcls = 0.50  !! Stratiform cloud albedo (for st. cloud cover = 1)
    real(p), parameter :: epssw = 0.020 !! Fraction of incoming solar radiation absorbed by flux_ozone_lower

    ! Shortwave absorptivities (for dp = 10^5 Pa)
    real(p), parameter :: absdry = 0.033 !! Absorptivity of dry air (visible band)
    real(p), parameter :: absaer = 0.033 !! Absorptivity of aerosols (visible band)
    real(p), parameter :: abswv1 = 0.022 !! Absorptivity of water vapour (visible band, for dq = 1 g/kg)
    real(p), parameter :: abswv2 = 15.000 !! Absorptivity of water vapour (near IR band, for dq = 1 g/kg)
    real(p), parameter :: abscl1 = 0.015 !! Absorptivity of clouds (visible band, maximum value)
    real(p), parameter :: abscl2 = 0.15  !! Absorptivity of clouds (visible band, for dq_base = 1 g/kg)

    ! Longwave absorptivities (per dp = 10^5 Pa)
    real(p), parameter :: ablwin = 0.3 !! Absorptivity of air in "window" band

    real(p), parameter :: ablwv1 = 0.7 !! Absorptivity of water vapour in H2O band 1 (weak), (for dq = 1 g/kg)
    real(p), parameter :: ablwv2 = 50.0 !! Absorptivity of water vapour in H2O band 2 (strong), (for dq = 1 g/kg)
    real(p), parameter :: ablcl1 = 12.0 !! Absorptivity of "thick" clouds in window band (below cloud top)
    real(p), parameter :: ablcl2 = 0.6 !! Absorptivity of "thin" upper clouds in window and H2O bands


contains
    !> Compute the absorption of shortwave radiation and initialize arrays
    !  for longwave-radiation routines
    subroutine get_shortwave_rad_fluxes(state, psa, qa, icltop, cloudc, clstr)
        use geometry, only : fsg, dhs
        use mod_radcon
        use model_state, only : ModelState_t

        type(ModelState_t), intent(inout) :: state
        real(p), intent(in) :: psa(ix, il)       !! Normalised surface pressure [p/p0]
        real(p), intent(in) :: qa(ix, il, kx)     !! Specific humidity [g/kg]
        integer, intent(in) :: icltop(ix, il)    !! Cloud top level
        real(p), intent(in) :: cloudc(ix, il)    !! Total cloud cover
        real(p), intent(in) :: clstr(ix, il)     !! Stratiform cloud cover

        integer :: i, j, k, nl1
        real(p) :: acloud(ix, il), psaz(ix, il), abs1, acloud1, deltap, eps1
        real(p) :: fband1, fband2

        nl1 = kx - 1

        fband2 = 0.05
        fband1 = 1.0 - fband2

        ! 1.  Initialization
        state%rad_tau2 = 0.0

        do i = 1, ix
            do j = 1, il
                if (icltop(i, j) <= kx) then
                    state%rad_tau2(i, j, icltop(i, j), 3) = albcl * cloudc(i, j)
                end if
                state%rad_tau2(i, j, kx, 3) = albcls * clstr(i, j)
            end do
        end do

        ! 2. Shortwave transmissivity:
        ! function of layer mass, flux_ozone_lower (in the statosphere),
        ! abs. humidity and cloud cover (in the troposphere)
        psaz = psa * state%zenit_correction
        acloud = cloudc * min(abscl1 * state%qcloud_equiv, abscl2)
        state%rad_tau2(:, :, 1, 1) = exp(-psaz * dhs(1) * absdry)

        do k = 2, nl1
            abs1 = absdry + absaer * fsg(k)**2

            do i = 1, ix
                do j = 1, il
                    if (k >= icltop(i, j)) then
                        state%rad_tau2(i, j, k, 1) = exp(&
                                -psaz(i, j) * dhs(k) * (abs1 + abswv1 * qa(i, j, k) + acloud(i, j)))
                    else
                        state%rad_tau2(i, j, k, 1) = exp(-psaz(i, j) * dhs(k) * (abs1 + abswv1 * qa(i, j, k)))
                    end if
                end do
            end do
        end do

        abs1 = absdry + absaer * fsg(kx)**2
        state%rad_tau2(:, :, kx, 1) = exp(-psaz * dhs(kx) * (abs1 + abswv1 * qa(:, :, kx)))

        do k = 2, kx
            state%rad_tau2(:, :, k, 2) = exp(-psaz * dhs(k) * abswv2 * qa(:, :, k))
        end do

        ! 3. Shortwave downward flux
        ! 3.1 Initialization of fluxes
        state%tsr = state%flux_solar_in
        state%rad_flux(:, :, 1) = state%flux_solar_in * fband1
        state%rad_flux(:, :, 2) = state%flux_solar_in * fband2

        ! 3.2 Ozone and dry-air absorption in the stratosphere
        k = 1
        state%tt_rsw(:, :, k) = state%rad_flux(:, :, 1)

        state%rad_flux(:, :, 1) = state%rad_tau2(:, :, k, 1) * (state%rad_flux(:, :, 1) &
                - state%flux_ozone_upper * psa)

        state%tt_rsw(:, :, k) = state%tt_rsw(:, :, k) - state%rad_flux(:, :, 1)

        k = 2
        state%tt_rsw(:, :, k) = state%rad_flux(:, :, 1)

        state%rad_flux(:, :, 1) = state%rad_tau2(:, :, k, 1) * (state%rad_flux(:, :, 1) &
                - state%flux_ozone_lower * psa)

        state%tt_rsw(:, :, k) = state%tt_rsw(:, :, k) - state%rad_flux(:, :, 1)

        ! 3.3  Absorption and reflection in the troposphere
        do k = 3, kx
            state%rad_tau2(:, :, k, 3) = state%rad_flux(:, :, 1) * state%rad_tau2(:, :, k, 3)
            state%rad_flux (:, :, 1) = state%rad_flux(:, :, 1) - state%rad_tau2(:, :, k, 3)
            state%tt_rsw(:, :, k) = state%rad_flux(:, :, 1)
            state%rad_flux (:, :, 1) = state%rad_tau2(:, :, k, 1) * state%rad_flux(:, :, 1)
            state%tt_rsw(:, :, k) = state%tt_rsw(:, :, k) - state%rad_flux(:, :, 1)
        end do

        do k = 2, kx
            state%tt_rsw(:, :, k) = state%tt_rsw(:, :, k) + state%rad_flux(:, :, 2)
            state%rad_flux(:, :, 2) = state%rad_tau2(:, :, k, 2) * state%rad_flux(:, :, 2)
            state%tt_rsw(:, :, k) = state%tt_rsw(:, :, k) - state%rad_flux(:, :, 2)
        end do

        ! 4. Shortwave upward state%rad_flux
        ! 4.1  Absorption and reflection at the surface
        state%ssrd = state%rad_flux(:, :, 1) + state%rad_flux(:, :, 2)
        state%rad_flux(:, :, 1) = state%rad_flux(:, :, 1) * state%alb_surface
        state%ssr = state%ssrd - state%rad_flux(:, :, 1)

        ! 4.2  Absorption of upward flux
        do k = kx, 1, -1
            state%tt_rsw(:, :, k) = state%tt_rsw(:, :, k) + state%rad_flux(:, :, 1)
            state%rad_flux(:, :, 1) = state%rad_tau2(:, :, k, 1) * state%rad_flux(:, :, 1)
            state%tt_rsw(:, :, k) = state%tt_rsw(:, :, k) - state%rad_flux(:, :, 1)
            state%rad_flux(:, :, 1) = state%rad_flux(:, :, 1) + state%rad_tau2(:, :, k, 3)
        end do

        ! 4.3  Net solar radiation = incoming - outgoing
        state%tsr = state%tsr - state%rad_flux(:, :, 1)

        ! 5.  Initialization of longwave radiation model
        ! 5.1  Longwave transmissivity:
        ! function of layer mass, abs. humidity and cloud cover.

        ! Cloud-free levels (stratosphere + PBL)
        k = 1
        state%rad_tau2(:, :, k, 1) = exp(-psa * dhs(k) * ablwin)
        state%rad_tau2(:, :, k, 2) = exp(-psa * dhs(k) * state%air_absortivity_co2)
        state%rad_tau2(:, :, k, 3) = 1.0
        state%rad_tau2(:, :, k, 4) = 1.0

        do k = 2, kx, kx - 2
            state%rad_tau2(:, :, k, 1) = exp(-psa * dhs(k) * ablwin)
            state%rad_tau2(:, :, k, 2) = exp(-psa * dhs(k) * state%air_absortivity_co2)
            state%rad_tau2(:, :, k, 3) = exp(-psa * dhs(k) * ablwv1 * qa(:, :, k))
            state%rad_tau2(:, :, k, 4) = exp(-psa * dhs(k) * ablwv2 * qa(:, :, k))
        end do

        ! Cloudy layers (free troposphere)
        acloud = cloudc * ablcl2

        do k = 3, nl1
            do i = 1, ix
                do j = 1, il
                    deltap = psa(i, j) * dhs(k)

                    if (k < icltop(i, j)) then
                        acloud1 = acloud(i, j)
                    else
                        acloud1 = ablcl1 * cloudc(i, j)
                    endif

                    state%rad_tau2(i, j, k, 1) = exp(-deltap * (ablwin + acloud1))
                    state%rad_tau2(i, j, k, 2) = exp(-deltap * state%air_absortivity_co2)
                    state%rad_tau2(i, j, k, 3) = exp(-deltap * max(ablwv1 * qa(i, j, k), acloud(i, j)))
                    state%rad_tau2(i, j, k, 4) = exp(-deltap * max(ablwv2 * qa(i, j, k), acloud(i, j)))
                end do
            end do
        end do

        ! 5.2  Stratospheric correction terms
        eps1 = epslw / (dhs(1) + dhs(2))
        state%rad_strat_corr(:, :, 1) = state%stratospheric_correction * psa
        state%rad_strat_corr(:, :, 2) = eps1 * psa
    end subroutine

    !> Compute zonally-averaged fields to be used in the computation of
    !  short-wave absorption
    subroutine get_zonal_average_fields(state, tyear)
        use geometry, only : sia, coa
        use model_state, only : ModelState_t

        type(ModelState_t), intent(inout) :: state

        real(p), intent(in) :: tyear !! time as fraction of year (0-1, 0 = 1jan.h00)

        real(p) :: topsr(il), alpha, azen, coz1, coz2, dalpha, flat2, fs0
        real(p) :: nzen, rzen
        integer :: j

        ! alpha = year phase ( 0 - 2pi, 0 = winter solstice = 22dec.h00 )
        alpha = 4.0 * asin(1.0) * (tyear + 10.0 / 365.0)
        dalpha = 0.0

        coz1 = 1.0 * max(0.0, cos(alpha - dalpha))
        coz2 = 1.8

        azen = 1.0
        nzen = 2

        rzen = -cos(alpha) * 23.45 * asin(1.0) / 90.0

        fs0 = 6.0

        ! Solar radiation at the top
        call solar(tyear, 4.0 * solc, topsr)

        do j = 1, il
            flat2 = 1.5 * sia(j)**2 - 0.5

            ! Solar radiation at the top
            state%flux_solar_in(:, j) = topsr(j)

            ! Ozone depth in upper and lower stratosphere
            state%flux_ozone_upper(:, j) = 0.5 * epssw
            state%flux_ozone_lower(:, j) = 0.4 * epssw * (1.0 + coz1 * sia(j) + coz2 * flat2)

            ! Optical depth ratio (function of solar zenith angle)
            ! Zenith angle correction to (downward) absorptivity
            state%zenit_correction(:, j) = 1.0 + azen * (1.0 - (coa(j) * cos(rzen) + sia(j) * sin(rzen)))**nzen

            ! Ozone absorption in upper and lower stratosphere
            state%flux_ozone_upper(:, j) = &
                    state%flux_solar_in(:, j) * state%flux_ozone_upper(:, j) * state%zenit_correction(:, j)
            state%flux_ozone_lower(:, j) = &
                    state%flux_solar_in(:, j) * state%flux_ozone_lower(:, j) * state%zenit_correction(:, j)

            ! Polar night cooling in the stratosphere
            state%stratospheric_correction(:, j) = max(fs0 - state%flux_solar_in(:, j), 0.0)
        end do
    end

    ! Average daily flux of solar radiation, from Hartmann (1994)
    subroutine solar(tyear, csol, topsr)
        use geometry, only : coa, sia

        real(p), intent(in) :: tyear     !! time as fraction of year (0-1, 0 = 1jan.h00)
        real(p), intent(in) :: csol       !! The solar constant [W/m^2]
        real(p), intent(out) :: topsr(il) !! Daily-average insolation at the top of the atmosphere
        !! as a function of latitude

        integer :: j
        real(p) :: ca1, ca2, ca3, cdecl, ch0, csolp, decl, fdis, h0, alpha, pigr, sa1
        real(p) :: sa2, sa3, sdecl, sh0, tdecl

        ! 1. Compute declination angle and Earth-Sun distance factor
        pigr = 2.0 * asin(1.0)
        alpha = 2.0 * pigr * tyear

        ca1 = cos(alpha)
        sa1 = sin(alpha)
        ca2 = ca1 * ca1 - sa1 * sa1
        sa2 = 2. * sa1 * ca1
        ca3 = ca1 * ca2 - sa1 * sa2
        sa3 = sa1 * ca2 + sa2 * ca1

        decl = 0.006918 - 0.399912 * ca1 + 0.070257 * sa1 - 0.006758 * ca2 + 0.000907 * sa2&
                & - 0.002697 * ca3 + 0.001480 * sa3

        fdis = 1.000110 + 0.034221 * ca1 + 0.001280 * sa1 + 0.000719 * ca2 + 0.000077 * sa2

        cdecl = cos(decl)
        sdecl = sin(decl)
        tdecl = sdecl / cdecl

        ! 2. Compute daily-average insolation at the atm. top
        csolp = csol / pigr

        do j = 1, il
            ch0 = min(1.0, max(-1.0, -tdecl * sia(j) / coa(j)))
            h0 = acos(ch0)
            sh0 = sin(h0)

            topsr(j) = csolp * fdis * (h0 * sia(j) * sdecl + sh0 * coa(j) * cdecl)
        end do
    end

    !>  Compute cloud-top level and cloud cover
    subroutine clouds(qa, rh, precnv, precls, iptop, gse, fmask, icltop, cloudc, clstr, qcloud_equiv)
        integer :: iptop(ix, il)
        real(p), intent(in) :: qa(ix, il, kx)   !! Specific humidity [g/kg]
        real(p), intent(in) :: rh(ix, il, kx)   !! Relative humidity
        real(p), intent(in) :: precnv(ix, il)  !! Convection precipitation
        real(p), intent(in) :: precls(ix, il)  !! Large-scale condensational precipitation
        real(p), intent(in) :: gse(ix, il)     !! Vertical gradient of dry static energy
        real(p), intent(in) :: fmask(ix, il)   !! Fraction land-sea mask
        integer, intent(out) :: icltop(ix, il) !! Cloud top level
        real(p), intent(out) :: cloudc(ix, il) !! Total cloud cover
        real(p), intent(out) :: clstr(ix, il)  !! Stratiform cloud cover
        real(p), intent(out) :: qcloud_equiv(ix, il)

        integer :: i, j, k, nl1, nlp
        real(p) :: clfact, clstrl, drh, fstab, pr1, rgse, rrcl

        nl1 = kx - 1
        nlp = kx + 1
        rrcl = 1. / (rhcl2 - rhcl1)

        ! 1.  Cloud cover, defined as the sum of:
        !     - a term proportional to the square-root of precip. rate
        !     - a quadratic function of the max. relative humidity
        !       in tropospheric layers above PBL where Q > QACL :
        !       ( = 0 for RHmax < RHCL1, = 1 for RHmax > RHCL2 )
        !     Cloud-top level: defined as the highest (i.e. least sigma)
        !       between the top of convection/condensation and
        !       the level of maximum relative humidity.

        do i = 1, ix
            do j = 1, il
                if (rh(i, j, nl1) > rhcl1) then
                    cloudc(i, j) = rh(i, j, nl1) - rhcl1
                    icltop(i, j) = nl1
                else
                    cloudc(i, j) = 0.0
                    icltop(i, j) = nlp
                end if
            end do
        end do

        do k = 3, kx - 2
            do i = 1, ix
                do j = 1, il
                    drh = rh(i, j, k) - rhcl1
                    if (drh > cloudc(i, j) .and. qa(i, j, k) > qacl) then
                        cloudc(i, j) = drh
                        icltop(i, j) = k
                    end if
                end do
            end do
        end do

        do i = 1, ix
            do j = 1, il
                pr1 = min(pmaxcl, 86.4 * (precnv(i, j) + precls(i, j)))
                cloudc(i, j) = min(1.0, wpcl * sqrt(pr1) + min(1.0, cloudc(i, j) * rrcl)**2.0)
                icltop(i, j) = min(iptop(i, j), icltop(i, j))
            end do
        end do

        ! 2.  Equivalent specific humidity of clouds
        qcloud_equiv = qa(:, :, nl1)

        ! 3. Stratiform clouds at the top of PBL
        clfact = 1.2
        rgse = 1.0 / (gse_s1 - gse_s0)

        do i = 1, ix
            do j = 1, il
                ! Stratocumulus clouds over sea
                fstab = max(0.0, min(1.0, rgse * (gse(i, j) - gse_s0)))
                clstr(i, j) = fstab * max(clsmax - clfact * cloudc(i, j), 0.0)

                ! Stratocumulus clouds over land
                clstrl = max(clstr(i, j), clsminl) * rh(i, j, kx)
                clstr(i, j) = clstr(i, j) + fmask(i, j) * (clstrl - clstr(i, j))
            end do
        end do
    end
end module
