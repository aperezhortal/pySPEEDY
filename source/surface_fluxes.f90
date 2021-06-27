!> Parametrization of surface fluxes
module surface_fluxes
    use types, only: p
    use params

    implicit none

    private
    public get_surface_fluxes, set_orog_land_sfc_drag

    !  Constants for surface fluxes
    real(p) :: fwind0 = 0.95 !! Ratio of near-sfc wind to lowest-level wind

    !> Weight for near-sfc temperature extrapolation (0-1) :
    !  1 : linear extrapolation from two lowest levels
    !  0 : constant potential temperature ( = lowest level)
    real(p) :: ftemp0 = 1.0

    !> Weight for near-sfc specific humidity extrapolation (0-1) :
    !  1 : extrap. with constant relative hum. ( = lowest level)
    !  0 : constant specific hum. ( = lowest level)
    real(p) :: fhum0 = 0.0

    real(p) :: cdl = 2.4e-3   !! Drag coefficient for momentum over land
    real(p) :: cds = 1.0e-3   !! Drag coefficient for momentum over sea
    real(p) :: chl = 1.2e-3   !! Heat exchange coefficient over land
    real(p) :: chs = 0.9e-3   !! Heat exchange coefficient over sea
    real(p) :: vgust = 5.0    !! Wind speed for sub-grid-scale gusts
    real(p) :: ctday = 1.0e-2 !! Daily-cycle correction (dTskin/dSSRad)
    real(p) :: dtheta = 3.0   !! Potential temp. gradient for stability correction
    real(p) :: fstab = 0.67   !! Amplitude of stability correction (fraction)
    real(p) :: hdrag = 2000.0 !! Height scale for orographic correction
    real(p) :: clambda = 7.0  !! Heat conductivity in skin-to-root soil layer
    real(p) :: clambsn = 7.0  !! Heat conductivity in soil for snow cover = 1

    real(p) :: forog(ix, il) ! Time-invariant fields (initial. in SFLSET)

contains
    !> Compute surface fluxes of momentum, energy and moisture, and define surface
    !  skin temperature from energy balance
    subroutine get_surface_fluxes(state, &
                                  psa, ua, va, ta, qa, rh, phi, fmask, tsea, &
                                  tsfc, tskin, u0, v0, t0, lfluxland)                    
        use physical_constants, only: p0, rgas, cp, alhc, sbc, sigl, wvi
        use mod_radcon, only: emisfc, alb_l, alb_s, snowc
        use land_model, only: stl_am, soilw_am
        use humidity, only: get_qsat, rel_hum_to_spec_hum
        use model_state, only: ModelState_t

        type(ModelState_t), intent(inout) :: state

        real(p), intent(in) :: psa(ix, il)    !! Normalised surface pressure
        real(p), intent(in) :: ua(ix, il, kx)  !! u-wind
        real(p), intent(in) :: va(ix, il, kx)  !! v-wind
        real(p), intent(in) :: ta(ix, il, kx)  !! Temperature
        real(p), intent(in) :: qa(ix, il, kx)  !! Specific humidity [g/kg]
        real(p), intent(in) :: rh(ix, il, kx)  !! Relative humidity
        real(p), intent(in) :: phi(ix, il, kx) !! Geopotential
        real(p), intent(in) :: fmask(ix, il)  !! Fractional land-sea mask
        real(p), intent(in) :: tsea(ix, il)   !! Sea-surface temperature
        logical, intent(in) :: lfluxland
        real(p), intent(out) :: tsfc(ix, il)   !! Surface temperature
        real(p), intent(out) :: tskin(ix, il)  !! Skin surface temperature
        real(p), intent(out) :: u0(ix, il) !! Near-surface u-wind
        real(p), intent(out) :: v0(ix, il) !! Near-surface v-wind
        real(p), intent(out) :: t0(ix, il) !! Near-surface temperature

        integer :: i, j, ks, nl1
        real(p), dimension(ix, il, 2), save :: t1, q1
        real(p), dimension(ix, il, 2) :: t2, qsat0
        real(p), save :: denvvs(ix, il, 0:2)
        real(p) :: dslr(ix, il), dtskin(ix, il), clamb(ix, il), astab, cdldv, cdsdv(ix, il), chlcp
        real(p) :: dt1, dthl, dths, esbc, ghum0, gtemp0
        real(p) :: rcp, rdth, tsk3(ix, il)

        logical lscasym, lskineb

        lscasym = .true.   ! true : use an asymmetric stability coefficient
        lskineb = .true.   ! true : redefine skin temp. from energy balance

        esbc = emisfc*sbc

        ghum0 = 1.0 - fhum0

        ! =========================================================================
        ! Land surface
        ! =========================================================================

        if (lfluxland) then
            ! 1. Extrapolation of wind, temp, hum. and density to the surface

            ! 1.1 Wind components
            u0 = fwind0*ua(:, :, kx)
            v0 = fwind0*va(:, :, kx)

            ! 1.2 Temperature
            gtemp0 = 1.0 - ftemp0
            rcp = 1.0/cp
            nl1 = kx - 1

            do i = 1, ix
                do j = 1, il
                    ! Temperature difference between lowest level and sfc
                    dt1 = wvi(kx, 2)*(ta(i, j, kx) - ta(i, j, nl1))

                    ! Extrapolated temperature using actual lapse rate (1:land, 2:sea)
                    t1(i, j, 1) = ta(i, j, kx) + dt1
                    t1(i, j, 2) = t1(i, j, 1) - state%phi0(i, j)*dt1/(rgas*288.0*sigl(kx))

                    ! Extrapolated temperature using dry-adiab. lapse rate (1:land, 2:sea)
                    t2(i, j, 2) = ta(i, j, kx) + rcp*phi(i, j, kx)
                    t2(i, j, 1) = t2(i, j, 2) - rcp*state%phi0(i, j)
                end do
            end do

            do i = 1, ix
                do j = 1, il
                    if (ta(i, j, kx) > ta(i, j, nl1)) then
                        ! Use extrapolated temp. if dT/dz < 0
                        t1(i, j, 1) = ftemp0*t1(i, j, 1) + gtemp0*t2(i, j, 1)
                        t1(i, j, 2) = ftemp0*t1(i, j, 2) + gtemp0*t2(i, j, 2)
                    else
                        ! Use temp. at lowest level if dT/dz > 0
                        t1(i, j, 1) = ta(i, j, kx)
                        t1(i, j, 2) = ta(i, j, kx)
                    end if
                    t0(i, j) = t1(i, j, 2) + fmask(i, j)*(t1(i, j, 1) - t1(i, j, 2))
                end do
            end do

            ! 1.3 Density * wind speed (including gustiness factor)
            denvvs(:, :, 0) = (p0*psa/(rgas*t0))*sqrt(u0**2.0 + v0**2.0 + vgust**2.0)

            ! 2. Compute land-sfc. fluxes using prescribed skin temperature

            ! 2.1 Define effective skin temperature to compensate for
            !     non-linearity of heat/moisture fluxes during the daily cycle
            do j = 1, il
                tskin(:, j) = stl_am(:, j) + ctday*sqrt(state%coa(j))*state%ssrd(:, j)*(1.0 - alb_l(:, j))*psa(:, j)
            end do

            ! 2.2 Stability correction = f[pot.temp.(sfc)-pot.temp.(air)]
            rdth = fstab/dtheta
            astab = 1.0
            if (lscasym) astab = 0.5   ! to get smaller ds/dt in stable conditions

            do i = 1, ix
                do j = 1, il
                    ! Potential temp. difference (land+sea average)
                    if (tskin(i, j) > t2(i, j, 1)) then
                        dthl = min(dtheta, tskin(i, j) - t2(i, j, 1))
                    else
                        dthl = max(-dtheta, astab*(tskin(i, j) - t2(i, j, 1)))
                    end if
                    denvvs(i, j, 1) = denvvs(i, j, 0)*(1.0 + dthl*rdth)
                end do
            end do

            ! 2.3 Wind stress
            do i = 1, ix
                do j = 1, il
                    cdldv = cdl*denvvs(i, j, 0)*forog(i, j)
                    state%ustr(i, j, 1) = -cdldv*ua(i, j, kx)
                    state%vstr(i, j, 1) = -cdldv*va(i, j, kx)
                end do
            end do

            ! 2.4 Sensible heat flux
            chlcp = chl*cp
            state%shf(:, :, 1) = chlcp*denvvs(:, :, 1)*(tskin - t1(:, :, 1))

            ! 2.5 Evaporation
            if (fhum0 > 0.0) then
                call rel_hum_to_spec_hum(t1, psa, 1.0_p, rh(:, :, kx), q1, qsat0(:, :, 1))

                q1(:, :, 1) = fhum0*q1(:, :, 1) + ghum0*qa(:, :, kx)
            else
                q1(:, :, 1) = qa(:, :, kx)
            end if

            qsat0(:, :, 1) = get_qsat(tskin, psa, 1.0_p)
            state%evap(:, :, 1) = chl*denvvs(:, :, 1)*max(0.0, soilw_am*qsat0(:, :, 1) - q1(:, :, 1))

            ! 3. Compute land-surface energy balance;
            !    adjust skin temperature and heat fluxes

            ! 3.1. Emission of lw radiation from the surface
            !      and net heat fluxes into land surface
            tsk3 = tskin**3.0
            dslr = 4.0*esbc*tsk3
            state%slru(:, :, 1) = esbc*tsk3*tskin
            state%hfluxn(:, :, 1) = state%ssrd*(1.0 - alb_l) &
                                    + state%slrd - (state%slru(:, :, 1) &
                                                    + state%shf(:, :, 1) + alhc*state%evap(:, :, 1))

            ! 3.2 Re-definition of skin temperature from energy balance
            if (lskineb) then
                ! Compute net heat flux including flux into ground
                clamb = clambda + snowc*(clambsn - clambda)
                state%hfluxn(:, :, 1) = state%hfluxn(:, :, 1) - clamb*(tskin - stl_am)
                dtskin = tskin + 1.0

                ! Compute d(Evap) for a 1-degree increment of Tskin
                qsat0(:, :, 2) = get_qsat(dtskin, psa, 1.0_p)

                do i = 1, ix
                    do j = 1, il
                        if (state%evap(i, j, 1) > 0.0) then
                            qsat0(i, j, 2) = soilw_am(i, j)*(qsat0(i, j, 2) - qsat0(i, j, 1))
                        else
                            qsat0(i, j, 2) = 0.0
                        end if
                    end do
                end do

                ! Redefine skin temperature to balance the heat budget
                dtskin = state%hfluxn(:, :, 1)/(clamb + dslr + chl*denvvs(:, :, 1)*(cp + alhc*qsat0(:, :, 2)))
                tskin = tskin + dtskin

                ! Add linear corrections to heat fluxes
                state%shf(:, :, 1) = state%shf(:, :, 1) + chlcp*denvvs(:, :, 1)*dtskin
                state%evap(:, :, 1) = state%evap(:, :, 1) + chl*denvvs(:, :, 1)*qsat0(:, :, 2)*dtskin
                state%slru(:, :, 1) = state%slru(:, :, 1) + dslr*dtskin
                state%hfluxn(:, :, 1) = clamb*(tskin - stl_am)
            end if

            rdth = fstab/dtheta
            astab = 1.0
            if (lscasym) astab = 0.5   ! to get smaller dS/dT in stable conditions

            do i = 1, ix
                do j = 1, il
                    if (tsea(i, j) > t2(i, j, 2)) then
                        dths = min(dtheta, tsea(i, j) - t2(i, j, 2))
                    else
                        dths = max(-dtheta, astab*(tsea(i, j) - t2(i, j, 2)))
                    end if
                    denvvs(i, j, 2) = denvvs(i, j, 0)*(1.0 + dths*rdth)
                end do
            end do

            if (fhum0 > 0.0) then
                call rel_hum_to_spec_hum(t1(:, :, 2), psa, 1.0_p, rh(:, :, kx), q1(:, :, 2), qsat0(:, :, 2))

                q1(:, :, 2) = fhum0*q1(:, :, 2) + ghum0*qa(:, :, kx)
            else
                q1(:, :, 2) = qa(:, :, kx)
            end if

            ! 4.2 Wind stress
            ks = 2

            cdsdv = cds*denvvs(:, :, ks)
            state%ustr(:, :, 2) = -cdsdv*ua(:, :, kx)
            state%vstr(:, :, 2) = -cdsdv*va(:, :, kx)
        end if

        ! =========================================================================
        ! Sea surface
        ! =========================================================================

        ! 4.3 Sensible heat flux
        state%shf(:, :, 2) = chs*cp*denvvs(:, :, ks)*(tsea - t1(:, :, 2))

        ! 4.4 Evaporation
        qsat0(:, :, 2) = get_qsat(tsea, psa, 1.0_p)
        state%evap(:, :, 2) = chs*denvvs(:, :, ks)*(qsat0(:, :, 2) - q1(:, :, 2))

        ! 4.5 Emission of lw radiation from the surface
        !     and net heat fluxes into sea surface
        state%slru(:, :, 2) = esbc*tsea**4.0
        state%hfluxn(:, :, 2) = state%ssrd*(1.0 - alb_s) &
                                + state%slrd - state%slru(:, :, 2) &
                                + state%shf(:, :, 2) + alhc*state%evap(:, :, 2)

        ! =========================================================================
        ! Weighted average of surface fluxes and temperatures according to land-sea
        ! mask
        ! =========================================================================

        if (lfluxland) then
            state%ustr(:, :, 3) = state%ustr(:, :, 2) + fmask*(state%ustr(:, :, 1) - state%ustr(:, :, 2))
            state%vstr(:, :, 3) = state%vstr(:, :, 2) + fmask*(state%vstr(:, :, 1) - state%vstr(:, :, 2))
            state%shf(:, :, 3) = state%shf(:, :, 2) + fmask*(state%shf(:, :, 1) - state%shf(:, :, 2))
            state%evap(:, :, 3) = state%evap(:, :, 2) + fmask*(state%evap(:, :, 1) - state%evap(:, :, 2))
            state%slru(:, :, 3) = state%slru(:, :, 2) + fmask*(state%slru(:, :, 1) - state%slru(:, :, 2))

            tsfc = tsea + fmask*(stl_am - tsea)
            tskin = tsea + fmask*(tskin - tsea)
            t0 = t1(:, :, 2) + fmask*(t1(:, :, 1) - t1(:, :, 2))
        end if
    end

    ! Compute orographic factor for land surface drag
    ! Input:   phi0 = surface geopotential
    subroutine set_orog_land_sfc_drag(phi0)
        use physical_constants, only: grav

        real(p), intent(in) :: phi0(ix, il)
        real(p) :: rhdrag

        rhdrag = 1.0/(grav*hdrag)

        forog = 1.0 + rhdrag*(1.0 - exp(-max(phi0, 0.0)*rhdrag))
    end
end module
