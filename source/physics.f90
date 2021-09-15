module physics
    use types, only : p
    use params

    implicit none

    private
    public get_physical_tendencies

contains

    !> Compute physical parametrization tendencies for u, v, t, q and add them
    !  to the dynamical grid-point tendencies
    subroutine get_physical_tendencies(state, j1, utend, vtend, ttend, qtend)
        use physical_constants, only : cp
        use geometry, only : fsg, sigh, grdsig, grdscp
        use sea_model, only : sea_coupling_flag
        use sppt, only : mu, gen_sppt
        use convection, only : get_convection_tendencies
        use large_scale_condensation, only : get_large_scale_condensation_tendencies
        use shortwave_radiation, only : get_shortwave_rad_fluxes, clouds
        use longwave_radiation, only : &
                get_downward_longwave_rad_fluxes, get_upward_longwave_rad_fluxes
        use surface_fluxes, only : get_surface_fluxes
        use vertical_diffusion, only : get_vertical_diffusion_tend
        use humidity, only : spec_hum_to_rel_hum
        use model_state, only : ModelState_t

        type(ModelState_t), intent(inout) :: state
        integer, intent(in) :: j1

        real(p), intent(inout) :: utend(ix, il, kx) !! Zonal velocity tendency
        real(p), intent(inout) :: vtend(ix, il, kx) !! Meridional velocity tendency
        real(p), intent(inout) :: ttend(ix, il, kx) !! Temperature tendency
        real(p), intent(inout) :: qtend(ix, il, kx) !! Specific humidity tendency

        real(p), allocatable, dimension(:, :, :) :: tt_rlw
        real(p), allocatable, dimension(:, :, :) :: ut_pbl, vt_pbl
        real(p), allocatable, dimension(:, :, :) :: tt_pbl, qt_pbl

        integer :: i, j, k

        complex(p), allocatable, dimension(:, :) :: ucos, vcos
        real(p), allocatable, dimension(:, :) :: pslg, rps, gse
        real(p), allocatable, dimension(:, :) :: psg, ts, tskin, u0, v0, t0
        real(p), allocatable, dimension(:, :) :: cloudc, clstr, cltop, prtop
        integer, allocatable, dimension(:, :) :: iptop, icnv
        integer, allocatable :: icltop(:, :, :)

        real(p), allocatable, dimension(:, :, :) :: ug, vg, tg, qg, phig
        real(p), allocatable, dimension(:, :, :) :: utend_dyn, vtend_dyn, ttend_dyn, qtend_dyn
        real(p), allocatable, dimension(:, :, :) :: se, rh, qsat
        real(p), allocatable, dimension(:, :, :) :: tt_cnv, qt_cnv, tt_lsc, qt_lsc
        real(p), allocatable, dimension(:, :, :) :: sppt_pattern

        allocate(tt_cnv(ix, il, kx), qt_cnv(ix, il, kx), tt_lsc(ix, il, kx), qt_lsc(ix, il, kx))
        allocate(tt_rlw(ix, il, kx), ut_pbl(ix, il, kx), vt_pbl(ix, il, kx), tt_pbl(ix, il, kx))
        allocate(qt_pbl(ix, il, kx), sppt_pattern(ix, il, kx))
        allocate(ug(ix, il, kx), vg(ix, il, kx), tg(ix, il, kx), qg(ix, il, kx))
        allocate(phig(ix, il, kx), utend_dyn(ix, il, kx))
        allocate(vtend_dyn(ix, il, kx), ttend_dyn(ix, il, kx))
        allocate(qtend_dyn(ix, il, kx), se(ix, il, kx), rh(ix, il, kx), qsat(ix, il, kx))

        allocate(ucos(mx, nx), vcos(mx, nx))

        allocate(pslg(ix, il), rps(ix, il), gse(ix, il), psg(ix, il))
        allocate(ts(ix, il), tskin(ix, il), u0(ix, il), v0(ix, il))
        allocate(t0(ix, il), cloudc(ix, il), clstr(ix, il), cltop(ix, il))
        allocate(prtop(ix, il), iptop(ix, il), icnv(ix, il))

        allocate(icltop(ix, il, 2))

        ! Keep a copy of the original (dynamics only) tendencies
        utend_dyn = utend
        vtend_dyn = vtend
        ttend_dyn = ttend
        qtend_dyn = qtend

        ! =========================================================================
        ! Compute grid-point fields
        ! =========================================================================
        ! Convert model spectral variables to grid-point variables
        do k = 1, kx
            call state%mod_spectral%vort2vel(&
                    state%vor(:, :, k, j1), state%div(:, :, k, j1), ucos, vcos)

            ug(:, :, k) = state%mod_spectral%spec2grid(ucos, 2)
            vg(:, :, k) = state%mod_spectral%spec2grid(vcos, 2)
            tg(:, :, k) = state%mod_spectral%spec2grid(state%t(:, :, k, j1), 1)
            qg(:, :, k) = state%mod_spectral%spec2grid(state%tr(:, :, k, j1, 1), 1) ! q
            phig(:, :, k) = state%mod_spectral%spec2grid(state%phi(:, :, k), 1)

        end do

        pslg = state%mod_spectral%spec2grid(state%ps(:, :, j1), 1)

        ! =========================================================================
        ! Compute thermodynamic variables
        ! =========================================================================

        psg = exp(pslg)
        rps = 1.0 / psg

        qg = max(qg, 0.0)
        se = cp * tg + phig

        do k = 1, kx
            call spec_hum_to_rel_hum(tg(:, :, k), psg, fsg(k), qg(:, :, k), &
                    rh(:, :, k), qsat(:, :, k))
        end do

        ! =========================================================================
        ! Precipitation
        ! =========================================================================

        ! Deep convection
        call get_convection_tendencies(psg, se, qg, qsat, iptop, state%cbmf, &
                state%precnv, tt_cnv, qt_cnv)

        do k = 2, kx
            tt_cnv(:, :, k) = tt_cnv(:, :, k) * rps * grdscp(k)
            qt_cnv(:, :, k) = qt_cnv(:, :, k) * rps * grdsig(k)
        end do

        icnv = kx - iptop

        ! Large-scale condensation
        call get_large_scale_condensation_tendencies(psg, qg, qsat, iptop, &
                state%precls, tt_lsc, qt_lsc)

        ttend = ttend + tt_cnv + tt_lsc
        qtend = qtend + qt_cnv + qt_lsc

        ! =========================================================================
        ! Radiation (shortwave and longwave) and surface fluxes
        ! =========================================================================

        ! Since the shortwave tendencies may not computed at each time time state,
        ! the previous states are saved in the state%tt_rsw variable
        ! (Flux of short-wave radiation absorbed in each atmospheric layer).

        ! Compute shortwave tendencies and initialize lw transmissivity
        ! The shortwave radiation may be called at selected time steps
        if (state%compute_shortwave) then
            gse = (se(:, :, kx - 1) - se(:, :, kx)) / (phig(:, :, kx - 1) - phig(:, :, kx))

            call clouds(qg, rh, state%precnv, state%precls, iptop, gse, &
                    state%fmask_land, icltop, cloudc, clstr, state%qcloud_equiv)

            do i = 1, ix
                do j = 1, il
                    cltop(i, j) = sigh(icltop(i, j, 1) - 1) * psg(i, j)
                    prtop(i, j) = float(iptop(i, j))
                end do
            end do

            call get_shortwave_rad_fluxes(state, psg, qg, icltop, cloudc, clstr)

            do k = 1, kx
                state%tt_rsw(:, :, k) = state%tt_rsw(:, :, k) * rps * grdscp(k)
            end do
        end if

        ! Compute downward longwave fluxes
        call get_downward_longwave_rad_fluxes(&
                tg, state%slrd, tt_rlw, state%fband, state%rad_flux, &
                state%rad_tau2, state%rad_st4a)

        ! Compute surface fluxes and land skin temperature
        call get_surface_fluxes(&
                psg, ug, vg, tg, qg, rh, phig, &
                state%phis0, state%fmask_land, state%forog, state%sst_am, &
                & state%ssrd, state%slrd, state%ustr, state%vstr, &
                state%shf, state%evap, state%slru, state%hfluxn, &
                ts, tskin, u0, v0, t0, .true., &
                state%alb_land, state%alb_sea, state%snowc,&
                state%land_temp, state%soil_avail_water)

        ! Recompute sea fluxes in case of anomaly coupling
        if (sea_coupling_flag > 0) then
            call get_surface_fluxes(&
                    psg, ug, vg, tg, qg, rh, phig, state%phis0, state%fmask_land, state%forog, &
                    state%ssti_om, state%ssrd, state%slrd, &
                    state%ustr, state%vstr, state%shf, &
                    state%evap, state%slru, &
                    state%hfluxn, ts, tskin, u0, v0, t0, .false., &
                    state%alb_land, state%alb_sea, state%snowc,&
                    state%land_temp, state%soil_avail_water)
        end if

        ! Compute upward longwave fluxes, convert them to tendencies and add
        ! shortwave tendencies
        call get_upward_longwave_rad_fluxes(tg, ts, state%slrd, &
                state%slru(:, :, 3), state%slr, &
                state%olr, tt_rlw, state%fband, &
                state%rad_flux, state%rad_tau2, state%rad_st4a, state%rad_strat_corr)
        do k = 1, kx
            tt_rlw(:, :, k) = tt_rlw(:, :, k) * rps * grdscp(k)
        end do

        ttend = ttend + state%tt_rsw + tt_rlw

        ! =========================================================================
        ! Planetary boundary later interactions with lower troposphere
        ! =========================================================================

        ! Vertical diffusion and shallow convection
        call get_vertical_diffusion_tend(se, rh, qg, qsat, phig, icnv, ut_pbl, vt_pbl, &
                & tt_pbl, qt_pbl)

        ! Add tendencies due to surface fluxes
        ut_pbl(:, :, kx) = ut_pbl(:, :, kx) + state%ustr(:, :, 3) * rps * grdsig(kx)
        vt_pbl(:, :, kx) = vt_pbl(:, :, kx) + state%vstr(:, :, 3) * rps * grdsig(kx)
        tt_pbl(:, :, kx) = tt_pbl(:, :, kx) + state%shf(:, :, 3) * rps * grdscp(kx)
        qt_pbl(:, :, kx) = qt_pbl(:, :, kx) + state%evap(:, :, 3) * rps * grdsig(kx)

        utend = utend + ut_pbl
        vtend = vtend + vt_pbl
        ttend = ttend + tt_pbl
        qtend = qtend + qt_pbl

        ! Add SPPT noise
        if (sppt_on) then
            sppt_pattern = gen_sppt(state%mod_spectral)

            ! The physical contribution to the tendency is *tend - *tend_dyn, where * is u, v, t, q
            do k = 1, kx
                utend(:, :, k) = (1 + sppt_pattern(:, :, k) * mu(k)) * (utend(:, :, k) - utend_dyn(:, :, k)) &
                        + utend_dyn(:, :, k)
                vtend(:, :, k) = (1 + sppt_pattern(:, :, k) * mu(k)) * (vtend(:, :, k) - vtend_dyn(:, :, k)) &
                        + vtend_dyn(:, :, k)
                ttend(:, :, k) = (1 + sppt_pattern(:, :, k) * mu(k)) * (ttend(:, :, k) - ttend_dyn(:, :, k)) &
                        + ttend_dyn(:, :, k)
                qtend(:, :, k) = (1 + sppt_pattern(:, :, k) * mu(k)) * (qtend(:, :, k) - qtend_dyn(:, :, k)) &
                        + qtend_dyn(:, :, k)
            end do
        end if

        deallocate(ucos, vcos, pslg, rps, gse)
        deallocate(psg, ts, tskin, u0, v0, t0, cloudc, clstr, cltop, prtop)
        deallocate(iptop, icnv, icltop, ug, vg, tg, qg, phig, utend_dyn, vtend_dyn)
        deallocate(ttend_dyn, qtend_dyn, se, rh, qsat, sppt_pattern)
        deallocate(tt_cnv, qt_cnv, tt_lsc, qt_lsc)
        deallocate(tt_rlw, ut_pbl, vt_pbl, tt_pbl, qt_pbl)
    end
end module
