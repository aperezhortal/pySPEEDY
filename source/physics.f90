module physics
    use types, only: p
    use params

    implicit none

    private
    public initialize_physics, get_physical_tendencies

contains
    ! Initialize physical parametrization routines
    subroutine initialize_physics(state)
        use physical_constants, only: grav, cp, p0, sigl, sigh, grdsig, grdscp, wvi
        use model_state, only: ModelState_t

        type(ModelState_t), intent(inout) :: state

        integer :: k

        ! 1.2 Functions of sigma and latitude
        sigh(0) = state%hsg(1)

        do k = 1, kx
            sigl(k) = log(state%fsg(k))
            sigh(k) = state%hsg(k + 1)
            grdsig(k) = grav/(state%dhs(k)*p0)
            grdscp(k) = grdsig(k)/cp
        end do

        ! Weights for vertical interpolation at half-levels(1,kx) and surface
        ! Note that for phys.par. half-lev(k) is between full-lev k and k+1
        ! Fhalf(k) = Ffull(k)+WVI(K,2)*(Ffull(k+1)-Ffull(k))
        ! Fsurf = Ffull(kx)+WVI(kx,2)*(Ffull(kx)-Ffull(kx-1))
        do k = 1, kx - 1
            wvi(k, 1) = 1./(sigl(k + 1) - sigl(k))
            wvi(k, 2) = (log(sigh(k)) - sigl(k))*wvi(k, 1)
        end do

        wvi(kx, 1) = 0.
        wvi(kx, 2) = (log(0.99) - sigl(kx))*wvi(kx - 1, 1)
    end

    !> Compute physical parametrization tendencies for u, v, t, q and add them
    !  to the dynamical grid-point tendencies
    subroutine get_physical_tendencies(state, j1, utend, vtend, ttend, qtend)
        use physical_constants, only: sigh, grdsig, grdscp, cp
        use land_model, only: fmask_l
        use sea_model, only: sst_am, ssti_om, sea_coupling_flag
        use sppt, only: mu, gen_sppt
        use convection, only: get_convection_tendencies
        use large_scale_condensation, only: get_large_scale_condensation_tendencies
        use shortwave_radiation, only: get_shortwave_rad_fluxes, clouds, compute_shortwave
        use longwave_radiation, only: &
            get_downward_longwave_rad_fluxes, get_upward_longwave_rad_fluxes
        use surface_fluxes, only: get_surface_fluxes
        use vertical_diffusion, only: get_vertical_diffusion_tend
        use humidity, only: spec_hum_to_rel_hum
        use spectral, only: spec_to_grid, uvspec
        use model_state, only: ModelState_t

        type(ModelState_t), intent(inout) :: state
        integer, intent(in) :: j1

        real(p), intent(inout) :: utend(ix, il, kx) !! Zonal velocity tendency
        real(p), intent(inout) :: vtend(ix, il, kx) !! Meridional velocity tendency
        real(p), intent(inout) :: ttend(ix, il, kx) !! Temperature tendency
        real(p), intent(inout) :: qtend(ix, il, kx) !! Specific humidity tendency

        !TODO: Make these variables allocatables
        complex(p), dimension(mx, nx) :: ucos, vcos
        real(p), dimension(ix, il) :: pslg, rps, gse
        real(p), dimension(ix, il, kx) :: ug, vg, tg, qg, phig, utend_dyn, vtend_dyn, ttend_dyn, qtend_dyn
        real(p), dimension(ix, il, kx) :: se, rh, qsat
        real(p), dimension(ix, il) :: psg, ts, tskin, u0, v0, t0, cloudc, clstr, cltop, prtop
        real(p), dimension(ix, il, kx) :: tt_cnv, qt_cnv, tt_lsc, qt_lsc, tt_rsw, tt_rlw, ut_pbl, vt_pbl,&
            & tt_pbl, qt_pbl
        integer :: iptop(ix, il), icltop(ix, il, 2), icnv(ix, il), i, j, k
        real(p) :: sppt_pattern(ix, il, kx)

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
            call uvspec(state%vor(:, :, k, j1), state%div(:, :, k, j1), ucos, vcos)
            ug(:, :, k) = spec_to_grid(ucos, 2, state%cosgr)
            vg(:, :, k) = spec_to_grid(vcos, 2, state%cosgr)
            tg(:, :, k) = spec_to_grid(state%t(:, :, k, j1), 1, state%cosgr)
            qg(:, :, k) = spec_to_grid(state%tr(:, :, k, j1, 1), 1, state%cosgr) ! q
            phig(:, :, k) = spec_to_grid(state%phi(:, :, k), 1, state%cosgr)
        end do

        pslg = spec_to_grid(state%ps(:, :, j1), 1, state%cosgr)

        ! =========================================================================
        ! Compute thermodynamic variables
        ! =========================================================================

        psg = exp(pslg)
        rps = 1.0/psg

        qg = max(qg, 0.0)
        se = cp*tg + phig

        do k = 1, kx
            call spec_hum_to_rel_hum(tg(:, :, k), psg, state%fsg(k), qg(:, :, k), &
                                     rh(:, :, k), qsat(:, :, k))
        end do

        ! =========================================================================
        ! Precipitation
        ! =========================================================================

        ! Deep convection 
        call get_convection_tendencies(state, psg, se, qg, qsat, iptop, tt_cnv, qt_cnv)

        do k = 2, kx
            tt_cnv(:, :, k) = tt_cnv(:, :, k)*rps*grdscp(k)
            qt_cnv(:, :, k) = qt_cnv(:, :, k)*rps*grdsig(k)
        end do

        icnv = kx - iptop

        ! Large-scale condensation
        call get_large_scale_condensation_tendencies(state, psg, qg, qsat, iptop, &
                                                     tt_lsc, qt_lsc)

        ttend = ttend + tt_cnv + tt_lsc
        qtend = qtend + qt_cnv + qt_lsc

        ! =========================================================================
        ! Radiation (shortwave and longwave) and surface fluxes
        ! =========================================================================

        ! Compute shortwave tendencies and initialize lw transmissivity
        ! The shortwave radiation may be called at selected time steps
        if (compute_shortwave) then
            gse = (se(:, :, kx - 1) - se(:, :, kx))/(phig(:, :, kx - 1) - phig(:, :, kx))

            call clouds(qg, rh, state%precnv, state%precls, iptop, gse, &
                        fmask_l, icltop, cloudc, clstr)

            do i = 1, ix
                do j = 1, il
                    cltop(i, j) = sigh(icltop(i, j, 1) - 1)*psg(i, j)
                    prtop(i, j) = float(iptop(i, j))
                end do
            end do

            call get_shortwave_rad_fluxes(state, psg, qg, icltop, cloudc, clstr, &
                                          state%ssrd, state%ssr, &
                                          state%tsr, tt_rsw)

            do k = 1, kx
                tt_rsw(:, :, k) = tt_rsw(:, :, k)*rps*grdscp(k)
            end do
        end if

        ! Compute downward longwave fluxes
        call get_downward_longwave_rad_fluxes(tg, state%slrd, tt_rlw)

        ! Compute surface fluxes and land skin temperature
        call get_surface_fluxes(state, psg, ug, vg, tg, qg, rh, phig, fmask_l, &
                                sst_am, ts, tskin, u0, v0, t0, .true.)

        ! Recompute sea fluxes in case of anomaly coupling
        if (sea_coupling_flag > 0) then
            call get_surface_fluxes(state, psg, ug, vg, tg, qg, rh, phig, fmask_l, &
                                    sst_am, ts, tskin, u0, v0, t0, .false.)
        end if

        ! Compute upward longwave fluxes, convert them to tendencies and add
        ! shortwave tendencies
        call get_upward_longwave_rad_fluxes(state, tg, ts, state%slrd, &
                                            state%slru(:, :, 3), state%slr, &
                                            state%olr, tt_rlw)



        do k = 1, kx
            tt_rlw(:, :, k) = tt_rlw(:, :, k)*rps*grdscp(k)
        end do

        ttend = ttend + tt_rsw + tt_rlw

        ! =========================================================================
        ! Planetary boundary later interactions with lower troposphere
        ! =========================================================================

        ! Vertical diffusion and shallow convection
        call get_vertical_diffusion_tend(state, se, rh, qg, qsat, phig, icnv, ut_pbl, vt_pbl, &
            & tt_pbl, qt_pbl)

        ! Add tendencies due to surface fluxes
        ut_pbl(:, :, kx) = ut_pbl(:, :, kx) + state%ustr(:, :, 3)*rps*grdsig(kx)
        vt_pbl(:, :, kx) = vt_pbl(:, :, kx) + state%vstr(:, :, 3)*rps*grdsig(kx)
        tt_pbl(:, :, kx) = tt_pbl(:, :, kx) + state%shf(:, :, 3)*rps*grdscp(kx)
        qt_pbl(:, :, kx) = qt_pbl(:, :, kx) + state%evap(:, :, 3)*rps*grdsig(kx)

        utend = utend + ut_pbl
        vtend = vtend + vt_pbl
        ttend = ttend + tt_pbl
        qtend = qtend + qt_pbl

        ! Add SPPT noise
        if (sppt_on) then
            sppt_pattern = gen_sppt(state)

            ! The physical contribution to the tendency is *tend - *tend_dyn, where * is u, v, t, q
            do k = 1, kx
                utend(:, :, k) = (1 + sppt_pattern(:, :, k)*mu(k))*(utend(:, :, k) - utend_dyn(:, :, k)) &
                                 + utend_dyn(:, :, k)
                vtend(:, :, k) = (1 + sppt_pattern(:, :, k)*mu(k))*(vtend(:, :, k) - vtend_dyn(:, :, k)) &
                                 + vtend_dyn(:, :, k)
                ttend(:, :, k) = (1 + sppt_pattern(:, :, k)*mu(k))*(ttend(:, :, k) - ttend_dyn(:, :, k)) &
                                 + ttend_dyn(:, :, k)
                qtend(:, :, k) = (1 + sppt_pattern(:, :, k)*mu(k))*(qtend(:, :, k) - qtend_dyn(:, :, k)) &
                                 + qtend_dyn(:, :, k)
            end do
        end if
    end
end module
