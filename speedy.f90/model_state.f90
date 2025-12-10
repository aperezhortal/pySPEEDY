!> author: Andres Perez Hortal
!  date: 23/06/2021
!  Module where all the model state variables are declared.
!  THIS CODE IS AUTOMATICALLY GENERATED.
!  DO NOT MODIFY IT DIRECTLY.

module model_state
    use types, only: p
    use spectral, only: ModSpectral_t
    use implicit, only: ModImplicit_t
    use geometry, only: ModGeometry_t
    use horizontal_diffusion,only: ModHorizontalDiffusion_t

    implicit none

    private
    public ModelState_t, ModelState_Ptr_t
    public ModelState_allocate, ModelState_deallocate
    public ModelState_allocate_sst_anom


    !> Model state
    type ModelState_t

        logical :: initialized = .false.

        class(ModGeometry_t), allocatable :: mod_geometry ! Geometry module instance
        logical :: mod_geometry_initialized = .false.
        class(ModSpectral_t), allocatable :: mod_spectral ! Spectral module instance
        logical :: mod_spectral_initialized = .false.
        class(ModImplicit_t), allocatable :: mod_implicit ! Implicit module instance
        logical :: mod_implicit_initialized = .false.

        complex(8), allocatable, dimension(:, :, :, :)  :: vor ! Vorticity
        logical :: vor_initialized = .false.

        complex(8), allocatable, dimension(:, :, :, :)  :: div ! Divergence
        logical :: div_initialized = .false.

        complex(8), allocatable, dimension(:, :, :, :)  :: t ! Temperature
        logical :: t_initialized = .false.

        complex(8), allocatable, dimension(:, :, :)  :: ps ! Log of (normalised) surface pressure
        logical :: ps_initialized = .false.

        complex(8), allocatable, dimension(:, :, :, :, :)  :: tr ! Tracers (tr(1): specific humidity in g/kg)
        logical :: tr_initialized = .false.

        complex(8), allocatable, dimension(:, :, :)  :: phi ! Atmospheric geopotential
        logical :: phi_initialized = .false.

        complex(8), allocatable, dimension(:, :)  :: phis ! Surface geopotential
        logical :: phis_initialized = .false.

        real(8), allocatable, dimension(:, :, :)  :: u_grid ! eastward_wind
        logical :: u_grid_initialized = .false.

        real(8), allocatable, dimension(:, :, :)  :: v_grid ! northward_wind
        logical :: v_grid_initialized = .false.

        real(8), allocatable, dimension(:, :, :)  :: t_grid ! air_temperature
        logical :: t_grid_initialized = .false.

        real(8), allocatable, dimension(:, :, :)  :: q_grid ! specific_humidity
        logical :: q_grid_initialized = .false.

        real(8), allocatable, dimension(:, :, :)  :: phi_grid ! geopotential_height
        logical :: phi_grid_initialized = .false.

        real(8), allocatable, dimension(:, :)  :: ps_grid ! surface_air_pressure
        logical :: ps_grid_initialized = .false.

        real(8), allocatable, dimension(:, :)  :: precnv ! Convective precipitation, total
        logical :: precnv_initialized = .false.

        real(8), allocatable, dimension(:, :)  :: precls ! Large-scale precipitation, total
        logical :: precls_initialized = .false.

        real(8), allocatable, dimension(:, :)  :: snowcv ! Convective precipitation, snow only
        logical :: snowcv_initialized = .false.

        real(8), allocatable, dimension(:, :)  :: snowls ! Large-scale precipitation, snow only
        logical :: snowls_initialized = .false.

        real(8), allocatable, dimension(:, :)  :: cbmf ! Cloud-base mass flux
        logical :: cbmf_initialized = .false.

        real(8), allocatable, dimension(:, :)  :: tsr ! Top-of-atmosphere shortwave radiation (downward)
        logical :: tsr_initialized = .false.

        real(8), allocatable, dimension(:, :)  :: ssrd ! Surface shortwave radiation (downward-only)
        logical :: ssrd_initialized = .false.

        real(8), allocatable, dimension(:, :)  :: ssr ! Surface shortwave radiation (net downward)
        logical :: ssr_initialized = .false.

        real(8), allocatable, dimension(:, :)  :: slrd ! Surface longwave radiation (downward-only)
        logical :: slrd_initialized = .false.

        real(8), allocatable, dimension(:, :)  :: slr ! Surface longwave radiation (net upward)
        logical :: slr_initialized = .false.

        real(8), allocatable, dimension(:, :)  :: olr ! Outgoing longwave radiation (upward)
        logical :: olr_initialized = .false.

        real(8), allocatable, dimension(:, :, :)  :: slru ! Surface longwave emission (upward)
        logical :: slru_initialized = .false.

        real(8), allocatable, dimension(:, :, :)  :: ustr ! U-stress
        logical :: ustr_initialized = .false.

        real(8), allocatable, dimension(:, :, :)  :: vstr ! Vstress
        logical :: vstr_initialized = .false.

        real(8), allocatable, dimension(:, :, :)  :: shf ! Sensible heat flux
        logical :: shf_initialized = .false.

        real(8), allocatable, dimension(:, :, :)  :: evap ! Evaporation
        logical :: evap_initialized = .false.

        real(8), allocatable, dimension(:, :, :)  :: hfluxn ! Net heat flux into surface
        logical :: hfluxn_initialized = .false.

        real(8), allocatable, dimension(:, :, :)  :: tt_rsw ! Flux of short-wave radiation absorbed in each atmospheric layer
        logical :: tt_rsw_initialized = .false.

        real(8), allocatable, dimension(:, :)  :: phi0 ! Unfiltered surface geopotential
        logical :: phi0_initialized = .false.

        real(8), allocatable, dimension(:, :)  :: orog ! Orography
        logical :: orog_initialized = .false.

        real(8), allocatable, dimension(:, :)  :: phis0 ! Spectrally-filtered surface geopotential
        logical :: phis0_initialized = .false.

        real(8), allocatable, dimension(:, :)  :: alb0 ! Bare-land annual-mean albedo
        logical :: alb0_initialized = .false.

        real(8), allocatable, dimension(:, :)  :: forog ! Orographic factor for land surface drag
        logical :: forog_initialized = .false.

        real(8), allocatable, dimension(:, :)  :: fmask_orig ! Original (fractional) land-sea mask
        logical :: fmask_orig_initialized = .false.

        real(8), allocatable, dimension(:)  :: xgeop1 ! Constant 1 for hydrostatic equation
        logical :: xgeop1_initialized = .false.

        real(8), allocatable, dimension(:)  :: xgeop2 ! Constant 2 for hydrostatic equation
        logical :: xgeop2_initialized = .false.

        real(8), allocatable, dimension(:, :, :)  :: stl12 ! Land surface temperature monthly-mean climatology
        logical :: stl12_initialized = .false.

        real(8), allocatable, dimension(:, :, :)  :: snowd12 ! Snow depth (water equivalent) monthly-mean climatology
        logical :: snowd12_initialized = .false.

        real(8), allocatable, dimension(:, :, :)  :: soilw12 ! Soil water availability monthly-mean climatology
        logical :: soilw12_initialized = .false.

        real(8), allocatable, dimension(:, :)  :: veg_low ! Low vegetation fraction
        logical :: veg_low_initialized = .false.

        real(8), allocatable, dimension(:, :)  :: veg_high ! High vegetation fraction
        logical :: veg_high_initialized = .false.

        real(8), allocatable, dimension(:, :, :)  :: soil_wc_l1 ! Soil water content: Layer 1
        logical :: soil_wc_l1_initialized = .false.

        real(8), allocatable, dimension(:, :, :)  :: soil_wc_l2 ! Soil water content: Layer 2
        logical :: soil_wc_l2_initialized = .false.

        real(8), allocatable, dimension(:, :, :)  :: soil_wc_l3 ! Soil water content: Layer 3
        logical :: soil_wc_l3_initialized = .false.

        real(8), allocatable, dimension(:, :, :)  :: sst12 ! Sea/ice surface temperature
        logical :: sst12_initialized = .false.

        real(8), allocatable, dimension(:, :, :)  :: sea_ice_frac12 ! Sea ice fraction
        logical :: sea_ice_frac12_initialized = .false.

        real(8), allocatable, dimension(:, :, :)  :: sst_anom ! Observed SST anomaly (input).
        logical :: sst_anom_initialized = .false.

        real(8), allocatable, dimension(:, :)  :: flux_solar_in ! Flux of incoming solar radiation
        logical :: flux_solar_in_initialized = .false.

        real(8), allocatable, dimension(:, :)  :: flux_ozone_lower ! Flux absorbed by ozone (lower stratosphere)
        logical :: flux_ozone_lower_initialized = .false.

        real(8), allocatable, dimension(:, :)  :: flux_ozone_upper ! Flux absorbed by ozone (upper stratosphere)
        logical :: flux_ozone_upper_initialized = .false.

        real(8), allocatable, dimension(:, :)  :: zenit_correction ! Zenith angle correction to (downward) absorptivity
        logical :: zenit_correction_initialized = .false.

        real(8), allocatable, dimension(:, :)  :: stratospheric_correction ! Stratospheric correction for polar night
        logical :: stratospheric_correction_initialized = .false.

        real(8), allocatable, dimension(:, :)  :: qcloud_equiv !  Equivalent specific humidity of clouds
        logical :: qcloud_equiv_initialized = .false.

        real(8), allocatable, dimension(:, :)  :: rhcapl ! 1/heat capacity (land)
        logical :: rhcapl_initialized = .false.

        real(8), allocatable, dimension(:, :)  :: cdland !  1/dissipation time (land)
        logical :: cdland_initialized = .false.

        real(8), allocatable, dimension(:, :)  :: stlcl_obs ! Climatological land surface temperature
        logical :: stlcl_obs_initialized = .false.

        real(8), allocatable, dimension(:, :)  :: snowdcl_obs ! Climatological snow depth (water equivalent)
        logical :: snowdcl_obs_initialized = .false.

        real(8), allocatable, dimension(:, :)  :: soilwcl_obs ! Climatological soil water availability
        logical :: soilwcl_obs_initialized = .false.

        real(8), allocatable, dimension(:, :)  :: land_temp ! Land surface temperature
        logical :: land_temp_initialized = .false.

        real(8), allocatable, dimension(:, :)  :: snow_depth ! Snow depth (water equivalent)
        logical :: snow_depth_initialized = .false.

        real(8), allocatable, dimension(:, :)  :: soil_avail_water ! Soil water availability
        logical :: soil_avail_water_initialized = .false.

        real(8), allocatable, dimension(:, :)  :: stl_lm ! Land-model surface temperature
        logical :: stl_lm_initialized = .false.

        real(8), allocatable, dimension(:, :)  :: fmask_land ! Fraction of land
        logical :: fmask_land_initialized = .false.

        real(8), allocatable, dimension(:, :)  :: bmask_land !  Binary land mask
        logical :: bmask_land_initialized = .false.

        real(p), allocatable, dimension(:, :)  :: rhcaps ! 1./heat_capacity (sea)
        logical :: rhcaps_initialized = .false.

        real(p), allocatable, dimension(:, :)  :: rhcapi ! 1./heat_capacity (ice)
        logical :: rhcapi_initialized = .false.

        real(p), allocatable, dimension(:, :)  :: cdsea ! 1./dissip_time (sea)
        logical :: cdsea_initialized = .false.

        real(p), allocatable, dimension(:, :)  :: cdice ! 1./dissip_time (ice)
        logical :: cdice_initialized = .false.

        real(p), allocatable, dimension(:, :)  :: fmask_sea ! Fraction of sea
        logical :: fmask_sea_initialized = .false.

        real(p), allocatable, dimension(:, :)  :: bmask_sea ! Binary sea mask
        logical :: bmask_sea_initialized = .false.

        real(p), allocatable, dimension(:)  :: deglat_s ! Grid latitudes
        logical :: deglat_s_initialized = .false.

        real(p), allocatable, dimension(:, :)  :: hfseacl ! Annual-mean heat flux into sea sfc.
        logical :: hfseacl_initialized = .false.

        real(p), allocatable, dimension(:, :, :)  :: sstom12 ! Ocean model SST climatology
        logical :: sstom12_initialized = .false.

        real(p), allocatable, dimension(:, :)  :: sstcl_ob ! Observed clim. SST
        logical :: sstcl_ob_initialized = .false.

        real(p), allocatable, dimension(:, :)  :: sicecl_ob ! Clim. sea ice fraction
        logical :: sicecl_ob_initialized = .false.

        real(p), allocatable, dimension(:, :)  :: ticecl_ob ! Clim. sea ice temperature
        logical :: ticecl_ob_initialized = .false.

        real(p), allocatable, dimension(:, :)  :: sstan_ob ! Daily observed SST anomaly
        logical :: sstan_ob_initialized = .false.

        real(p), allocatable, dimension(:, :)  :: sstcl_om ! Ocean model clim. SST
        logical :: sstcl_om_initialized = .false.

        real(p), allocatable, dimension(:, :)  :: sst_am ! SST (full-field)
        logical :: sst_am_initialized = .false.

        real(p), allocatable, dimension(:, :)  :: sstan_am ! SST anomaly
        logical :: sstan_am_initialized = .false.

        real(p), allocatable, dimension(:, :)  :: sice_am ! Sea ice fraction
        logical :: sice_am_initialized = .false.

        real(p), allocatable, dimension(:, :)  :: tice_am ! Sea ice temperature
        logical :: tice_am_initialized = .false.

        real(p), allocatable, dimension(:, :)  :: sst_om ! Ocean model SST
        logical :: sst_om_initialized = .false.

        real(p), allocatable, dimension(:, :)  :: sice_om ! Model sea ice fraction
        logical :: sice_om_initialized = .false.

        real(p), allocatable, dimension(:, :)  :: tice_om ! Model sea ice temperature
        logical :: tice_om_initialized = .false.

        real(p), allocatable, dimension(:, :)  :: ssti_om ! Model SST + sea ice temp.
        logical :: ssti_om_initialized = .false.

        real(p), allocatable, dimension(:, :)  :: wsst_ob ! Weight for obs. SST anomaly in coupled runs
        logical :: wsst_ob_initialized = .false.

        real(8), allocatable, dimension(:, :)  :: fband ! Energy fraction emitted in each LW band = f(T)
        logical :: fband_initialized = .false.

        real(8), allocatable, dimension(:, :)  :: alb_land ! Daily-mean albedo over land (bare-land + snow)
        logical :: alb_land_initialized = .false.

        real(8), allocatable, dimension(:, :)  :: alb_sea ! Daily-mean albedo over sea  (open sea + sea ice)
        logical :: alb_sea_initialized = .false.

        real(8), allocatable, dimension(:, :)  :: alb_surface ! Combined surface albedo (land + sea)
        logical :: alb_surface_initialized = .false.

        real(8), allocatable, dimension(:, :)  :: snowc ! Effective snow cover (fraction)
        logical :: snowc_initialized = .false.

        real(8), allocatable, dimension(:, :, :)  :: rad_flux ! Radiative flux in different spectral bands
        logical :: rad_flux_initialized = .false.

        real(8), allocatable, dimension(:, :, :, :)  :: rad_tau2 ! Transmissivity of atmospheric layers
        logical :: rad_tau2_initialized = .false.

        real(8), allocatable, dimension(:, :, :, :)  :: rad_st4a ! Blackbody emission from full and half atmospheric levels
        logical :: rad_st4a_initialized = .false.

        real(8), allocatable, dimension(:, :, :)  :: rad_strat_corr ! Stratospheric correction term
        logical :: rad_strat_corr_initialized = .false.

        real, allocatable, dimension(:)  :: lon ! longitude
        logical :: lon_initialized = .false.

        real, allocatable, dimension(:)  :: lat ! latitude
        logical :: lat_initialized = .false.

        real, allocatable, dimension(:)  :: lev ! Vertical sigma coordinate
        logical :: lev_initialized = .false.


        integer :: current_step ! Current model step.
        logical :: increase_co2 !  Flag for CO2 optical thickness increase
        logical :: compute_shortwave ! Flag for shortwave radiation routine (turned on and off in main loop depending on the value of nstrad)
        real(8) :: air_absortivity_co2 ! Absorptivity of air in CO2 band
        logical :: land_coupling_flag ! Flag for land-coupling (0: off, 1: on)
        logical :: sst_anomaly_coupling_flag ! Weight for obs. SST anomaly in coupled runs
        real(8) :: ablco2_ref ! Initial absorptivity of air in CO2 band (t=t0)

    end type

    type ModelState_Ptr_t
        type(ModelState_t), pointer :: p => NULL()
    end type

contains

    !> Allocate Model State variables
    subroutine ModelState_allocate(state)
        use params, only: mx, nx, kx, ntr, ix, il, iy, t_levs, aux_dim
        type(ModelState_t), intent(out) :: state

        allocate( state%mod_geometry ) ! Geometry module instance
        state%mod_geometry_initialized = .true.
        allocate( state%mod_spectral ) ! Spectral module instance
        state%mod_spectral_initialized = .true.
        allocate( state%mod_implicit ) ! Implicit module instance
        state%mod_implicit_initialized = .true.

        !========================================================
        ! Allocate model state variables
        allocate( state%vor(mx, nx, kx, t_levs) ) ! Vorticity
        state%vor_initialized = .true.
        state%vor=0

        allocate( state%div(mx, nx, kx, t_levs) ) ! Divergence
        state%div_initialized = .true.
        state%div=0

        allocate( state%t(mx, nx, kx, t_levs) ) ! Temperature
        state%t_initialized = .true.
        state%t=0

        allocate( state%ps(mx, nx, t_levs) ) ! Log of (normalised) surface pressure
        state%ps_initialized = .true.
        state%ps=0

        allocate( state%tr(mx, nx, kx, t_levs,ntr) ) ! Tracers (tr(1): specific humidity in g/kg)
        state%tr_initialized = .true.
        state%tr=0

        allocate( state%phi(mx, nx, kx) ) ! Atmospheric geopotential
        state%phi_initialized = .true.
        state%phi=0

        allocate( state%phis(mx, nx) ) ! Surface geopotential
        state%phis_initialized = .true.
        state%phis=0

        allocate( state%u_grid(ix, il, kx) ) ! eastward_wind
        state%u_grid_initialized = .true.
        state%u_grid=0

        allocate( state%v_grid(ix, il, kx) ) ! northward_wind
        state%v_grid_initialized = .true.
        state%v_grid=0

        allocate( state%t_grid(ix, il, kx) ) ! air_temperature
        state%t_grid_initialized = .true.
        state%t_grid=0

        allocate( state%q_grid(ix, il, kx) ) ! specific_humidity
        state%q_grid_initialized = .true.
        state%q_grid=0

        allocate( state%phi_grid(ix, il, kx) ) ! geopotential_height
        state%phi_grid_initialized = .true.
        state%phi_grid=0

        allocate( state%ps_grid(ix, il) ) ! surface_air_pressure
        state%ps_grid_initialized = .true.
        state%ps_grid=0

        allocate( state%precnv(ix, il) ) ! Convective precipitation, total
        state%precnv_initialized = .true.
        state%precnv=0

        allocate( state%precls(ix, il) ) ! Large-scale precipitation, total
        state%precls_initialized = .true.
        state%precls=0

        allocate( state%snowcv(ix, il) ) ! Convective precipitation, snow only
        state%snowcv_initialized = .true.
        state%snowcv=0

        allocate( state%snowls(ix, il) ) ! Large-scale precipitation, snow only
        state%snowls_initialized = .true.
        state%snowls=0

        allocate( state%cbmf(ix, il) ) ! Cloud-base mass flux
        state%cbmf_initialized = .true.
        state%cbmf=0

        allocate( state%tsr(ix, il) ) ! Top-of-atmosphere shortwave radiation (downward)
        state%tsr_initialized = .true.
        state%tsr=0

        allocate( state%ssrd(ix, il) ) ! Surface shortwave radiation (downward-only)
        state%ssrd_initialized = .true.
        state%ssrd=0

        allocate( state%ssr(ix, il) ) ! Surface shortwave radiation (net downward)
        state%ssr_initialized = .true.
        state%ssr=0

        allocate( state%slrd(ix, il) ) ! Surface longwave radiation (downward-only)
        state%slrd_initialized = .true.
        state%slrd=0

        allocate( state%slr(ix, il) ) ! Surface longwave radiation (net upward)
        state%slr_initialized = .true.
        state%slr=0

        allocate( state%olr(ix, il) ) ! Outgoing longwave radiation (upward)
        state%olr_initialized = .true.
        state%olr=0

        allocate( state%slru(ix, il,aux_dim) ) ! Surface longwave emission (upward)
        state%slru_initialized = .true.
        state%slru=0

        allocate( state%ustr(ix, il,aux_dim) ) ! U-stress
        state%ustr_initialized = .true.
        state%ustr=0

        allocate( state%vstr(ix, il,aux_dim) ) ! Vstress
        state%vstr_initialized = .true.
        state%vstr=0

        allocate( state%shf(ix, il,aux_dim) ) ! Sensible heat flux
        state%shf_initialized = .true.
        state%shf=0

        allocate( state%evap(ix, il,aux_dim) ) ! Evaporation
        state%evap_initialized = .true.
        state%evap=0

        allocate( state%hfluxn(ix, il,aux_dim) ) ! Net heat flux into surface
        state%hfluxn_initialized = .true.
        state%hfluxn=0

        allocate( state%tt_rsw(ix, il,kx) ) ! Flux of short-wave radiation absorbed in each atmospheric layer
        state%tt_rsw_initialized = .true.
        state%tt_rsw=0

        allocate( state%phi0(ix, il) ) ! Unfiltered surface geopotential
        state%phi0_initialized = .true.
        state%phi0=0

        allocate( state%orog(ix, il) ) ! Orography
        state%orog_initialized = .true.
        state%orog=0

        allocate( state%phis0(ix, il) ) ! Spectrally-filtered surface geopotential
        state%phis0_initialized = .true.
        state%phis0=0

        allocate( state%alb0(ix, il) ) ! Bare-land annual-mean albedo
        state%alb0_initialized = .true.
        state%alb0=0

        allocate( state%forog(ix, il) ) ! Orographic factor for land surface drag
        state%forog_initialized = .true.
        state%forog=0

        allocate( state%fmask_orig(ix, il) ) ! Original (fractional) land-sea mask
        state%fmask_orig_initialized = .true.
        state%fmask_orig=0

        allocate( state%xgeop1(kx) ) ! Constant 1 for hydrostatic equation
        state%xgeop1_initialized = .true.
        state%xgeop1=0

        allocate( state%xgeop2(kx) ) ! Constant 2 for hydrostatic equation
        state%xgeop2_initialized = .true.
        state%xgeop2=0

        allocate( state%stl12(ix, il, 12) ) ! Land surface temperature monthly-mean climatology
        state%stl12_initialized = .true.
        state%stl12=0

        allocate( state%snowd12(ix, il, 12) ) ! Snow depth (water equivalent) monthly-mean climatology
        state%snowd12_initialized = .true.
        state%snowd12=0

        allocate( state%soilw12(ix, il, 12) ) ! Soil water availability monthly-mean climatology
        state%soilw12_initialized = .true.
        state%soilw12=0

        allocate( state%veg_low(ix, il) ) ! Low vegetation fraction
        state%veg_low_initialized = .true.
        state%veg_low=0

        allocate( state%veg_high(ix, il) ) ! High vegetation fraction
        state%veg_high_initialized = .true.
        state%veg_high=0

        allocate( state%soil_wc_l1(ix, il, 12) ) ! Soil water content: Layer 1
        state%soil_wc_l1_initialized = .true.
        state%soil_wc_l1=0

        allocate( state%soil_wc_l2(ix, il, 12) ) ! Soil water content: Layer 2
        state%soil_wc_l2_initialized = .true.
        state%soil_wc_l2=0

        allocate( state%soil_wc_l3(ix, il, 12) ) ! Soil water content: Layer 3
        state%soil_wc_l3_initialized = .true.
        state%soil_wc_l3=0

        allocate( state%sst12(ix, il, 12) ) ! Sea/ice surface temperature
        state%sst12_initialized = .true.
        state%sst12=0

        allocate( state%sea_ice_frac12(ix, il, 12) ) ! Sea ice fraction
        state%sea_ice_frac12_initialized = .true.
        state%sea_ice_frac12=0

        allocate( state%flux_solar_in(ix, il) ) ! Flux of incoming solar radiation
        state%flux_solar_in_initialized = .true.
        state%flux_solar_in=0

        allocate( state%flux_ozone_lower(ix, il) ) ! Flux absorbed by ozone (lower stratosphere)
        state%flux_ozone_lower_initialized = .true.
        state%flux_ozone_lower=0

        allocate( state%flux_ozone_upper(ix, il) ) ! Flux absorbed by ozone (upper stratosphere)
        state%flux_ozone_upper_initialized = .true.
        state%flux_ozone_upper=0

        allocate( state%zenit_correction(ix, il) ) ! Zenith angle correction to (downward) absorptivity
        state%zenit_correction_initialized = .true.
        state%zenit_correction=0

        allocate( state%stratospheric_correction(ix, il) ) ! Stratospheric correction for polar night
        state%stratospheric_correction_initialized = .true.
        state%stratospheric_correction=0

        allocate( state%qcloud_equiv(ix, il) ) !  Equivalent specific humidity of clouds
        state%qcloud_equiv_initialized = .true.
        state%qcloud_equiv=0

        allocate( state%rhcapl(ix, il) ) ! 1/heat capacity (land)
        state%rhcapl_initialized = .true.
        state%rhcapl=0

        allocate( state%cdland(ix, il) ) !  1/dissipation time (land)
        state%cdland_initialized = .true.
        state%cdland=0

        allocate( state%stlcl_obs(ix, il) ) ! Climatological land surface temperature
        state%stlcl_obs_initialized = .true.
        state%stlcl_obs=0

        allocate( state%snowdcl_obs(ix, il) ) ! Climatological snow depth (water equivalent)
        state%snowdcl_obs_initialized = .true.
        state%snowdcl_obs=0

        allocate( state%soilwcl_obs(ix, il) ) ! Climatological soil water availability
        state%soilwcl_obs_initialized = .true.
        state%soilwcl_obs=0

        allocate( state%land_temp(ix, il) ) ! Land surface temperature
        state%land_temp_initialized = .true.
        state%land_temp=0

        allocate( state%snow_depth(ix, il) ) ! Snow depth (water equivalent)
        state%snow_depth_initialized = .true.
        state%snow_depth=0

        allocate( state%soil_avail_water(ix, il) ) ! Soil water availability
        state%soil_avail_water_initialized = .true.
        state%soil_avail_water=0

        allocate( state%stl_lm(ix, il) ) ! Land-model surface temperature
        state%stl_lm_initialized = .true.
        state%stl_lm=0

        allocate( state%fmask_land(ix, il) ) ! Fraction of land
        state%fmask_land_initialized = .true.
        state%fmask_land=0

        allocate( state%bmask_land(ix, il) ) !  Binary land mask
        state%bmask_land_initialized = .true.
        state%bmask_land=0

        allocate( state%rhcaps(ix, il) ) ! 1./heat_capacity (sea)
        state%rhcaps_initialized = .true.
        state%rhcaps=0

        allocate( state%rhcapi(ix, il) ) ! 1./heat_capacity (ice)
        state%rhcapi_initialized = .true.
        state%rhcapi=0

        allocate( state%cdsea(ix, il) ) ! 1./dissip_time (sea)
        state%cdsea_initialized = .true.
        state%cdsea=0

        allocate( state%cdice(ix, il) ) ! 1./dissip_time (ice)
        state%cdice_initialized = .true.
        state%cdice=0

        allocate( state%fmask_sea(ix, il) ) ! Fraction of sea
        state%fmask_sea_initialized = .true.
        state%fmask_sea=0

        allocate( state%bmask_sea(ix, il) ) ! Binary sea mask
        state%bmask_sea_initialized = .true.
        state%bmask_sea=0

        allocate( state%deglat_s(il) ) ! Grid latitudes
        state%deglat_s_initialized = .true.
        state%deglat_s=0

        allocate( state%hfseacl(ix, il) ) ! Annual-mean heat flux into sea sfc.
        state%hfseacl_initialized = .true.
        state%hfseacl=0

        allocate( state%sstom12(ix, il, 12) ) ! Ocean model SST climatology
        state%sstom12_initialized = .true.
        state%sstom12=0

        allocate( state%sstcl_ob(ix, il) ) ! Observed clim. SST
        state%sstcl_ob_initialized = .true.
        state%sstcl_ob=0

        allocate( state%sicecl_ob(ix, il) ) ! Clim. sea ice fraction
        state%sicecl_ob_initialized = .true.
        state%sicecl_ob=0

        allocate( state%ticecl_ob(ix, il) ) ! Clim. sea ice temperature
        state%ticecl_ob_initialized = .true.
        state%ticecl_ob=0

        allocate( state%sstan_ob(ix, il) ) ! Daily observed SST anomaly
        state%sstan_ob_initialized = .true.
        state%sstan_ob=0

        allocate( state%sstcl_om(ix, il) ) ! Ocean model clim. SST
        state%sstcl_om_initialized = .true.
        state%sstcl_om=0

        allocate( state%sst_am(ix, il) ) ! SST (full-field)
        state%sst_am_initialized = .true.
        state%sst_am=0

        allocate( state%sstan_am(ix, il) ) ! SST anomaly
        state%sstan_am_initialized = .true.
        state%sstan_am=0

        allocate( state%sice_am(ix, il) ) ! Sea ice fraction
        state%sice_am_initialized = .true.
        state%sice_am=0

        allocate( state%tice_am(ix, il) ) ! Sea ice temperature
        state%tice_am_initialized = .true.
        state%tice_am=0

        allocate( state%sst_om(ix, il) ) ! Ocean model SST
        state%sst_om_initialized = .true.
        state%sst_om=0

        allocate( state%sice_om(ix, il) ) ! Model sea ice fraction
        state%sice_om_initialized = .true.
        state%sice_om=0

        allocate( state%tice_om(ix, il) ) ! Model sea ice temperature
        state%tice_om_initialized = .true.
        state%tice_om=0

        allocate( state%ssti_om(ix, il) ) ! Model SST + sea ice temp.
        state%ssti_om_initialized = .true.
        state%ssti_om=0

        allocate( state%wsst_ob(ix, il) ) ! Weight for obs. SST anomaly in coupled runs
        state%wsst_ob_initialized = .true.
        state%wsst_ob=0

        allocate( state%fband(100:400,4) ) ! Energy fraction emitted in each LW band = f(T)
        state%fband_initialized = .true.
        state%fband=0

        allocate( state%alb_land(ix,il) ) ! Daily-mean albedo over land (bare-land + snow)
        state%alb_land_initialized = .true.
        state%alb_land=0

        allocate( state%alb_sea(ix,il) ) ! Daily-mean albedo over sea  (open sea + sea ice)
        state%alb_sea_initialized = .true.
        state%alb_sea=0

        allocate( state%alb_surface(ix,il) ) ! Combined surface albedo (land + sea)
        state%alb_surface_initialized = .true.
        state%alb_surface=0

        allocate( state%snowc(ix,il) ) ! Effective snow cover (fraction)
        state%snowc_initialized = .true.
        state%snowc=0

        allocate( state%rad_flux(ix,il,4) ) ! Radiative flux in different spectral bands
        state%rad_flux_initialized = .true.
        state%rad_flux=0

        allocate( state%rad_tau2(ix,il,kx,4) ) ! Transmissivity of atmospheric layers
        state%rad_tau2_initialized = .true.
        state%rad_tau2=0

        allocate( state%rad_st4a(ix,il,kx,2) ) ! Blackbody emission from full and half atmospheric levels
        state%rad_st4a_initialized = .true.
        state%rad_st4a=0

        allocate( state%rad_strat_corr(ix,il,2) ) ! Stratospheric correction term
        state%rad_strat_corr_initialized = .true.
        state%rad_strat_corr=0

        allocate( state%lon(ix) ) ! longitude
        state%lon_initialized = .true.
        state%lon=0

        allocate( state%lat(il) ) ! latitude
        state%lat_initialized = .true.
        state%lat=0

        allocate( state%lev(kx) ) ! Vertical sigma coordinate
        state%lev_initialized = .true.
        state%lev=0


        ! IMPORTANT: sst_anom is not initialized here

        ! Initialize default values for scalars
        state%increase_co2= .false.
        state%compute_shortwave= .true.
        state%air_absortivity_co2= 6.0
        state%land_coupling_flag= .true.
        state%sst_anomaly_coupling_flag= .true.

    end subroutine


    !> Allocate sst_anom
    subroutine ModelState_allocate_sst_anom(state, n_months)
        use params, only: mx, nx, kx, ntr, ix, il, iy, t_levs, aux_dim
        type(ModelState_t), intent(inout) :: state
        integer, intent(in) :: n_months

        !=========================================
        ! Allocate sst_anom(lon, lat, n_months)
        allocate( state%sst_anom(ix, il, 0:n_months+1) ) ! Observed SST anomaly (input).
        state%sst_anom = 0
        state%sst_anom_initialized = .true.
    end subroutine


    !> Deallocate model state variables
    subroutine ModelState_deallocate(state)
        type(ModelState_t), intent(inout) :: state

        !========================================================
        ! Safely deallocate the model state variables
        if( allocated(state%vor) ) deallocate( state%vor ) ! Vorticity
        state%vor_initialized = .false.

        if( allocated(state%div) ) deallocate( state%div ) ! Divergence
        state%div_initialized = .false.

        if( allocated(state%t) ) deallocate( state%t ) ! Temperature
        state%t_initialized = .false.

        if( allocated(state%ps) ) deallocate( state%ps ) ! Log of (normalised) surface pressure
        state%ps_initialized = .false.

        if( allocated(state%tr) ) deallocate( state%tr ) ! Tracers (tr(1): specific humidity in g/kg)
        state%tr_initialized = .false.

        if( allocated(state%phi) ) deallocate( state%phi ) ! Atmospheric geopotential
        state%phi_initialized = .false.

        if( allocated(state%phis) ) deallocate( state%phis ) ! Surface geopotential
        state%phis_initialized = .false.

        if( allocated(state%u_grid) ) deallocate( state%u_grid ) ! eastward_wind
        state%u_grid_initialized = .false.

        if( allocated(state%v_grid) ) deallocate( state%v_grid ) ! northward_wind
        state%v_grid_initialized = .false.

        if( allocated(state%t_grid) ) deallocate( state%t_grid ) ! air_temperature
        state%t_grid_initialized = .false.

        if( allocated(state%q_grid) ) deallocate( state%q_grid ) ! specific_humidity
        state%q_grid_initialized = .false.

        if( allocated(state%phi_grid) ) deallocate( state%phi_grid ) ! geopotential_height
        state%phi_grid_initialized = .false.

        if( allocated(state%ps_grid) ) deallocate( state%ps_grid ) ! surface_air_pressure
        state%ps_grid_initialized = .false.

        if( allocated(state%precnv) ) deallocate( state%precnv ) ! Convective precipitation, total
        state%precnv_initialized = .false.

        if( allocated(state%precls) ) deallocate( state%precls ) ! Large-scale precipitation, total
        state%precls_initialized = .false.

        if( allocated(state%snowcv) ) deallocate( state%snowcv ) ! Convective precipitation, snow only
        state%snowcv_initialized = .false.

        if( allocated(state%snowls) ) deallocate( state%snowls ) ! Large-scale precipitation, snow only
        state%snowls_initialized = .false.

        if( allocated(state%cbmf) ) deallocate( state%cbmf ) ! Cloud-base mass flux
        state%cbmf_initialized = .false.

        if( allocated(state%tsr) ) deallocate( state%tsr ) ! Top-of-atmosphere shortwave radiation (downward)
        state%tsr_initialized = .false.

        if( allocated(state%ssrd) ) deallocate( state%ssrd ) ! Surface shortwave radiation (downward-only)
        state%ssrd_initialized = .false.

        if( allocated(state%ssr) ) deallocate( state%ssr ) ! Surface shortwave radiation (net downward)
        state%ssr_initialized = .false.

        if( allocated(state%slrd) ) deallocate( state%slrd ) ! Surface longwave radiation (downward-only)
        state%slrd_initialized = .false.

        if( allocated(state%slr) ) deallocate( state%slr ) ! Surface longwave radiation (net upward)
        state%slr_initialized = .false.

        if( allocated(state%olr) ) deallocate( state%olr ) ! Outgoing longwave radiation (upward)
        state%olr_initialized = .false.

        if( allocated(state%slru) ) deallocate( state%slru ) ! Surface longwave emission (upward)
        state%slru_initialized = .false.

        if( allocated(state%ustr) ) deallocate( state%ustr ) ! U-stress
        state%ustr_initialized = .false.

        if( allocated(state%vstr) ) deallocate( state%vstr ) ! Vstress
        state%vstr_initialized = .false.

        if( allocated(state%shf) ) deallocate( state%shf ) ! Sensible heat flux
        state%shf_initialized = .false.

        if( allocated(state%evap) ) deallocate( state%evap ) ! Evaporation
        state%evap_initialized = .false.

        if( allocated(state%hfluxn) ) deallocate( state%hfluxn ) ! Net heat flux into surface
        state%hfluxn_initialized = .false.

        if( allocated(state%tt_rsw) ) deallocate( state%tt_rsw ) ! Flux of short-wave radiation absorbed in each atmospheric layer
        state%tt_rsw_initialized = .false.

        if( allocated(state%phi0) ) deallocate( state%phi0 ) ! Unfiltered surface geopotential
        state%phi0_initialized = .false.

        if( allocated(state%orog) ) deallocate( state%orog ) ! Orography
        state%orog_initialized = .false.

        if( allocated(state%phis0) ) deallocate( state%phis0 ) ! Spectrally-filtered surface geopotential
        state%phis0_initialized = .false.

        if( allocated(state%alb0) ) deallocate( state%alb0 ) ! Bare-land annual-mean albedo
        state%alb0_initialized = .false.

        if( allocated(state%forog) ) deallocate( state%forog ) ! Orographic factor for land surface drag
        state%forog_initialized = .false.

        if( allocated(state%fmask_orig) ) deallocate( state%fmask_orig ) ! Original (fractional) land-sea mask
        state%fmask_orig_initialized = .false.

        if( allocated(state%xgeop1) ) deallocate( state%xgeop1 ) ! Constant 1 for hydrostatic equation
        state%xgeop1_initialized = .false.

        if( allocated(state%xgeop2) ) deallocate( state%xgeop2 ) ! Constant 2 for hydrostatic equation
        state%xgeop2_initialized = .false.

        if( allocated(state%stl12) ) deallocate( state%stl12 ) ! Land surface temperature monthly-mean climatology
        state%stl12_initialized = .false.

        if( allocated(state%snowd12) ) deallocate( state%snowd12 ) ! Snow depth (water equivalent) monthly-mean climatology
        state%snowd12_initialized = .false.

        if( allocated(state%soilw12) ) deallocate( state%soilw12 ) ! Soil water availability monthly-mean climatology
        state%soilw12_initialized = .false.

        if( allocated(state%veg_low) ) deallocate( state%veg_low ) ! Low vegetation fraction
        state%veg_low_initialized = .false.

        if( allocated(state%veg_high) ) deallocate( state%veg_high ) ! High vegetation fraction
        state%veg_high_initialized = .false.

        if( allocated(state%soil_wc_l1) ) deallocate( state%soil_wc_l1 ) ! Soil water content: Layer 1
        state%soil_wc_l1_initialized = .false.

        if( allocated(state%soil_wc_l2) ) deallocate( state%soil_wc_l2 ) ! Soil water content: Layer 2
        state%soil_wc_l2_initialized = .false.

        if( allocated(state%soil_wc_l3) ) deallocate( state%soil_wc_l3 ) ! Soil water content: Layer 3
        state%soil_wc_l3_initialized = .false.

        if( allocated(state%sst12) ) deallocate( state%sst12 ) ! Sea/ice surface temperature
        state%sst12_initialized = .false.

        if( allocated(state%sea_ice_frac12) ) deallocate( state%sea_ice_frac12 ) ! Sea ice fraction
        state%sea_ice_frac12_initialized = .false.

        if( allocated(state%sst_anom) ) deallocate( state%sst_anom ) ! Observed SST anomaly (input).
        state%sst_anom_initialized = .false.

        if( allocated(state%flux_solar_in) ) deallocate( state%flux_solar_in ) ! Flux of incoming solar radiation
        state%flux_solar_in_initialized = .false.

        if( allocated(state%flux_ozone_lower) ) deallocate( state%flux_ozone_lower ) ! Flux absorbed by ozone (lower stratosphere)
        state%flux_ozone_lower_initialized = .false.

        if( allocated(state%flux_ozone_upper) ) deallocate( state%flux_ozone_upper ) ! Flux absorbed by ozone (upper stratosphere)
        state%flux_ozone_upper_initialized = .false.

        if( allocated(state%zenit_correction) ) deallocate( state%zenit_correction ) ! Zenith angle correction to (downward) absorptivity
        state%zenit_correction_initialized = .false.

        if( allocated(state%stratospheric_correction) ) deallocate( state%stratospheric_correction ) ! Stratospheric correction for polar night
        state%stratospheric_correction_initialized = .false.

        if( allocated(state%qcloud_equiv) ) deallocate( state%qcloud_equiv ) !  Equivalent specific humidity of clouds
        state%qcloud_equiv_initialized = .false.

        if( allocated(state%rhcapl) ) deallocate( state%rhcapl ) ! 1/heat capacity (land)
        state%rhcapl_initialized = .false.

        if( allocated(state%cdland) ) deallocate( state%cdland ) !  1/dissipation time (land)
        state%cdland_initialized = .false.

        if( allocated(state%stlcl_obs) ) deallocate( state%stlcl_obs ) ! Climatological land surface temperature
        state%stlcl_obs_initialized = .false.

        if( allocated(state%snowdcl_obs) ) deallocate( state%snowdcl_obs ) ! Climatological snow depth (water equivalent)
        state%snowdcl_obs_initialized = .false.

        if( allocated(state%soilwcl_obs) ) deallocate( state%soilwcl_obs ) ! Climatological soil water availability
        state%soilwcl_obs_initialized = .false.

        if( allocated(state%land_temp) ) deallocate( state%land_temp ) ! Land surface temperature
        state%land_temp_initialized = .false.

        if( allocated(state%snow_depth) ) deallocate( state%snow_depth ) ! Snow depth (water equivalent)
        state%snow_depth_initialized = .false.

        if( allocated(state%soil_avail_water) ) deallocate( state%soil_avail_water ) ! Soil water availability
        state%soil_avail_water_initialized = .false.

        if( allocated(state%stl_lm) ) deallocate( state%stl_lm ) ! Land-model surface temperature
        state%stl_lm_initialized = .false.

        if( allocated(state%fmask_land) ) deallocate( state%fmask_land ) ! Fraction of land
        state%fmask_land_initialized = .false.

        if( allocated(state%bmask_land) ) deallocate( state%bmask_land ) !  Binary land mask
        state%bmask_land_initialized = .false.

        if( allocated(state%rhcaps) ) deallocate( state%rhcaps ) ! 1./heat_capacity (sea)
        state%rhcaps_initialized = .false.

        if( allocated(state%rhcapi) ) deallocate( state%rhcapi ) ! 1./heat_capacity (ice)
        state%rhcapi_initialized = .false.

        if( allocated(state%cdsea) ) deallocate( state%cdsea ) ! 1./dissip_time (sea)
        state%cdsea_initialized = .false.

        if( allocated(state%cdice) ) deallocate( state%cdice ) ! 1./dissip_time (ice)
        state%cdice_initialized = .false.

        if( allocated(state%fmask_sea) ) deallocate( state%fmask_sea ) ! Fraction of sea
        state%fmask_sea_initialized = .false.

        if( allocated(state%bmask_sea) ) deallocate( state%bmask_sea ) ! Binary sea mask
        state%bmask_sea_initialized = .false.

        if( allocated(state%deglat_s) ) deallocate( state%deglat_s ) ! Grid latitudes
        state%deglat_s_initialized = .false.

        if( allocated(state%hfseacl) ) deallocate( state%hfseacl ) ! Annual-mean heat flux into sea sfc.
        state%hfseacl_initialized = .false.

        if( allocated(state%sstom12) ) deallocate( state%sstom12 ) ! Ocean model SST climatology
        state%sstom12_initialized = .false.

        if( allocated(state%sstcl_ob) ) deallocate( state%sstcl_ob ) ! Observed clim. SST
        state%sstcl_ob_initialized = .false.

        if( allocated(state%sicecl_ob) ) deallocate( state%sicecl_ob ) ! Clim. sea ice fraction
        state%sicecl_ob_initialized = .false.

        if( allocated(state%ticecl_ob) ) deallocate( state%ticecl_ob ) ! Clim. sea ice temperature
        state%ticecl_ob_initialized = .false.

        if( allocated(state%sstan_ob) ) deallocate( state%sstan_ob ) ! Daily observed SST anomaly
        state%sstan_ob_initialized = .false.

        if( allocated(state%sstcl_om) ) deallocate( state%sstcl_om ) ! Ocean model clim. SST
        state%sstcl_om_initialized = .false.

        if( allocated(state%sst_am) ) deallocate( state%sst_am ) ! SST (full-field)
        state%sst_am_initialized = .false.

        if( allocated(state%sstan_am) ) deallocate( state%sstan_am ) ! SST anomaly
        state%sstan_am_initialized = .false.

        if( allocated(state%sice_am) ) deallocate( state%sice_am ) ! Sea ice fraction
        state%sice_am_initialized = .false.

        if( allocated(state%tice_am) ) deallocate( state%tice_am ) ! Sea ice temperature
        state%tice_am_initialized = .false.

        if( allocated(state%sst_om) ) deallocate( state%sst_om ) ! Ocean model SST
        state%sst_om_initialized = .false.

        if( allocated(state%sice_om) ) deallocate( state%sice_om ) ! Model sea ice fraction
        state%sice_om_initialized = .false.

        if( allocated(state%tice_om) ) deallocate( state%tice_om ) ! Model sea ice temperature
        state%tice_om_initialized = .false.

        if( allocated(state%ssti_om) ) deallocate( state%ssti_om ) ! Model SST + sea ice temp.
        state%ssti_om_initialized = .false.

        if( allocated(state%wsst_ob) ) deallocate( state%wsst_ob ) ! Weight for obs. SST anomaly in coupled runs
        state%wsst_ob_initialized = .false.

        if( allocated(state%fband) ) deallocate( state%fband ) ! Energy fraction emitted in each LW band = f(T)
        state%fband_initialized = .false.

        if( allocated(state%alb_land) ) deallocate( state%alb_land ) ! Daily-mean albedo over land (bare-land + snow)
        state%alb_land_initialized = .false.

        if( allocated(state%alb_sea) ) deallocate( state%alb_sea ) ! Daily-mean albedo over sea  (open sea + sea ice)
        state%alb_sea_initialized = .false.

        if( allocated(state%alb_surface) ) deallocate( state%alb_surface ) ! Combined surface albedo (land + sea)
        state%alb_surface_initialized = .false.

        if( allocated(state%snowc) ) deallocate( state%snowc ) ! Effective snow cover (fraction)
        state%snowc_initialized = .false.

        if( allocated(state%rad_flux) ) deallocate( state%rad_flux ) ! Radiative flux in different spectral bands
        state%rad_flux_initialized = .false.

        if( allocated(state%rad_tau2) ) deallocate( state%rad_tau2 ) ! Transmissivity of atmospheric layers
        state%rad_tau2_initialized = .false.

        if( allocated(state%rad_st4a) ) deallocate( state%rad_st4a ) ! Blackbody emission from full and half atmospheric levels
        state%rad_st4a_initialized = .false.

        if( allocated(state%rad_strat_corr) ) deallocate( state%rad_strat_corr ) ! Stratospheric correction term
        state%rad_strat_corr_initialized = .false.

        if( allocated(state%lon) ) deallocate( state%lon ) ! longitude
        state%lon_initialized = .false.

        if( allocated(state%lat) ) deallocate( state%lat ) ! latitude
        state%lat_initialized = .false.

        if( allocated(state%lev) ) deallocate( state%lev ) ! Vertical sigma coordinate
        state%lev_initialized = .false.


        if( allocated(state%mod_geometry) ) then
            call state%mod_geometry%delete()
            deallocate( state%mod_geometry ) ! Geometry module instance
        end if
        state%mod_geometry_initialized = .false.

        if( allocated(state%mod_spectral) ) then
            call state%mod_spectral%delete()
            deallocate( state%mod_spectral ) ! Spectral module instance
        end if
        state%mod_spectral_initialized = .false.

        if( allocated(state%mod_implicit) ) then
            call state%mod_implicit%delete()
            deallocate( state%mod_implicit ) ! Implicit module instance
        end if
        state%mod_implicit_initialized = .false.



        state%initialized = .false.
    end subroutine

end module