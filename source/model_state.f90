!> author: Andres Perez Hortal
!  date: 23/06/2021
!  Module where all the model state variables are declared.
!  THIS MODULE IS AUTOMATICALLY GENERATED.
!  DO NOT MODIFY IT DIRECTLY.

module model_state
    use types, only: p

    implicit none

    private
    public ModelState_t, ModelState_allocate, ModelState_deallocate

    !> Model state
    type ModelState_t
        complex(p), allocatable, dimension(:, :, :, :)  :: vor ! Vorticity
        complex(p), allocatable, dimension(:, :, :, :)  :: div ! Divergence
        complex(p), allocatable, dimension(:, :, :, :)  :: t ! Temperature [K]
        complex(p), allocatable, dimension(:, :, :)  :: ps ! Log of (normalised) surface pressure (p_s/p0)
        complex(p), allocatable, dimension(:, :, :, :, :)  :: tr ! Tracers (tr(1): specific humidity in g/kg)
        complex(p), allocatable, dimension(:, :, :)  :: phi ! Atmospheric geopotential
        complex(p), allocatable, dimension(:, :)  :: phis ! Surface geopotential
        real(p), allocatable, dimension(:, :)  :: precnv ! Convective precipitation  [g/(m^2 s)], total
        real(p), allocatable, dimension(:, :)  :: precls ! Large-scale precipitation [g/(m^2 s)], total
        real(p), allocatable, dimension(:, :)  :: snowcv ! Convective precipitation  [g/(m^2 s)], snow only
        real(p), allocatable, dimension(:, :)  :: snowls ! Large-scale precipitation [g/(m^2 s)], snow only
        real(p), allocatable, dimension(:, :)  :: cbmf ! Cloud-base mass flux
        real(p), allocatable, dimension(:, :)  :: tsr ! Top-of-atmosphere shortwave radiation (downward)
        real(p), allocatable, dimension(:, :)  :: ssrd ! Surface shortwave radiation (downward-only)
        real(p), allocatable, dimension(:, :)  :: ssr ! Surface shortwave radiation (net downward)
        real(p), allocatable, dimension(:, :)  :: slrd ! Surface longwave radiation (downward-only)
        real(p), allocatable, dimension(:, :)  :: slr !  Surface longwave radiation (net upward)
        real(p), allocatable, dimension(:, :)  :: olr ! Outgoing longwave radiation (upward)
        real(p), allocatable, dimension(:, :, :)  :: slru ! Surface longwave emission (upward)
        real(p), allocatable, dimension(:, :, :)  :: ustr ! U-stress
        real(p), allocatable, dimension(:, :, :)  :: vstr ! Vstress
        real(p), allocatable, dimension(:, :, :)  :: shf ! Sensible heat flux
        real(p), allocatable, dimension(:, :, :)  :: evap ! Evaporation [g/(m^2 s)]
        real(p), allocatable, dimension(:, :, :)  :: hfluxn ! Net heat flux into surface
        real(p), allocatable, dimension(:, :)  :: fmask_orig ! Original (fractional) land-sea mask
        real(p), allocatable, dimension(:, :)  :: phi0 ! Unfiltered surface geopotential
        real(p), allocatable, dimension(:, :)  :: phis0 ! Spectrally-filtered surface geopotential
        real(p), allocatable, dimension(:, :)  :: alb0 ! Bare-land annual-mean albedo
        real(p), allocatable, dimension(:)  :: hsg ! Half sigma levels
        real(p), allocatable, dimension(:)  :: dhs ! Sigma level thicknesses
        real(p), allocatable, dimension(:)  :: fsg ! Full sigma levels
        real(p), allocatable, dimension(:)  :: dhsr ! 1/(2*sigma level thicknesses)
        real(p), allocatable, dimension(:)  :: fsgr ! akap/(2*full sigma levels)
        real(p), allocatable, dimension(:)  :: radang ! Latitudes in radians
        real(p), allocatable, dimension(:)  :: coriol ! Coriolis parameter as a function of latitude
        real(p), allocatable, dimension(:)  :: sia ! sine(latitude)
        real(p), allocatable, dimension(:)  :: coa ! cosine(latitude)
        real(p), allocatable, dimension(:)  :: sia_half ! sine(latitude) over one hemisphere only
        real(p), allocatable, dimension(:)  :: coa_half ! cosine(latitude) over one hemisphere only
        real(p), allocatable, dimension(:)  :: cosg ! Same as coa (TODO: remove)
        real(p), allocatable, dimension(:)  :: cosgr ! 1/coa
        real(p), allocatable, dimension(:)  :: cosgr2 ! cosgr2   !! 1/coa^2
    end type

contains

    !> Allocate Model State variables
    subroutine ModelState_allocate(state)
        use params, only: mx, nx, kx, ntr, ix, il, iy
        type(ModelState_t), intent(out) :: state

        !========================================================
        ! Allocate model state variables
        allocate( state%vor(mx, nx, kx, 2) ) ! Vorticity
        allocate( state%div(mx, nx, kx, 2) ) ! Divergence
        allocate( state%t(mx, nx, kx, 2) ) ! Temperature [K]
        allocate( state%ps(mx, nx, 2) ) ! Log of (normalised) surface pressure (p_s/p0)
        allocate( state%tr(mx, nx, kx, 2,ntr) ) ! Tracers (tr(1): specific humidity in g/kg)
        allocate( state%phi(mx, nx, kx) ) ! Atmospheric geopotential
        allocate( state%phis(mx, nx) ) ! Surface geopotential
        allocate( state%precnv(ix, il) ) ! Convective precipitation  [g/(m^2 s)], total
        allocate( state%precls(ix, il) ) ! Large-scale precipitation [g/(m^2 s)], total
        allocate( state%snowcv(ix, il) ) ! Convective precipitation  [g/(m^2 s)], snow only
        allocate( state%snowls(ix, il) ) ! Large-scale precipitation [g/(m^2 s)], snow only
        allocate( state%cbmf(ix, il) ) ! Cloud-base mass flux
        allocate( state%tsr(ix, il) ) ! Top-of-atmosphere shortwave radiation (downward)
        allocate( state%ssrd(ix, il) ) ! Surface shortwave radiation (downward-only)
        allocate( state%ssr(ix, il) ) ! Surface shortwave radiation (net downward)
        allocate( state%slrd(ix, il) ) ! Surface longwave radiation (downward-only)
        allocate( state%slr(ix, il) ) !  Surface longwave radiation (net upward)
        allocate( state%olr(ix, il) ) ! Outgoing longwave radiation (upward)
        allocate( state%slru(ix, il,3) ) ! Surface longwave emission (upward)
        allocate( state%ustr(ix, il,3) ) ! U-stress
        allocate( state%vstr(ix, il,3) ) ! Vstress
        allocate( state%shf(ix, il,3) ) ! Sensible heat flux
        allocate( state%evap(ix, il,3) ) ! Evaporation [g/(m^2 s)]
        allocate( state%hfluxn(ix, il,3) ) ! Net heat flux into surface
        allocate( state%fmask_orig(ix, il) ) ! Original (fractional) land-sea mask
        allocate( state%phi0(ix, il) ) ! Unfiltered surface geopotential
        allocate( state%phis0(ix, il) ) ! Spectrally-filtered surface geopotential
        allocate( state%alb0(ix, il) ) ! Bare-land annual-mean albedo
        allocate( state%hsg(kx+1) ) ! Half sigma levels
        allocate( state%dhs(kx) ) ! Sigma level thicknesses
        allocate( state%fsg(kx) ) ! Full sigma levels
        allocate( state%dhsr(kx) ) ! 1/(2*sigma level thicknesses)
        allocate( state%fsgr(kx) ) ! akap/(2*full sigma levels)
        allocate( state%radang(il) ) ! Latitudes in radians
        allocate( state%coriol(il) ) ! Coriolis parameter as a function of latitude
        allocate( state%sia(il) ) ! sine(latitude)
        allocate( state%coa(il) ) ! cosine(latitude)
        allocate( state%sia_half(iy) ) ! sine(latitude) over one hemisphere only
        allocate( state%coa_half(il) ) ! cosine(latitude) over one hemisphere only
        allocate( state%cosg(il) ) ! Same as coa (TODO: remove)
        allocate( state%cosgr(il) ) ! 1/coa
        allocate( state%cosgr2(il) ) ! cosgr2   !! 1/coa^2

        !=====================================
        ! Initialize to 0 the allocated arrays
        state%vor=0
        state%div=0
        state%t=0
        state%ps=0
        state%tr=0
        state%phi=0
        state%phis=0
        state%precnv=0
        state%precls=0
        state%snowcv=0
        state%snowls=0
        state%cbmf=0
        state%tsr=0
        state%ssrd=0
        state%ssr=0
        state%slrd=0
        state%slr=0
        state%olr=0
        state%slru=0
        state%ustr=0
        state%vstr=0
        state%shf=0
        state%evap=0
        state%hfluxn=0
        state%fmask_orig=0
        state%phi0=0
        state%phis0=0
        state%alb0=0
        state%hsg=0
        state%dhs=0
        state%fsg=0
        state%dhsr=0
        state%fsgr=0
        state%radang=0
        state%coriol=0
        state%sia=0
        state%coa=0
        state%sia_half=0
        state%coa_half=0
        state%cosg=0
        state%cosgr=0
        state%cosgr2=0

    end subroutine

    !> Deallocate model state variables
    subroutine ModelState_deallocate(state)
        type(ModelState_t), intent(inout) :: state

        !========================================================
        ! Safely deallocate the model state variables
        if( allocated(state%vor) ) deallocate( state%vor ) ! Vorticity
        if( allocated(state%div) ) deallocate( state%div ) ! Divergence
        if( allocated(state%t) ) deallocate( state%t ) ! Temperature [K]
        if( allocated(state%ps) ) deallocate( state%ps ) ! Log of (normalised) surface pressure (p_s/p0)
        if( allocated(state%tr) ) deallocate( state%tr ) ! Tracers (tr(1): specific humidity in g/kg)
        if( allocated(state%phi) ) deallocate( state%phi ) ! Atmospheric geopotential
        if( allocated(state%phis) ) deallocate( state%phis ) ! Surface geopotential
        if( allocated(state%precnv) ) deallocate( state%precnv ) ! Convective precipitation  [g/(m^2 s)], total
        if( allocated(state%precls) ) deallocate( state%precls ) ! Large-scale precipitation [g/(m^2 s)], total
        if( allocated(state%snowcv) ) deallocate( state%snowcv ) ! Convective precipitation  [g/(m^2 s)], snow only
        if( allocated(state%snowls) ) deallocate( state%snowls ) ! Large-scale precipitation [g/(m^2 s)], snow only
        if( allocated(state%cbmf) ) deallocate( state%cbmf ) ! Cloud-base mass flux
        if( allocated(state%tsr) ) deallocate( state%tsr ) ! Top-of-atmosphere shortwave radiation (downward)
        if( allocated(state%ssrd) ) deallocate( state%ssrd ) ! Surface shortwave radiation (downward-only)
        if( allocated(state%ssr) ) deallocate( state%ssr ) ! Surface shortwave radiation (net downward)
        if( allocated(state%slrd) ) deallocate( state%slrd ) ! Surface longwave radiation (downward-only)
        if( allocated(state%slr) ) deallocate( state%slr ) !  Surface longwave radiation (net upward)
        if( allocated(state%olr) ) deallocate( state%olr ) ! Outgoing longwave radiation (upward)
        if( allocated(state%slru) ) deallocate( state%slru ) ! Surface longwave emission (upward)
        if( allocated(state%ustr) ) deallocate( state%ustr ) ! U-stress
        if( allocated(state%vstr) ) deallocate( state%vstr ) ! Vstress
        if( allocated(state%shf) ) deallocate( state%shf ) ! Sensible heat flux
        if( allocated(state%evap) ) deallocate( state%evap ) ! Evaporation [g/(m^2 s)]
        if( allocated(state%hfluxn) ) deallocate( state%hfluxn ) ! Net heat flux into surface
        if( allocated(state%fmask_orig) ) deallocate( state%fmask_orig ) ! Original (fractional) land-sea mask
        if( allocated(state%phi0) ) deallocate( state%phi0 ) ! Unfiltered surface geopotential
        if( allocated(state%phis0) ) deallocate( state%phis0 ) ! Spectrally-filtered surface geopotential
        if( allocated(state%alb0) ) deallocate( state%alb0 ) ! Bare-land annual-mean albedo
        if( allocated(state%hsg) ) deallocate( state%hsg ) ! Half sigma levels
        if( allocated(state%dhs) ) deallocate( state%dhs ) ! Sigma level thicknesses
        if( allocated(state%fsg) ) deallocate( state%fsg ) ! Full sigma levels
        if( allocated(state%dhsr) ) deallocate( state%dhsr ) ! 1/(2*sigma level thicknesses)
        if( allocated(state%fsgr) ) deallocate( state%fsgr ) ! akap/(2*full sigma levels)
        if( allocated(state%radang) ) deallocate( state%radang ) ! Latitudes in radians
        if( allocated(state%coriol) ) deallocate( state%coriol ) ! Coriolis parameter as a function of latitude
        if( allocated(state%sia) ) deallocate( state%sia ) ! sine(latitude)
        if( allocated(state%coa) ) deallocate( state%coa ) ! cosine(latitude)
        if( allocated(state%sia_half) ) deallocate( state%sia_half ) ! sine(latitude) over one hemisphere only
        if( allocated(state%coa_half) ) deallocate( state%coa_half ) ! cosine(latitude) over one hemisphere only
        if( allocated(state%cosg) ) deallocate( state%cosg ) ! Same as coa (TODO: remove)
        if( allocated(state%cosgr) ) deallocate( state%cosgr ) ! 1/coa
        if( allocated(state%cosgr2) ) deallocate( state%cosgr2 ) ! cosgr2   !! 1/coa^2

    end subroutine

end module