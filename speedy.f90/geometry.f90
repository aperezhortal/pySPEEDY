!> author: Sam Hatfield, Fred Kucharski, Franco Molteni
!  date: 01/05/2019
!  For storing all variables related to the model's grid space.

module geometry
    use types, only : p
    use params

    implicit none

    private
    public ModGeometry_t

    !> Geometry module object
    type ModGeometry_t
        logical :: mod_geometry_initialized = .false.

        ! Vertical level parameters
        real(p), allocatable, dimension(:) :: hsg !! Half sigma levels. Dimension(kx + 1).
        real(p), allocatable, dimension(:) :: dhs   !! Sigma level thicknesses. Dimension(kx).
        real(p), allocatable, dimension(:) :: fsg   !! Full sigma levels. Dimension(kx).
        real(p), allocatable, dimension(:) :: dhsr  !! 1/(2*sigma level thicknesses). Dimension(kx).
        real(p), allocatable, dimension(:) :: fsgr  !! akap/(2*full sigma levels). Dimension(kx).

        ! Functions of latitude and longitude
        real(p), allocatable, dimension(:) :: radang   !! Latitudes in radians. Dimension(il).
        real(p), allocatable, dimension(:) :: coriol   !! Coriolis parameter as a function of latitude. Dimension(il).
        real(p), allocatable, dimension(:) :: sia      !! sine(latitude). Dimension(il).
        real(p), allocatable, dimension(:) :: coa      !! cosine(latitude). Dimension(il).
        real(p), allocatable, dimension(:) :: sia_half !! sine(latitude) over one hemisphere only. Dimension(iy).
        real(p), allocatable, dimension(:) :: coa_half !! cosine(latitude) over one hemisphere only. Dimension(il).
        real(p), allocatable, dimension(:) :: cosg     !! Same as coa (TODO: remove). Dimension(il).
        real(p), allocatable, dimension(:) :: cosgr    !! 1/coa. Dimension(il).
        real(p), allocatable, dimension(:) :: cosgr2   !! 1/coa^2. Dimension(il).

        ! Functions of sigma and latitude (initial. in INPHYS)
        real(p), allocatable, dimension(:) :: sigl   !! Logarithm of full-level sigma. Dimension(kx).
        real(p), allocatable, dimension(:) :: sigh   !! Half-level sigma. Dimension(0:kx).

        !> g/(d_sigma p0) : to convert fluxes of u,v,q into d(u,v,q)/dt .
        !! Dimension(kx).
        real(p), allocatable, dimension(:) :: grdsig

        !> g/(d_sigma p0 c_p): to convert energy fluxes into dT/dt
        !! Dimension(kx).
        real(p), allocatable, dimension(:) :: grdscp

        !> Weights for vertical interpolation. Dimension(kx, 2).
        real(p), allocatable, dimension(:, :) :: wvi

    contains
        procedure :: initialize => ModGeometry_initialize
        procedure :: delete => ModGeometry_delete
        !        procedure :: fourier => ModFourier_fourier
        !        procedure :: fourier_inv => ModFourier_fourier_inv
    end type


contains
    !> Initializes all of the model geometry variables.
    subroutine ModGeometry_initialize(this)
        use physical_constants, only : akap, omega, grav, cp, p0
        class(ModGeometry_t), intent(inout) :: this

        integer j, jj, k

        if (this%mod_geometry_initialized) then
            !Do nothing, the module is already initialized.
            return
        end if

        ! Allocate variables -----------------------------------------------
        allocate(this%hsg(kx + 1))
        allocate(this%dhs(kx), this%fsg(kx), this%dhsr(kx), this%fsgr(kx))
        allocate(this%radang(il), this%coriol(il), this%sia(il), this%coa(il))
        allocate(this%sia_half(iy), this%coa_half(il))
        allocate(this%cosg(il), this%cosgr(il), this%cosgr2(il))
        allocate(this%sigl(kx), this%sigh(0:kx))
        allocate(this%grdsig(kx), this%grdscp(kx), this%wvi(kx, 2))
        ! End allocation ---------------------------------------------------

        ! Definition of model levels
        ! Half (vertical velocity) levels
        if (kx == 5) then
            this%hsg(:6) = (/0.000, 0.150, 0.350, 0.650, 0.900, 1.000/)
        else if (kx == 7) then
            this%hsg(:8) = (/0.020, 0.140, 0.260, 0.420, 0.600, 0.770, 0.900, 1.000/)
        else if (kx == 8) then
            this%hsg(:9) = (/0.000, 0.050, 0.140, 0.260, 0.420, 0.600, 0.770, 0.900, 1.000/)
        end if

        ! Layer thicknesses and full (u,v,T) levels
        do k = 1, kx
            this%dhs(k) = this%hsg(k + 1) - this%hsg(k)
            this%fsg(k) = 0.5 * (this%hsg(k + 1) + this%hsg(k))
        end do

        ! Additional functions of sigma
        do k = 1, kx
            this%dhsr(k) = 0.5 / this%dhs(k)
            this%fsgr(k) = akap / (2. * this%fsg(k))
        end do

        ! Horizontal functions

        ! Latitudes and functions of latitude
        ! NB: J=1 is Southernmost point!
        do j = 1, iy
            jj = il + 1 - j
            this%sia_half(j) = cos(3.141592654 * (j - 0.25) / (il + 0.5))
            this%coa_half(j) = sqrt(1.0 - this%sia_half(j)**2.0)
            this%sia(j) = -this%sia_half(j)
            this%sia(jj) = this%sia_half(j)
            this%coa(j) = this%coa_half(j)
            this%coa(jj) = this%coa_half(j)
            this%radang(j) = -asin(this%sia_half(j))
            this%radang(jj) = asin(this%sia_half(j))
        end do

        ! Expand cosine and its reciprocal to cover both hemispheres
        do j = 1, iy
            jj = il + 1 - j
            this%cosg(j) = this%coa_half(j)
            this%cosg(jj) = this%coa_half(j)
            this%cosgr(j) = 1. / this%coa_half(j)
            this%cosgr(jj) = 1. / this%coa_half(j)
            this%cosgr2(j) = 1. / (this%coa_half(j) * this%coa_half(j))
            this%cosgr2(jj) = 1. / (this%coa_half(j) * this%coa_half(j))
        end do

        this%coriol = 2.0 * omega * this%sia

        ! 1.2 Functions of sigma and latitude
        this%sigh(0) = this%hsg(1)

        do k = 1, kx
            this%sigl(k) = log(this%fsg(k))
            this%sigh(k) = this%hsg(k + 1)
            this%grdsig(k) = grav / (this%dhs(k) * p0)
            this%grdscp(k) = this%grdsig(k) / cp
        end do

        ! Weights for vertical interpolation at half-levels(1,kx) and surface
        ! Note that for phys.par. half-lev(k) is between full-lev k and k+1
        ! Fhalf(k) = Ffull(k)+this%wvi(K,2)*(Ffull(k+1)-Ffull(k))
        ! Fsurf = Ffull(kx)+this%wvi(kx,2)*(Ffull(kx)-Ffull(kx-1))
        do k = 1, kx - 1
            this%wvi(k, 1) = 1. / (this%sigl(k + 1) - this%sigl(k))
            this%wvi(k, 2) = (log(this%sigh(k)) - this%sigl(k)) * this%wvi(k, 1)
        end do

        this%wvi(kx, 1) = 0.
        this%wvi(kx, 2) = (log(0.99) - this%sigl(kx)) * this%wvi(kx - 1, 1)

        this%mod_geometry_initialized = .true.
    end subroutine

    !> Delete the Geometry instance
    subroutine ModGeometry_delete(this)
        class(ModGeometry_t), intent(inout) :: this

        if (this%mod_geometry_initialized) then
            ! Allocate variables -----------------------------------------------
            deallocate(this%hsg, this%dhs, this%fsg, this%dhsr, this%fsgr)
            deallocate(this%radang, this%coriol, this%sia, this%coa)
            deallocate(this%sia_half, this%coa_half, this%cosg, this%cosgr, this%cosgr2)
            deallocate(this%sigl, this%sigh, this%grdsig, this%grdscp, this%wvi)
            ! End allocation ---------------------------------------------------
            this%mod_geometry_initialized = .false.
        end if
    end subroutine
end module
