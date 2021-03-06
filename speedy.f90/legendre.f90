!> author: Sam Hatfield, Fred Kucharski, Franco Molteni
!  date: 09/05/2019
!  For computing direct and inverse Legendre transforms.
module legendre
    use types, only : p
    use params
    use geometry, only : ModGeometry_t
    implicit none

    private
    public ModLegendre_t, ModLegendre_initialize, ModLegendre_delete

    !> Legendre module variables
    type ModLegendre_t
        logical :: mod_legendre_initialized = .false.
        real(8), allocatable, dimension(:, :) :: epsi ! Epsilon function used for various spectral calculations
        real(8), allocatable, dimension(:, :, :) :: cpol ! The Legendre polynomials
        real(8), allocatable, dimension(:, :) :: repsi ! 1/legendre_epsi
        integer, allocatable, dimension(:) :: nsh2 ! Used for defining shape of spectral triangle
        real(8), allocatable, dimension(:) :: wt ! Gaussian weights used for integration in direct Legendre transform

        ! We keep a pointer to a ModGeometry_t instance
        class(ModGeometry_t), pointer :: mod_geometry => null() !! Spectral module instance
        logical :: mod_geometry_initialized = .false.

    contains
        procedure :: initialize => ModLegendre_initialize
        procedure :: legendre => ModLegendre_legendre
        procedure :: legendre_inv => ModLegendre_legendre_inv
        procedure :: delete => ModLegendre_delete
        procedure :: legendre_poly => ModLegendre_legendre_poly
    end type


contains
    !> Initializes Legendre transforms and constants used for other subroutines
    !  that manipulate spherical harmonics.
    subroutine ModLegendre_initialize(this, mod_geometry)
        use physical_constants, only : rearth
        class(ModLegendre_t), intent(inout) :: this
        class(ModGeometry_t), intent(in), target :: mod_geometry

        real(p) :: emm2, ell2
        real(p), allocatable :: poly(:, :)
        integer :: j, n, m, m1, m2
        integer, allocatable :: mm(:), wavenum_tot(:, :)

        ! First lets initialize the pointer to the geometry module
        this%mod_geometry => mod_geometry
        if (.not. this%mod_geometry%mod_geometry_initialized) then
            call this%mod_geometry%initialize()
        end if

        allocate(poly(mx, nx))
        allocate(mm(mx), wavenum_tot(mx, nx))

        ! Allocate internal arrays
        allocate(this%cpol(2 * mx, nx, iy))  !! The Legendre polynomials
        allocate(this%epsi(mx + 1, nx + 1))   !! Epsilon function used for various spectral calculations
        allocate(this%repsi(mx + 1, nx + 1))  !! 1/epsi
        allocate(this%nsh2(nx))       !! Used for defining shape of spectral triangle
        allocate(this%wt(iy)) !! Gaussian weights used for integration in direct Legendre transform

        ! First compute Gaussian latitudes and weights at the IY points from
        ! pole to equator
        this%wt = get_weights()

        do n = 1, nx
            this%nsh2(n) = 0
            do m = 1, mx
                mm(m) = m - 1
                wavenum_tot(m, n) = mm(m) + n - 1
                if (wavenum_tot(m, n) <= (trunc + 1) .or. ix /= 4 * iy) then
                    this%nsh2(n) = this%nsh2(n) + 2
                end if
            end do
        end do

        do m = 1, mx + 1
            do n = 1, nx + 1
                emm2 = float(m - 1)**2
                ell2 = float(n + m - 2)**2
                if (n == (nx + 1)) then
                    this%epsi(m, n) = 0.0
                else if(n == 1 .and. m == 1) then
                    this%epsi(m, n) = 0.0
                else
                    this%epsi(m, n) = sqrt((ell2 - emm2) / (4.0 * ell2 - 1.0))
                end if
                this%repsi(m, n) = 0.0
                if (this%epsi(m, n) > 0.) then
                    this%repsi(m, n) = 1.0 / this%epsi(m, n)
                end if
            end do
        end do

        ! Generate associated Legendre polynomials
        do j = 1, iy
            poly = this%legendre_poly(j)
            do n = 1, nx
                do m = 1, mx
                    m1 = 2 * m - 1
                    m2 = 2 * m
                    this%cpol(m1, n, j) = poly(m, n)
                    this%cpol(m2, n, j) = poly(m, n)
                end do
            end do
        end do

        deallocate(poly, mm, wavenum_tot)
        this%mod_legendre_initialized = .true.
    end subroutine

    subroutine ModLegendre_delete(this)
        class(ModLegendre_t), intent(inout) :: this
        if (this%mod_legendre_initialized) then
            deallocate(this%cpol, this%epsi)
            deallocate(this%repsi, this%nsh2, this%wt)
            this%mod_legendre_initialized = .false.

            ! Clean the pointer to the geometry module
            this%mod_geometry => null()
            ! IMPORTANT: The geometry module is not deleted here!
        end if
    end subroutine

    !> Computes inverse Legendre transformation.
    ! The Legendre polynomials (cpol) and the triangular shape definition (nsh2)
    ! needs to be initialized and passed to the function.
    function ModLegendre_legendre_inv(this, input) result(output)
        class(ModLegendre_t), intent(in) :: this
        ! 2*mx because these arrays actually represent complex variables
        real(p), intent(in) :: input(2 * mx, nx)  !! Input field

        real(p) :: output(2 * mx, il) !! Output field

        real(p) :: even(2 * mx), odd(2 * mx)
        integer :: j, j1, m, n

        ! Loop over Northern Hemisphere, computing odd and even decomposition of
        ! incoming field
        do j = 1, iy
            j1 = il + 1 - j

            ! Initialise arrays
            even = 0.0
            odd = 0.0

            ! Compute even decomposition
            do n = 1, nx, 2
                do m = 1, this%nsh2(n)
                    even(m) = even(m) + input(m, n) * this%cpol(m, n, j)
                end do
            end do

            ! Compute odd decomposition
            do n = 2, nx, 2
                do m = 1, this%nsh2(n)
                    odd(m) = odd(m) + input(m, n) * this%cpol(m, n, j)
                end do
            end do

            ! Compute Southern Hemisphere
            output(:, j1) = even + odd

            ! Compute Northern Hemisphere
            output(:, j) = even - odd
        end do
    end function

    !> Computes direct Legendre transformation.
    ! The Legendre polynomials (cpol), the triangular shape definition (nsh2),
    ! and the gaussian weights (wt) used for the integration of the Legendre transform
    ! needs to be initialized and passed to the function.
    function ModLegendre_legendre(this, input) result(output)
        class(ModLegendre_t), intent(in) :: this
        ! 2*mx because these arrays actually represent complex variables
        real(p), intent(in) :: input(2 * mx, il)  !! Input field
        real(p) :: output(2 * mx, nx) !! Output field

        real(p), allocatable :: even(:, :), odd(:, :)
        integer :: j, j1, m, n

        allocate(even(2 * mx, iy), odd(2 * mx, iy))

        ! Initialise output array
        output = 0.0

        ! Loop over Northern Hemisphere, computing odd and even decomposition of
        ! incoming field. The Legendre weights (wt) are applied here
        do j = 1, iy
            ! Corresponding Southern Hemisphere latitude
            j1 = il + 1 - j

            even(:, j) = (input(:, j1) + input(:, j)) * this%wt(j)
            odd(:, j) = (input(:, j1) - input(:, j)) * this%wt(j)
        end do

        ! The parity of an associated Legendre polynomial is the same
        ! as the parity of n' - m'. n', m' are the actual total wavenumber and zonal
        ! wavenumber, n and m are the indices used for SPEEDY's spectral packing.
        ! m' = m - 1 and n' = m + n - 2, therefore n' - m' = n - 1

        ! Loop over coefficients corresponding to even associated Legendre
        ! polynomials
        do n = 1, trunc + 1, 2
            do m = 1, this%nsh2(n)
                output(m, n) = dot_product(this%cpol(m, n, :iy), even(m, :iy))
            end do
        end do

        ! Loop over coefficients corresponding to odd associated Legendre
        ! polynomials
        do n = 2, trunc + 1, 2
            do m = 1, this%nsh2(n)
                output(m, n) = dot_product(this%cpol(m, n, :iy), odd(m, :iy))
            end do
        end do

        deallocate(even, odd)
    end function

    !> Compute Gaussian weights for direct Legendre transform
    function get_weights() result(w)
        ! A slightly modified version of a program in Numerical Recipes
        ! (Cambridge Univ. Press, 1989).

        real(p) :: w(iy) !! Weights in gaussian quadrature (sum should equal 1.0)

        real(p) :: z, z1, p1, p2, p3, pp = 0
        integer :: n, j, i

        n = 2 * iy

        z1 = 2.0

        do i = 1, iy
            z = cos(3.141592654_p * (real(i, p) - 0.25_p) / (real(n, p) + 0.5_p))

            do while (abs(z - z1) > epsilon(real(1.0, p)))
                p1 = 1.0_p
                p2 = 0.0_p

                do j = 1, n
                    p3 = p2
                    p2 = p1
                    p1 = ((2.0_p * real(j, p) - 1.0_p) * z * p2 - (real(j, p) - 1.0_p) * p3) / j
                end do

                pp = real(n, p) * (z * p1 - p2) / (z**2.0_p - 1.0_p)
                z1 = z
                z = z1 - p1 / pp
            end do

            w(i) = 2.0 / ((1.0 - z**2.0) * pp**2.0)
        end do
    end function

    !> Compute associated Legendre polynomials at given latitude.
    function ModLegendre_legendre_poly(this, j) result(poly)
        class(ModLegendre_t), intent(in) :: this

        integer, intent(in) :: j  !! The latitude index to compute the polynomials at
        real(p) :: poly(mx, nx)  !! The Legendre polynomials

        real(p), parameter :: small = 1.e-30
        real(p) :: x, y
        integer :: m, n
        real(p), allocatable :: alp(:, :), consq(:)

        allocate(alp(mx + 1, nx), consq(mx))

        y = this%mod_geometry%coa_half(j)
        x = this%mod_geometry%sia_half(j)

        do m = 1, mx
            consq(m) = sqrt(0.5 * (2.0 * float(m) + 1.0) / float(m))
        end do

        ! start recursion with N=1 (M=L) diagonal
        alp(1, 1) = sqrt(0.5)
        do m = 2, mx + 1
            alp(m, 1) = consq(m - 1) * y * alp(m - 1, 1)
        end do

        ! continue with other elements
        do m = 1, mx + 1
            alp(m, 2) = (x * alp(m, 1)) * this%repsi(m, 2)
        end do

        do n = 3, nx
            do m = 1, mx + 1
                alp(m, n) = (x * alp(m, n - 1) - this%epsi(m, n - 1) * alp(m, n - 2)) * this%repsi(m, n)
            end do
        end do

        ! zero polynomials with absolute values smaller than 10**(-30)
        do n = 1, nx
            do m = 1, mx + 1
                if(abs(alp(m, n)) <= small) alp(m, n) = 0.0
            end do
        end do

        ! pick off the required polynomials
        poly = alp(1:mx, 1:nx)
        deallocate(alp, consq)
    end function
end module
