!> author: Sam Hatfield
!  date: 29/04/2019
!  For computing stochastically perturbed parametrization tendency (SPPT)
!  patterns.
!
!  To be used as multiplicative noise applied to physical tendencies. SPPT is
!  a parametrization of model error.
!  See ECMWF Tech. Memo. #598 (Palmer et al. 2009).
module sppt
    use types, only : p
    use params

    implicit none

    private
    public mu, gen_sppt

    !> Array for tapering value of SPPT in the different layers of the
    !  atmosphere. A value of 1 means the tendency is not tapered at that level
    real(p), parameter, dimension(kx) :: mu = (/1, 1, 1, 1, 1, 1, 1, 1/)

    ! TODO: Move random seed initialization to the main intialization routine.
    !> Flag for controlling first-use behaviour
    logical :: first = .true.

    !> Decorrelation time of SPPT perturbation (in hours)
    real(p), parameter :: time_decorr = 6.0

    !> Time autocorrelation of spectral AR(1) signals
    real(p), parameter :: phi = exp(-(24 / real(nsteps, p)) / time_decorr)

    !> Correlation length scale of SPPT perturbation (in metres)
    real(p), parameter :: len_decorr = 500000.0

    !> Standard deviation of SPPT perturbation (in grid point space)
    real(p), parameter :: stddev = 0.33

contains
    !> Generate grid point space SPPT pattern distribution.
    function gen_sppt(mod_spectral) result(sppt_grid)
        use spectral, only : ModSpectral_t
        use physical_constants, only : rearth
        use legendre, only : ModLegendre_t
        class(ModSpectral_t), intent(in) :: mod_spectral

        real(p), allocatable :: sppt_grid(:, :, :) !! The generated grid point pattern

        complex(p), allocatable :: sppt_spec(:, :, :) ! SPPT pattern in spectral space

        !> Total wavenumber-wise standard deviation of spectral signals
        real(p), allocatable :: sigma(:, :, :)
        integer :: m, n, k
        complex(p), allocatable :: eta(:, :, :)
        real(p) :: f0, randreal, randimag

        ! Allocate variables
        allocate(sppt_grid(ix, il, kx))
        allocate(sigma(ix, il, kx))
        allocate(eta(mx, nx, kx))
        allocate(sppt_spec(mx, nx, kx))

        ! TODO: This should be done at the model initialization
        ! Seed RNG if first use of SPPT
        if (first) call time_seed()

        ! Generate Gaussian noise
        do m = 1, mx
            do n = 1, nx
                do k = 1, kx
                    randreal = randn(0.0_p, 1.0_p)
                    randimag = randn(0.0_p, 1.0_p)

                    ! Clip noise to +- 10 standard deviations
                    eta(m, n, k) = cmplx(&
                            min(10.0_p, abs(randreal)) * sign(1.0_p, randreal), &
                            min(10.0_p, abs(randimag)) * sign(1.0_p, randimag), p)
                end do
            end do
        end do

        ! If first timestep
        if (first) then
            ! Generate spatial amplitude pattern and time correlation
            f0 = sum((/((2 * n + 1) * exp(-0.5 * (len_decorr / rearth)**2 * n * (n + 1)), n = 1, trunc)/))
            f0 = sqrt((stddev**2 * (1 - phi**2)) / (2 * f0))

            do k = 1, kx
                sigma(:, :, k) = f0 * exp(-0.25 * len_decorr**2 * mod_spectral%el2)
            end do

            ! First AR(1) step
            sppt_spec = (1 - phi**2)**(-0.5) * sigma * eta

            first = .false.
        else
            ! Subsequent AR(1) steps
            sppt_spec = phi * sppt_spec + sigma * eta
        end if

        ! Convert to grid point space
        do k = 1, kx
            sppt_grid(:, :, k) = mod_spectral%spec2grid(sppt_spec(:, :, k), 1)
        end do

        ! Clip to +/- 1.0
        sppt_grid = min(1.0_p, abs(sppt_grid)) * sign(1.0_p, sppt_grid)
        deallocate(sppt_grid)
        deallocate(sigma)
        deallocate(eta)
        deallocate(sppt_spec)
    end function

    !> Generates a random number drawn for the specified normal distribution.
    function randn(mean, stdev)
        real(p), intent(in) :: mean  !! The mean of the distribution to draw from
        real(p), intent(in) :: stdev !! The standard deviation of the distribution to draw from
        real(p) :: randn !! The generated random number

        real(p) :: u, v
        real(p) :: rand(2)

        call random_number(rand)

        ! Box-Muller method
        u = (-2.0 * log(rand(1)))**0.5
        v = 2.0 * 6.28318530718 * rand(2)
        randn = mean + stdev * u * sin(v)
    end function

    ! TODO: How to make this initialization by thread?
    !> Seeds RNG from system clock.
    subroutine time_seed()
        integer :: i, n, clock
        integer, allocatable :: seed(:)

        call random_seed(size = n)
        allocate (seed(n))

        call system_clock(count = clock)

        seed = clock + 37 * (/(i - 1, i = 1, n)/)
        call random_seed(put = seed)

        deallocate (seed)
    end subroutine
end module
