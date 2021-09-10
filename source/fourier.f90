!> author: Sam Hatfield, Fred Kucharski, Franco Molteni
!  date: 01/05/2019
!  For computing direct and inverse Fourier transforms.
module fourier
    use types, only : p
    use params
    use legendre, only : ModLegendre_t

    implicit none

    private
    public ModFourier_t, ModFourier_initialize, ModFourier_delete

    !> Fourier module object
    ! Although the ModLegendre_t is not need it, we make it a base class
    ! to allow the ModSpectral to inherit that class too when the ModFourier module
    ! is inherited. This needs to be done since Fortran does not suppoert multiple inheritance.
    type, extends(ModLegendre_t) :: ModFourier_t
        logical :: mod_fourier_initialized = .false.
        real(p), allocatable, dimension(:) :: work !! Work array required by FFTPACK. Contains trigonometric functions etc.
        integer, allocatable, dimension(:) :: ifac !! Work array required by FFTPACK. Contains prime factors
    contains
        procedure :: initialize => ModFourier_initialize
        procedure :: delete => ModFourier_delete
        procedure :: fourier => ModFourier_fourier
        procedure :: fourier_inv => ModFourier_fourier_inv

    end type

contains
    !> Initializes the ModFourier instance.
    subroutine ModFourier_initialize(this)
        use legendre, only : ModLegendre_initialize
        class(ModFourier_t), intent(inout) :: this

        call ModLegendre_initialize(this)
        if (this%mod_fourier_initialized) then
            return
        end if

        allocate(this%work(ix))
        allocate(this%ifac(15))

        call rffti1(ix, this%work, this%ifac)

    end subroutine

    !> Delete the Fourier instance
    subroutine ModFourier_delete(this)
        use legendre, only : ModLegendre_delete
        class(ModFourier_t), intent(inout) :: this

        call ModLegendre_delete(this)

        if (this%mod_fourier_initialized) then
            deallocate(this%work, this%ifac)
        end if
    end subroutine

    !> Transforms Fourier coefficients to grid-point data.
    function ModFourier_fourier_inv(this, input, kcos) result(output)
        use geometry, only : cosgr
        class(ModFourier_t), intent(in) :: this

        real(p), intent(in) :: input(2 * mx, il) !! Input field
        integer, intent(in) :: kcos           !! Scale output by cos(lat) (1) or not (0)
        real(p) :: output(ix, il)  !! Output field

        integer :: j, m
        real(p) :: fvar(ix), ch(ix)

        do j = 1, il
            fvar(1) = input(1, j)

            do m = 3, 2 * mx
                fvar(m - 1) = input(m, j)
            end do
            do m = 2 * mx, ix
                fvar(m) = 0.0
            end do

            ! Inverse FFT
            call rfftb1(ix, fvar, ch, this%work, this%ifac)

            ! Copy output into grid-point field, scaling by cos(lat) if needed
            if (kcos == 1) then
                output(:, j) = fvar
            else
                output(:, j) = fvar * cosgr(j)
            end if
        end do
    end function

    !> Transforms grid-point data to Fourier coefficients.
    function ModFourier_fourier(this, input) result(output)
        class(ModFourier_t), intent(in) :: this
        real(p), intent(in) :: input(ix, il)    !! Input field
        real(p) :: output(2 * mx, il) !! Output field

        integer :: j, m
        real(p) :: fvar(ix), scale
        real(p) :: ch(ix)

        ! Copy grid-point data into working array
        do j = 1, il
            fvar = input(:, j)

            ! Direct FFT
            call rfftf1(ix, fvar, ch, this%work, this%ifac)

            ! Copy output into spectral field, dividing by no. of long.
            scale = 1.0 / float(ix)

            ! Mean value (a(0))
            output(1, j) = fvar(1) * scale
            output(2, j) = 0.0

            do m = 3, 2 * mx
                output(m, j) = fvar(m - 1) * scale
            end do
        end do
    end function
end module
