module spectral
    use types, only : p
    use params
    use fourier, only : ModFourier_t
    use geometry, only : ModGeometry_t
    implicit none

    private
    public ModSpectral_t

    type, extends(ModFourier_t) :: ModSpectral_t
        logical :: spectral_mod_initialized_flag = .false.

        real(p), allocatable, dimension(:, :) :: el2, elm2, el4, trfilt
        real(p), allocatable, dimension(:, :) :: gradym, gradyp
        real(p), allocatable, dimension(:, :) :: uvdx, uvdym, uvdyp
        real(p), allocatable, dimension(:, :) :: vddym, vddyp
        real(p), allocatable, dimension(:) :: gradx
    contains
        procedure :: initialize => ModSpectral_initialize
        procedure :: delete => ModSpectral_delete
        procedure :: spec2grid => ModSpectral_spec2grid
        procedure :: grid2spec => ModSpectral_grid2spec
        procedure :: gradient => ModSpectral_gradient
        procedure :: vel2vort => ModSpectral_vel2vort
        procedure :: vort2vel => ModSpectral_vort2vel
        procedure :: grid_vel2vort => ModSpectral_grid_vel2vort
        procedure :: truncate => ModSpectral_truncate
        procedure :: grid_filter => ModSpectral_grid_filter
        procedure :: laplacian => ModSpectral_laplacian
        procedure :: laplacian_inv => ModSpectral_laplacian_inv

    end type ModSpectral_t

contains


    !> Initialize the spectral module instance
    subroutine ModSpectral_initialize(this, mod_geometry)
        use physical_constants, only : rearth
        use fourier, only : ModFourier_initialize
        class(ModSpectral_t), intent(inout) :: this
        class(ModGeometry_t), intent(in), target :: mod_geometry

        ! Local variables declarations
        real(p) :: el1
        integer :: m, m1, m2, n, wavenum_tot(mx, nx), mm(mx)
        call ModFourier_initialize(this, mod_geometry)

        if (this%spectral_mod_initialized_flag) then
            !Do nothing, the module is already initialized.
            return
        end if

        allocate (this%el2(mx, nx), this%el4(mx, nx))
        allocate (this%elm2(mx, nx), this%trfilt(mx, nx))

        allocate (this%gradym(mx, nx), this%gradyp(mx, nx))
        allocate (this%uvdx(mx, nx), this%uvdym(mx, nx))
        allocate (this%uvdyp(mx, nx))

        allocate (this%vddym(mx, nx), this%vddyp(mx, nx))
        allocate (this%gradx(mx))

        !  MM = zonal wavenumber = m
        !  wavenum_tot = total wavenumber of spherical harmonic = l
        !  L2 = l*(l+1)
        !  EL2 = l*(l+1)/(a**2)
        !  EL4 = EL2*EL2 ; for biharmonic diffusion
        !  ELM2 = 1./EL2
        !  TRFILT used to filter out "non-triangular" part of rhomboidal truncation
        do n = 1, nx
            do m = 1, mx
                mm(m) = m - 1
                wavenum_tot(m, n) = mm(m) + n - 1
                this%el2(m, n) = float(wavenum_tot(m, n) * (wavenum_tot(m, n) + 1)) / (rearth**2.0)
                this%el4(m, n) = this%el2(m, n)**2.0
                if (wavenum_tot(m, n) <= trunc) then
                    this%trfilt(m, n) = 1.0
                else
                    this%trfilt(m, n) = 0.0
                end if
            end do
        end do

        this%elm2(1, 1) = 0.0
        this%elm2(2:mx, :) = 1.0 / this%el2(2:mx, :)
        this%elm2(1, 2:nx) = 1.0 / this%el2(1, 2:nx)

        ! quantities required by subroutines GRAD, UVSPEC, and VDS
        ! saved in spectral
        do m = 1, mx
            do n = 1, nx
                m1 = mm(m)
                m2 = m1 + 1
                el1 = float(wavenum_tot(m, n))
                if (n == 1) then
                    this%gradx(m) = float(m1) / rearth
                    this%uvdx(m, 1) = -rearth / float(m1 + 1)
                    this%uvdym(m, 1) = 0.0
                    this%vddym(m, 1) = 0.0
                else
                    this%uvdx(m, n) = -rearth * float(m1) / (el1 * (el1 + 1))
                    this%gradym(m, n) = (el1 - 1.0) * this%epsi(m2, n) / rearth
                    this%uvdym(m, n) = -rearth * this%epsi(m2, n) / el1
                    this%vddym(m, n) = (el1 + 1) * this%epsi(m2, n) / rearth
                end if
                this%gradyp(m, n) = (el1 + 2.0) * this%epsi(m2, n + 1) / rearth
                this%uvdyp(m, n) = -rearth * this%epsi(m2, n + 1) / (el1 + 1.0)
                this%vddyp(m, n) = el1 * this%epsi(m2, n + 1) / rearth
            end do
        end do

        this%spectral_mod_initialized_flag = .true.

    end subroutine

    subroutine ModSpectral_delete(this)
        use fourier, only : ModFourier_delete
        class(ModSpectral_t), intent(inout) :: this

        if (this%spectral_mod_initialized_flag) then
            deallocate (this%el2, this%elm2, this%el4, this%trfilt)
            deallocate (this%gradym, this%gradyp, this%uvdx)
            deallocate (this%uvdym, this%uvdyp, this%vddym)
            deallocate (this%vddyp, this%gradx)
            this%spectral_mod_initialized_flag = .false.
        end if

        call ModFourier_delete(this)

    end subroutine

    subroutine ModSpectral_truncate(this, vor)
        class(ModSpectral_t), intent(in) :: this
        complex(p), intent(inout) :: vor(mx, nx)
        vor = vor * this%trfilt
    end

    function ModSpectral_laplacian(this, input) result(output)
        class(ModSpectral_t), intent(in) :: this
        complex(p), intent(in) :: input(mx, nx)
        complex(p) :: output(mx, nx)

        output = -input * this%el2
    end function

    !inverse_laplacian
    function ModSpectral_laplacian_inv(this, input) result(output)
        class(ModSpectral_t), intent(in) :: this
        complex(p), intent(in) :: input(mx, nx)
        complex(p) :: output(mx, nx)

        output = -input * this%elm2
    end function


    !> Compute the Vorticity and Divergenge from a wind field (U,V).
    ! All the fields are in the spectral space.
    subroutine ModSpectral_vel2vort(this, ucosm, vcosm, vorm, divm)
        class(ModSpectral_t), intent(in) :: this
        complex(p), dimension(mx, nx) :: ucosm, vcosm
        complex(p), dimension(mx, nx), intent(inout) :: vorm, divm
        complex(p), dimension(mx, nx) :: zc, zp

        integer :: n, m

        do n = 1, nx
            zp(:, n) = this%gradx * ucosm(:, n) * (0.0, 1.0)
            zc(:, n) = this%gradx * vcosm(:, n) * (0.0, 1.0)
        end do

        do m = 1, mx
            vorm(m, 1) = zc(m, 1) - this%vddyp(m, 1) * ucosm(m, 2)
            vorm(m, nx) = this%vddym(m, nx) * ucosm(m, trunc + 1)
            divm(m, 1) = zp(m, 1) + this%vddyp(m, 1) * vcosm(m, 2)
            divm(m, nx) = -this%vddym(m, nx) * vcosm(m, trunc + 1)
        end do

        do n = 2, trunc + 1
            do m = 1, mx
                vorm(m, n) = this%vddym(m, n) * ucosm(m, n - 1) - this%vddyp(m, n) * ucosm(m, n + 1) + zc(m, n)
                divm(m, n) = -this%vddym(m, n) * vcosm(m, n - 1) + this%vddyp(m, n) * vcosm(m, n + 1) + zp(m, n)
            end do
        end do
    end

    !> Compute the U and V winds from the Vorticity and Divergenge fields.
    ! All the fields are in the spectral space.
    subroutine ModSpectral_vort2vel(this, vorm, divm, ucosm, vcosm)
        class(ModSpectral_t), intent(in) :: this
        complex(p), dimension(mx, nx), intent(in) :: vorm, divm
        complex(p), dimension(mx, nx), intent(inout) :: ucosm, vcosm
        complex(p), dimension(mx, nx) :: zc, zp

        integer :: n, m

        zp = this%uvdx * vorm * (0.0, 1.0)
        zc = this%uvdx * divm * (0.0, 1.0)

        do m = 1, mx
            ucosm(m, 1) = zc(m, 1) - this%uvdyp(m, 1) * vorm(m, 2)
            ucosm(m, nx) = this%uvdym(m, nx) * vorm(m, trunc + 1)
            vcosm(m, 1) = zp(m, 1) + this%uvdyp(m, 1) * divm(m, 2)
            vcosm(m, nx) = -this%uvdym(m, nx) * divm(m, trunc + 1)
        end do

        do n = 2, trunc + 1
            do m = 1, mx
                vcosm(m, n) = -this%uvdym(m, n) * divm(m, n - 1) + this%uvdyp(m, n) * divm(m, n + 1) + zp(m, n)
                ucosm(m, n) = this%uvdym(m, n) * vorm(m, n - 1) - this%uvdyp(m, n) * vorm(m, n + 1) + zc(m, n)
            end do
        end do
    end

    !> Convert u and v in the grid space to Vorticity and Divergence
    ! in the spectral space.
    subroutine ModSpectral_grid_vel2vort(this, ug, vg, vorm, divm, kcos)
        class(ModSpectral_t), intent(in) :: this

        real(p), intent(in) :: ug(ix, il), vg(ix, il)
        complex(p), intent(out) :: vorm(mx, nx), divm(mx, nx)
        integer, intent(in) :: kcos

        integer :: i, j
        real(p) :: ug1(ix, il), vg1(ix, il)
        complex(p) :: specu(mx, nx), specv(mx, nx)

        if (kcos == 2) then
            do j = 1, il
                do i = 1, ix
                    ug1(i, j) = ug(i, j) * this%mod_geometry%cosgr(j)
                    vg1(i, j) = vg(i, j) * this%mod_geometry%cosgr(j)
                end do
            end do
        else
            do j = 1, il
                do i = 1, ix
                    ug1(i, j) = ug(i, j) * this%mod_geometry%cosgr2(j)
                    vg1(i, j) = vg(i, j) * this%mod_geometry%cosgr2(j)
                end do
            end do
        end if

        specu = this%grid2spec(ug1)
        specv = this%grid2spec(vg1)
        call this%vel2vort(specu, specv, vorm, divm)
    end


    function ModSpectral_spec2grid(this, vorm, kcos) result(vorg)
        class(ModSpectral_t), intent(in) :: this
        complex(p), intent(in) :: vorm(mx, nx)
        integer, intent(in) :: kcos

        real(p) :: vorg(ix, il)
        real(p) :: vorm_r(2 * mx, nx)

        vorm_r = reshape(transfer(vorm, vorm_r), (/ 2 * mx, nx /))
        vorg = this%fourier_inv(this%legendre_inv(vorm_r), kcos)
    end function

    function ModSpectral_grid2spec(this, vorg) result(vorm)
        class(ModSpectral_t), intent(in) :: this
        real(p), intent(in) :: vorg(ix, il)

        ! Local vars
        complex(p) :: vorm(mx, nx)
        real(p) :: vorm_r(2 * mx, nx)

        vorm_r = this%legendre(this%fourier(vorg))
        vorm = reshape(transfer(vorm_r, vorm), (/ mx, nx /))
    end function

    subroutine ModSpectral_gradient(this, psi, psdx, psdy)
        class(ModSpectral_t), intent(in) :: this
        complex(p), dimension(mx, nx), intent(inout) :: psi
        complex(p), dimension(mx, nx), intent(inout) :: psdx, psdy

        integer :: n, m

        do n = 1, nx
            psdx(:, n) = this%gradx * psi(:, n) * (0.0, 1.0)
        end do

        do m = 1, mx
            psdy(m, 1) = this%gradyp(m, 1) * psi(m, 2)
            psdy(m, nx) = -this%gradym(m, nx) * psi(m, trunc + 1)
        end do

        do n = 2, trunc + 1
            do m = 1, mx
                psdy(m, n) = -this%gradym(m, n) * psi(m, n - 1) + this%gradyp(m, n) * psi(m, n + 1)
            end do
        end do
    end

    !> Compute a spectrally-filtered grid-point field.
    subroutine ModSpectral_grid_filter(this, fg1, fg2)
        class(ModSpectral_t), intent(in) :: this
        real(p), intent(inout) :: fg1(ix, il) !! Original grid-point field
        real(p), intent(inout) :: fg2(ix, il) !! Filtered grid-point field

        complex(p) :: fsp(mx, nx)
        integer :: n, m, total_wavenumber

        fsp = this%grid2spec(fg1)

        do n = 1, nx
            do m = 1, mx
                total_wavenumber = m + n - 2
                if (total_wavenumber > trunc) fsp(m, n) = (0.0, 0.0)
            end do
        end do

        fg2 = this%spec2grid(fsp, 1)
    end subroutine
end module
