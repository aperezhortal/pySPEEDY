module spectral
    use types, only : p
    use params

    implicit none

    private
    public el2
    public initialize_spectral, deinitialize_spectral
    public laplacian, inverse_laplacian, ModLegendre_spec2grid, ModLegendre_grid2spec
    public ModLegendre_spectral_truncation
    public grad, vds, uvspec, vdspec, trunct

    ! Make them allocatable to avoid declaring them statically.
    ! This suppresses some warnings in the compiler.
    real(p), allocatable, dimension(:, :), save :: el2, elm2, el4, trfilt
    real(p), allocatable, dimension(:, :), save :: gradym, gradyp
    real(p), allocatable, dimension(:, :), save :: uvdx, uvdym, uvdyp
    real(p), allocatable, dimension(:, :), save :: vddym, vddyp
    real(p), allocatable, dimension(:), save :: gradx

    logical, save :: spectral_mod_initialized_flag = .false.
contains
    ! Initialize spectral transforms
    subroutine initialize_spectral(state)
        use physical_constants, only : rearth
        use fourier, only : initialize_fourier
        use legendre, only : ModLegendre_initialize
        use model_state, only : ModelState_t, ModLegendre_t
        type(ModelState_t), intent(inout), target :: state

        ! Local variables declarations
        real(p) :: el1
        integer :: m, m1, m2, n, wavenum_tot(mx, nx), mm(mx)
        type(ModLegendre_t), pointer :: legendre_mod

        legendre_mod => state%legendre_mod

        call ModLegendre_initialize(state%legendre_mod)

        if (spectral_mod_initialized_flag) then
            !Do nothing, the module is already initialized.
            return
        end if

        if (.not. allocated(el2)) allocate (el2(mx, nx))
        if (.not. allocated(elm2)) allocate (elm2(mx, nx))
        if (.not. allocated(el4)) allocate (el4(mx, nx))
        if (.not. allocated(trfilt)) allocate (trfilt(mx, nx))

        if (.not. allocated(gradym)) allocate (gradym(mx, nx))
        if (.not. allocated(gradyp)) allocate (gradyp(mx, nx))

        if (.not. allocated(uvdx)) allocate (uvdx(mx, nx))
        if (.not. allocated(uvdym)) allocate (uvdym(mx, nx))
        if (.not. allocated(uvdyp)) allocate (uvdyp(mx, nx))

        if (.not. allocated(vddym)) allocate (vddym(mx, nx))
        if (.not. allocated(vddyp)) allocate (vddyp(mx, nx))

        if (.not. allocated(gradx)) allocate (gradx(mx))

        ! Initialize Fourier transforms
        call initialize_fourier

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
                el2(m, n) = float(wavenum_tot(m, n) * (wavenum_tot(m, n) + 1)) / (rearth**2.0)
                el4(m, n) = el2(m, n)**2.0
                if (wavenum_tot(m, n) <= trunc) then
                    trfilt(m, n) = 1.0
                else
                    trfilt(m, n) = 0.0
                end if
            end do
        end do

        elm2(1, 1) = 0.0
        elm2(2:mx, :) = 1.0 / el2(2:mx, :)
        elm2(1, 2:nx) = 1.0 / el2(1, 2:nx)

        ! quantities required by subroutines GRAD, UVSPEC, and VDS
        ! saved in spectral
        do m = 1, mx
            do n = 1, nx
                m1 = mm(m)
                m2 = m1 + 1
                el1 = float(wavenum_tot(m, n))
                if (n == 1) then
                    gradx(m) = float(m1) / rearth
                    uvdx(m, 1) = -rearth / float(m1 + 1)
                    uvdym(m, 1) = 0.0
                    vddym(m, 1) = 0.0
                else
                    uvdx(m, n) = -rearth * float(m1) / (el1 * (el1 + 1))
                    gradym(m, n) = (el1 - 1.0) * legendre_mod%epsi(m2, n) / rearth
                    uvdym(m, n) = -rearth * legendre_mod%epsi(m2, n) / el1
                    vddym(m, n) = (el1 + 1) * legendre_mod%epsi(m2, n) / rearth
                end if
                gradyp(m, n) = (el1 + 2.0) * legendre_mod%epsi(m2, n + 1) / rearth
                uvdyp(m, n) = -rearth * legendre_mod%epsi(m2, n + 1) / (el1 + 1.0)
                vddyp(m, n) = el1 * legendre_mod%epsi(m2, n + 1) / rearth
            end do
        end do

        spectral_mod_initialized_flag = .true.

    end subroutine

    ! Deinitialize global variables
    subroutine deinitialize_spectral
        if (allocated(el2)) deallocate (el2)
        if (allocated(elm2)) deallocate (elm2)
        if (allocated(el4)) deallocate (el4)
        if (allocated(trfilt)) deallocate (trfilt)

        if (allocated(gradym)) deallocate (gradym)
        if (allocated(gradyp)) deallocate (gradyp)

        if (allocated(uvdx)) deallocate (uvdx)
        if (allocated(uvdym)) deallocate (uvdym)
        if (allocated(uvdyp)) deallocate (uvdyp)

        if (allocated(vddym)) deallocate (vddym)
        if (allocated(vddyp)) deallocate (vddyp)

        if (allocated(gradx)) deallocate (gradx)

        spectral_mod_initialized_flag = .false.
    end subroutine

    function laplacian(input) result(output)
        complex(p), intent(in) :: input(mx, nx)
        complex(p) :: output(mx, nx)

        output = -input * el2
    end function

    function inverse_laplacian(input) result(output)
        complex(p), intent(in) :: input(mx, nx)
        complex(p) :: output(mx, nx)

        output = -input * elm2
    end function

    function ModLegendre_spec2grid(legendre_mod, vorm, kcos) result(vorg)
        use legendre, only : ModLegendre_inverse
        use fourier, only : fourier_inv
        use model_state, only : ModLegendre_t

        type(ModLegendre_t), intent(in) :: legendre_mod
        complex(p), intent(in) :: vorm(mx, nx)
        integer, intent(in) :: kcos

        real(p) :: vorg(ix, il)
        real(p) :: vorm_r(2 * mx, nx)

        vorm_r = reshape(transfer(vorm, vorm_r), (/ 2 * mx, nx /))
        vorg = fourier_inv(ModLegendre_inverse(legendre_mod,vorm_r), kcos)
    end function

    function ModLegendre_grid2spec(legendre_mod, vorg) result(vorm)
        use legendre, only : ModLegendre_legendre
        use fourier, only : fourier_dir
        use model_state, only : ModLegendre_t

        type(ModLegendre_t), intent(in) :: legendre_mod
        real(p), intent(in) :: vorg(ix, il)

        ! Local vars
        complex(p) :: vorm(mx, nx)
        real(p) :: vorm_r(2 * mx, nx)

        vorm_r = ModLegendre_legendre(legendre_mod,fourier_dir(vorg))
        vorm = reshape(transfer(vorm_r, vorm), (/ mx, nx /))
    end function

    subroutine grad(psi, psdx, psdy)
        complex(p), dimension(mx, nx), intent(inout) :: psi
        complex(p), dimension(mx, nx), intent(inout) :: psdx, psdy

        integer :: n, m

        do n = 1, nx
            psdx(:, n) = gradx * psi(:, n) * (0.0, 1.0)
        end do

        do m = 1, mx
            psdy(m, 1) = gradyp(m, 1) * psi(m, 2)
            psdy(m, nx) = -gradym(m, nx) * psi(m, trunc + 1)
        end do

        do n = 2, trunc + 1
            do m = 1, mx
                psdy(m, n) = -gradym(m, n) * psi(m, n - 1) + gradyp(m, n) * psi(m, n + 1)
            end do
        end do
    end

    subroutine vds(ucosm, vcosm, vorm, divm)
        complex(p), dimension(mx, nx) :: ucosm, vcosm
        complex(p), dimension(mx, nx), intent(inout) :: vorm, divm
        complex(p), dimension(mx, nx) :: zc, zp

        integer :: n, m

        do n = 1, nx
            zp(:, n) = gradx * ucosm(:, n) * (0.0, 1.0)
            zc(:, n) = gradx * vcosm(:, n) * (0.0, 1.0)
        end do

        do m = 1, mx
            vorm(m, 1) = zc(m, 1) - vddyp(m, 1) * ucosm(m, 2)
            vorm(m, nx) = vddym(m, nx) * ucosm(m, trunc + 1)
            divm(m, 1) = zp(m, 1) + vddyp(m, 1) * vcosm(m, 2)
            divm(m, nx) = -vddym(m, nx) * vcosm(m, trunc + 1)
        end do

        do n = 2, trunc + 1
            do m = 1, mx
                vorm(m, n) = vddym(m, n) * ucosm(m, n - 1) - vddyp(m, n) * ucosm(m, n + 1) + zc(m, n)
                divm(m, n) = -vddym(m, n) * vcosm(m, n - 1) + vddyp(m, n) * vcosm(m, n + 1) + zp(m, n)
            end do
        end do
    end

    subroutine uvspec(vorm, divm, ucosm, vcosm)
        complex(p), dimension(mx, nx), intent(in) :: vorm, divm
        complex(p), dimension(mx, nx), intent(inout) :: ucosm, vcosm
        complex(p), dimension(mx, nx) :: zc, zp

        integer :: n, m

        zp = uvdx * vorm * (0.0, 1.0)
        zc = uvdx * divm * (0.0, 1.0)

        do m = 1, mx
            ucosm(m, 1) = zc(m, 1) - uvdyp(m, 1) * vorm(m, 2)
            ucosm(m, nx) = uvdym(m, nx) * vorm(m, trunc + 1)
            vcosm(m, 1) = zp(m, 1) + uvdyp(m, 1) * divm(m, 2)
            vcosm(m, nx) = -uvdym(m, nx) * divm(m, trunc + 1)
        end do

        do n = 2, trunc + 1
            do m = 1, mx
                vcosm(m, n) = -uvdym(m, n) * divm(m, n - 1) + uvdyp(m, n) * divm(m, n + 1) + zp(m, n)
                ucosm(m, n) = uvdym(m, n) * vorm(m, n - 1) - uvdyp(m, n) * vorm(m, n + 1) + zc(m, n)
            end do
        end do
    end

    subroutine vdspec(ug, vg, vorm, divm, kcos, legendre_mod)
        use model_state, only : ModLegendre_t
        use geometry, only : cosgr, cosgr2
        real(p), intent(in) :: ug(ix, il), vg(ix, il)
        complex(p), intent(out) :: vorm(mx, nx), divm(mx, nx)
        integer, intent(in) :: kcos
        type(ModLegendre_t), intent(in) :: legendre_mod

        integer :: i, j
        real(p) :: ug1(ix, il), vg1(ix, il)
        complex(p) :: specu(mx, nx), specv(mx, nx)

        if (kcos.eq.2) then
            do j = 1, il
                do i = 1, ix
                    ug1(i, j) = ug(i, j) * cosgr(j)
                    vg1(i, j) = vg(i, j) * cosgr(j)
                end do
            end do
        else
            do j = 1, il
                do i = 1, ix
                    ug1(i, j) = ug(i, j) * cosgr2(j)
                    vg1(i, j) = vg(i, j) * cosgr2(j)
                end do
            end do
        end if

        specu = ModLegendre_grid2spec(legendre_mod, ug1)
        specv = ModLegendre_grid2spec(legendre_mod, vg1)
        call vds(specu, specv, vorm, divm)
    end

    subroutine trunct(vor)
        complex(p), intent(inout) :: vor(mx, nx)

        vor = vor * trfilt
    end

    !> Compute a spectrally-filtered grid-point field.
    subroutine ModLegendre_spectral_truncation(legendre_mod, fg1, fg2)
        use model_state, only : ModLegendre_t
        type(ModLegendre_t), intent(in) :: legendre_mod
        real(p), intent(inout) :: fg1(ix, il) !! Original grid-point field
        real(p), intent(inout) :: fg2(ix, il) !! Filtered grid-point field

        complex(p) :: fsp(mx, nx)
        integer :: n, m, total_wavenumber

        fsp = ModLegendre_grid2spec(legendre_mod, fg1)

        do n = 1, nx
            do m = 1, mx
                total_wavenumber = m + n - 2
                if (total_wavenumber > trunc) fsp(m, n) = (0.0, 0.0)
            end do
        end do

        fg2 = ModLegendre_spec2grid(legendre_mod, fsp, 1)
    end subroutine
end module
