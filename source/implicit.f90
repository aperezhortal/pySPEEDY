!> author: Sam Hatfield, Fred Kucharski, Franco Molteni
!  date: 08/05/2019
!  For initializing and performing implicit computations.
module implicit
    use types, only : p
    use params
    use horizontal_diffusion, only : ModHorizontalDiffusion_t

    implicit none

    private
    public ModImplicit_t

    type, extends(ModHorizontalDiffusion_t) :: ModImplicit_t
        logical :: mod_implicit_initialize_flag = .false.

        real(p), allocatable, dimension(:) :: tref, tref2, tref3, dhsx
        real(p), allocatable, dimension(:, :) :: xc, xd, elz
        real(p), allocatable, dimension(:, :, :) :: xj
    contains
        procedure :: initialize => ModImplicit_initialize
        procedure :: set_time_step => ModImplicit_set_time_step
        procedure :: delete => ModImplicit_delete
        procedure :: implicit_terms => ModImplicit_implicit_terms
    end type ModImplicit_t

contains

    !> Initialize constants for implicit computation of horizontal diffusion and
    !  gravity waves.
    !
    !  Initialize_implicit initializes constants for the implicit gravity wave computation.
    !  It is assumed that that all implicit steps are of length 2*delt and use
    !  the forward/backward parameter alph. initialize_implicit has to be re-called
    !  whenever either of these two parameters is changed. initialize_implicit should
    !  be called even if the explicit option is chosen for the gravity wave
    !  terms (the reference state temperature tref is subtracted from some
    !  terms anyway to reduce roundoff error; also the constants needed for
    !  the biharmonic diffusion, which is assumed always to be backwards
    !  implicit, are defined in initialize_implicit).
    subroutine ModImplicit_initialize(this)
        use dynamical_constants, only : gamma
        use physical_constants, only : akap, rgas, grav
        use geometry, only : fsg, fsgr
        use horizontal_diffusion, only : ModHorizontalDiffusion_initialize
        use matrix_inversion, only : inv

        class(ModImplicit_t), intent(inout) :: this

        integer :: k
        real(p) :: rgam

        ! Initialize horizontal diffusion module
        call ModHorizontalDiffusion_initialize(this)

        if (this%mod_implicit_initialize_flag) then
            return
        end if

        ! Add to the state
        allocate(this%tref(kx), this%tref2(kx), this%tref3(kx), this%dhsx(kx))
        allocate(this%xc(kx, kx), this%xd(kx, kx))
        allocate(this%xj(kx, kx, mx + nx + 1))
        allocate(this%elz(mx, nx))

        ! 1. Constants for implicit gravity wave computation
        ! reference atmosphere, function of sigma only
        rgam = rgas * gamma / (1000. * grav)

        do k = 1, kx
            this%tref(k) = 288. * max(0.2, fsg(k))**rgam
            this%tref2(k) = akap * this%tref(k)
            this%tref3(k) = fsgr(k) * this%tref(k)
        end do

        this%mod_implicit_initialize_flag = .true.

    end subroutine

    subroutine ModImplicit_set_time_step(this, dt)
        use dynamical_constants, only : gamma
        use physical_constants, only : akap, rgas, grav, rearth
        use geometry, only : hsg, dhs, fsg, fsgr
        use horizontal_diffusion, only : ModHorizontalDiffusion_initialize
        use matrix_inversion, only : inv

        class(ModImplicit_t), intent(inout) :: this
        real(p), intent(in) :: dt !! Time step

        integer :: m, n, k, k1, k2, l
        real(p) :: rgam, xi, xxi, xxx

        real(p), allocatable, dimension(:) :: dsum
        integer, allocatable, dimension(:) :: indx
        real(p), allocatable, dimension(:, :) :: xa, xb, xe, ya
        real(p), allocatable, dimension(:, :, :) :: xf

        allocate(dsum(kx), indx(kx))
        allocate(xa(kx, kx), xb(kx, kx), xe(kx, kx), ya(kx, kx))
        allocate(xf(kx, kx, mx + nx + 1))

        !~~~~~~~~~~~~~~
        ! 1. Constants for backwards implicit biharmonic diffusion
        do m = 1, mx
            do n = 1, nx
                this%dmp1 (m, n) = 1. / (1. + this%dmp (m, n) * dt)
                this%dmp1d(m, n) = 1. / (1. + this%dmpd(m, n) * dt)
                this%dmp1s(m, n) = 1. / (1. + this%dmps(m, n) * dt)
            end do
        end do

        ! Other constants
        xi = dt * alph
        xxi = xi / (rearth * rearth)

        this%dhsx = xi * dhs

        do n = 1, nx
            do m = 1, mx
                this%elz(m, n) = float(m + n - 2) * float(m + n - 1) * xxi
            end do
        end do

        !T(K) = TEX(K)+YA(K,K')*D(K') + XA(K,K')*SIG(K')

        xa(:kx, :kx - 1) = 0.0

        do k = 1, kx
            do k1 = 1, kx
                ya(k, k1) = -akap * this%tref(k) * dhs(k1)
            end do
        end do

        do k = 2, kx
            xa(k, k - 1) = 0.5 * (akap * this%tref(k) / fsg(k) - (this%tref(k) - this%tref(k - 1)) / dhs(k))
        end do

        do k = 1, kx - 1
            xa(k, k) = 0.5 * (akap * this%tref(k) / fsg(k) - (this%tref(k + 1) - this%tref(k)) / dhs(k))
        end do

        !sig(k)=xb(k,k')*d(k')
        dsum(1) = dhs(1)
        do k = 2, kx
            dsum(k) = dsum(k - 1) + dhs(k)
        end do

        do k = 1, kx - 1
            do k1 = 1, kx
                xb(k, k1) = dhs(k1) * dsum(k)
                if(k1<=k) xb(k, k1) = xb(k, k1) - dhs(k1)
            end do
        end do

        !t(k)=tex(k)+this%xc(k,k')*d(k')
        do k = 1, kx
            do k1 = 1, kx
                this%xc(k, k1) = ya(k, k1)
                do k2 = 1, kx - 1
                    this%xc(k, k1) = this%xc(k, k1) + xa(k, k2) * xb(k2, k1)
                end do
            end do
        end do

        !P(K)=XD(K,K')*T(K')
        this%xd = 0.0

        do k = 1, kx
            do k1 = k + 1, kx
                this%xd(k, k1) = rgas * log(hsg(k1 + 1) / hsg(k1))
            end do
        end do
        do k = 1, kx
            this%xd(k, k) = rgas * log(hsg(k + 1) / fsg(k))
        end do

        !P(K)=YE(K)+XE(K,K')*D(K')
        do k = 1, kx
            do k1 = 1, kx
                xe(k, k1) = 0.
                do k2 = 1, kx
                    xe(k, k1) = xe(k, k1) + this%xd(k, k2) * this%xc(k2, k1)
                end do
            end do
        end do

        do l = 1, mx + nx + 1
            xxx = (float(l) * float(l + 1)) / (rearth * rearth)
            do k = 1, kx
                do k1 = 1, kx
                    xf(k, k1, l) = xi * xi * xxx * (rgas * this%tref(k) * dhs(k1) - xe(k, k1))
                end do
            end do
            do k = 1, kx
                xf(k, k, l) = xf(k, k, l) + 1.
            end do
        end do

        do l = 1, mx + nx + 1
            call inv(xf(1, 1, l), this%xj(1, 1, l), indx, kx)
        end do

        do k = 1, kx
            do k1 = 1, kx
                this%xc(k, k1) = this%xc(k, k1) * xi
            end do
        end do

        deallocate(xa, xb, xe, xf, dsum, indx, ya)

    end subroutine

    subroutine ModImplicit_delete(this)
        use horizontal_diffusion, only : ModHorizontalDiffusion_delete

        class(ModImplicit_t), intent(inout) :: this

        if (this%mod_implicit_initialize_flag) then
            deallocate(this%tref, this%tref2, this%tref3)
            deallocate(this%xc, this%xd, this%xj, this%elz)
            this%mod_implicit_initialize_flag = .false.
            call ModHorizontalDiffusion_delete(this)
        end if
    end subroutine

    !> Correct tendencies for implicit gravity wave model
    subroutine ModImplicit_implicit_terms(this, divdt, tdt, psdt)
        use physical_constants, only : rgas
        class(ModImplicit_t), intent(inout) :: this
        complex(p), intent(inout) :: divdt(mx, nx, kx) !! Divergence tendency
        complex(p), intent(inout) :: tdt(mx, nx, kx)   !! Temperature tendency
        complex(p), intent(inout) :: psdt(mx, nx)     !! log(surface pressure) tendency

        complex(p), allocatable, dimension(:, :, :) :: ye, yf
        integer :: k1, k, m, n

        allocate(ye(mx, nx, kx), yf(mx, nx, kx))

        ye(:, :, :) = (0.0, 0.0)

        do k1 = 1, kx
            do k = 1, kx
                ye(:, :, k) = ye(:, :, k) + this%xd(k, k1) * tdt(:, :, k1)
            end do
        end do

        do k = 1, kx
            ye(:, :, k) = ye(:, :, k) + rgas * this%tref(k) * psdt
        end do

        do k = 1, kx
            do m = 1, mx
                do n = 1, nx
                    yf(m, n, k) = divdt(m, n, k) + this%elz(m, n) * ye(m, n, k)
                end do
            end do
        end do

        divdt(:, :, :) = (0.0, 0.0)

        do n = 1, nx
            do m = 1, mx
                if((m + n - 2) /= 0) then
                    do k1 = 1, kx
                        divdt(m, n, :) = divdt(m, n, :) + this%xj(:, k1, m + n - 2) * yf(m, n, k1)
                    end do
                endif
            end do
        end do

        do k = 1, kx
            psdt = psdt - divdt(:, :, k) * this%dhsx(k)
        end do

        do k = 1, kx
            do k1 = 1, kx
                tdt(:, :, k) = tdt(:, :, k) + this%xc(k, k1) * divdt(:, :, k1)
            end do
        end do

        deallocate(ye, yf)
    end
end module
