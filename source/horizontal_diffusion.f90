!> author: Sam Hatfield, Fred Kucharski, Franco Molteni
!  date: 07/05/2019
!  For performing horizontal diffusion.
module horizontal_diffusion
    use types, only : p
    use params

    implicit none

    private
    public ModHorizontalDiffusion_t
    public ModHorizontalDiffusion_initialize, ModHorizontalDiffusion_delete
    public do_horizontal_diffusion

    type ModHorizontalDiffusion_t
        real(p), allocatable :: dmp(:, :)  !! Damping coefficient for temperature and vorticity (explicit)
        real(p), allocatable :: dmpd(:, :) !! Damping coefficient for divergence (explicit)
        real(p), allocatable :: dmps(:, :) !! Damping coefficient for extra diffusion in the stratosphere (explicit)

        real(p), allocatable :: dmp1(:, :)  !! Damping coefficient for temperature and vorticity (implicit)
        real(p), allocatable :: dmp1d(:, :) !! Damping coefficient for divergence (implicit)
        real(p), allocatable :: dmp1s(:, :) !! Damping coefficient for extra diffusion in the stratosphere (implicit)

        real(p), allocatable :: tcorv(:) !! Vertical component of orographic correction for temperature
        real(p), allocatable :: qcorv(:) !! Vertical component of orographic correction for humidity

        complex(p), allocatable :: tcorh(:, :) !! Horizontal component of orographic correction for temperature
        complex(p), allocatable :: qcorh(:, :) !! Horizontal component of orographic correction for humidity

        logical :: mod_diffusion_initialized = .false.
    contains
        procedure :: initialize => ModHorizontalDiffusion_initialize
        procedure :: delete => ModHorizontalDiffusion_delete
        !        procedure :: do_horizontal_diffusion => do_horizontal_diffusion
    end type ModHorizontalDiffusion_t

    interface do_horizontal_diffusion
        module procedure do_horizontal_diffusion_2d
        module procedure do_horizontal_diffusion_3d
    end interface

contains
    !> Initializes the arrays used for horizontal diffusion.
    subroutine ModHorizontalDiffusion_initialize(this)
        use dynamical_constants, only : thd, thdd, thds, gamma, hscale, hshum
        use physical_constants, only : grav, rgas
        use geometry, only : fsg

        class(ModHorizontalDiffusion_t), intent(inout) :: this

        integer :: j, k

        integer, parameter :: npowhd = 4 ! Power of Laplacian in horizontal diffusion

        real(p) :: elap, elapn, hdifd, hdiff, hdifs, qexp, rgam, rlap, twn

        if (this%mod_diffusion_initialized) then
            return
        end if

        allocate (this%dmp(mx, nx), this%dmpd(mx, nx), this%dmps(mx, nx))
        allocate (this%dmp1(mx, nx), this%dmp1d(mx, nx), this%dmp1s(mx, nx))
        allocate (this%tcorv(kx), this%qcorv(kx))
        allocate (this%tcorh(mx, nx))
        allocate (this%qcorh(mx, nx))

        ! Coefficients for horizontal diffusion
        ! Spectral damping coefficients
        hdiff = 1. / (thd * 3600.)
        hdifd = 1. / (thdd * 3600.)
        hdifs = 1. / (thds * 3600.)
        rlap = 1. / float(trunc * (trunc + 1))

        do j = 1, nx
            do k = 1, mx
                twn = float(k + j - 2)
                elap = (twn * (twn + 1.) * rlap)
                elapn = elap**npowhd
                this%dmp(k, j) = hdiff * elapn
                this%dmpd(k, j) = hdifd * elapn
                this%dmps(k, j) = hdifs * elap
            end do
        end do

        ! 5.2 Orographic correction terms for temperature and humidity
        !     (vertical component)
        rgam = rgas * gamma / (1000. * grav)
        qexp = hscale / hshum

        this%tcorv(1) = 0.
        this%qcorv(1) = 0.
        this%qcorv(2) = 0.

        do k = 2, kx
            this%tcorv(k) = fsg(k)**rgam
            if (k > 2) this%qcorv(k) = fsg(k)**qexp
        end do

        this%mod_diffusion_initialized = .true.
    end subroutine

    ! Deallocate the module data to release memory.
    subroutine ModHorizontalDiffusion_delete(this)
        class(ModHorizontalDiffusion_t), intent(inout) :: this

        if (this%mod_diffusion_initialized) then
            deallocate (this%dmp, this%dmpd, this%dmps)
            deallocate (this%dmp1, this%dmp1d, this%dmp1s)
            deallocate (this%tcorv, this%qcorv, this%tcorh, this%qcorh)
            this%mod_diffusion_initialized = .false.
        end if

    end subroutine

    !> Adds horizontal diffusion tendency of field to spectral tendency fdt
    !  using damping coefficients dmp and dmp1.
    function do_horizontal_diffusion_2d(field, fdt_in, dmp_in, dmp1_in) result(fdt_out)
        complex(p), intent(in) :: field(mx, nx), fdt_in(mx, nx)
        complex(p) :: fdt_out(mx, nx)
        real(p), intent(in) :: dmp_in(mx, nx), dmp1_in(mx, nx)

        fdt_out = (fdt_in - dmp_in * field) * dmp1_in
    end function

    !> Adds horizontal diffusion tendency of field to spectral tendency fdt
    !  at all model levels using damping coefficients dmp and dmp1.
    function do_horizontal_diffusion_3d(field, fdt_in, dmp_in, dmp1_in) result(fdt_out)
        complex(p), intent(in) :: field(mx, nx, kx), fdt_in(mx, nx, kx)
        complex(p) :: fdt_out(mx, nx, kx)
        real(p), intent(in) :: dmp_in(mx, nx), dmp1_in(mx, nx)
        integer :: k

        do k = 1, kx
            fdt_out(:, :, k) = do_horizontal_diffusion_2d(field(:, :, k), &
                    fdt_in(:, :, k), &
                    dmp_in, dmp1_in)
        end do
    end function
end module
