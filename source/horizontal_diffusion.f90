!> author: Sam Hatfield, Fred Kucharski, Franco Molteni
!  date: 07/05/2019
!  For performing horizontal diffusion.
module horizontal_diffusion
    use types, only: p
    use params

    implicit none

    private
    public initialize_horizontal_diffusion, deinitialize_horizontal_diffusion 
    public do_horizontal_diffusion
    public dmp, dmpd, dmps, dmp1, dmp1d, dmp1s, tcorv, qcorv, tcorh, qcorh

    interface do_horizontal_diffusion
        module procedure do_horizontal_diffusion_2d
        module procedure do_horizontal_diffusion_3d
    end interface

    ! Make this variables allocatable and initialize them only once.
    real(p), save, allocatable :: dmp(:, :)  !! Damping coefficient for temperature and vorticity (explicit)
    real(p), save, allocatable :: dmpd(:, :) !! Damping coefficient for divergence (explicit)
    real(p), save, allocatable :: dmps(:, :) !! Damping coefficient for extra diffusion in the stratosphere (explicit)

    real(p), save, allocatable :: dmp1(:, :)  !! Damping coefficient for temperature and vorticity (implicit)
    real(p), save, allocatable :: dmp1d(:, :) !! Damping coefficient for divergence (implicit)
    real(p), save, allocatable :: dmp1s(:, :) !! Damping coefficient for extra diffusion in the stratosphere
                                              !! (implicit)

    real(p), save, allocatable :: tcorv(:) !! Vertical component of orographic correction for temperature
    real(p), save, allocatable :: qcorv(:) !! Vertical component of orographic correction for humidity

    complex(p), save, allocatable :: tcorh(:, :) !! Horizontal component of orographic correction for temperature
    complex(p), save, allocatable :: qcorh(:, :) !! Horizontal component of orographic correction for humidity

    logical, save :: horizontal_diffusion_mod_initialized_flag = .false.

contains
    !> Initializes the arrays used for horizontal diffusion.
    subroutine initialize_horizontal_diffusion
        use dynamical_constants, only: thd, thdd, thds, gamma, hscale, hshum
        use physical_constants, only: grav, rgas
        use geometry, only: fsg

        integer :: j, k, npowhd
        real(p) :: elap, elapn, hdifd, hdiff, hdifs, qexp, rgam, rlap, twn

        if (horizontal_diffusion_mod_initialized_flag) then
            return
        end if

        if (.not. allocated(dmp)) allocate (dmp(mx, nx))
        if (.not. allocated(dmpd)) allocate (dmpd(mx, nx))
        if (.not. allocated(dmps)) allocate (dmps(mx, nx))
        if (.not. allocated(dmp1)) allocate (dmp1(mx, nx))
        if (.not. allocated(dmp1d)) allocate (dmp1d(mx, nx))
        if (.not. allocated(dmp1s)) allocate (dmp1s(mx, nx))
        if (.not. allocated(dmp1d)) allocate (dmp1d(mx, nx))

        if (.not. allocated(tcorv)) allocate (tcorv(kx))
        if (.not. allocated(qcorv)) allocate (qcorv(kx))

        if (.not. allocated(tcorh)) allocate (tcorh(mx, nx))
        if (.not. allocated(qcorh)) allocate (qcorh(mx, nx))

        ! 1. Definition of constants
        if (mod(nsteps, 2) /= 0) stop ' Invalid no. of time steps'

        ! Power of Laplacian in horizontal diffusion
        npowhd = 4

        ! Coefficients for horizontal diffusion
        ! Spectral damping coefficients
        hdiff = 1./(thd*3600.)
        hdifd = 1./(thdd*3600.)
        hdifs = 1./(thds*3600.)
        rlap = 1./float(trunc*(trunc + 1))

        do j = 1, nx
            do k = 1, mx
                twn = float(k + j - 2)
                elap = (twn*(twn + 1.)*rlap)
                elapn = elap**npowhd
                dmp(k, j) = hdiff*elapn
                dmpd(k, j) = hdifd*elapn
                dmps(k, j) = hdifs*elap
            end do
            ! dmps(1,j)=0.
        end do

        ! 5.2 Orographic correction terms for temperature and humidity
        !     (vertical component)
        rgam = rgas*gamma/(1000.*grav)
        qexp = hscale/hshum

        tcorv(1) = 0.
        qcorv(1) = 0.
        qcorv(2) = 0.

        do k = 2, kx
            tcorv(k) = fsg(k)**rgam
            if (k .gt. 2) qcorv(k) = fsg(k)**qexp
        end do

        horizontal_diffusion_mod_initialized_flag = .true.
    end subroutine

    ! Deallocate the module data to release memory.
    subroutine deinitialize_horizontal_diffusion()
        if ( allocated(dmp)) deallocate (dmp)
        if ( allocated(dmpd)) deallocate (dmpd)
        if ( allocated(dmps)) deallocate (dmps)
        if ( allocated(dmp1)) deallocate (dmp1)
        if ( allocated(dmp1d)) deallocate (dmp1d)
        if ( allocated(dmp1s)) deallocate (dmp1s)
        if ( allocated(dmp1d)) deallocate (dmp1d)

        if ( allocated(tcorv)) deallocate (tcorv)
        if ( allocated(qcorv)) deallocate (qcorv)

        if ( allocated(tcorh)) deallocate (tcorh)
        if ( allocated(qcorh)) deallocate (qcorh)
        horizontal_diffusion_mod_initialized_flag = .false.
    end subroutine

    !> Adds horizontal diffusion tendency of field to spectral tendency fdt
    !  using damping coefficients dmp and dmp1.
    function do_horizontal_diffusion_2d(field, fdt_in, dmp_in, dmp1_in) result(fdt_out)
        complex(p), intent(in) :: field(mx, nx), fdt_in(mx, nx)
        complex(p) :: fdt_out(mx, nx)
        real(p), intent(in) :: dmp_in(mx, nx), dmp1_in(mx, nx)

        fdt_out = (fdt_in - dmp_in*field)*dmp1_in
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
