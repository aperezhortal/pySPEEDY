!> author: Sam Hatfield, Fred Kucharski, Franco Molteni
!  date: 01/05/2019
!  For checking model diagnostics in case of numerical instability.
module diagnostics
    use types, only : p

    implicit none

    private
    public check_diagnostics

contains
    !> Compute the lobal mean of eddy kinetic energy and temperature.
    !  Stops the integration if the computed diagnostics are outside of
    !  allowable ranges.
    subroutine check_diagnostics(vor, div, t, istep, nstdia, mod_spectral)
        use params
        use spectral, only : ModSpectral_t

        complex(p), dimension(mx, nx, kx), intent(in) :: vor    !! Spectral vorticity
        complex(p), dimension(mx, nx, kx), intent(in) :: div    !! Spectral divergence
        complex(p), dimension(mx, nx, kx), intent(in) :: t      !! Spectral temperature
        integer, intent(in) :: istep  !! Current time step
        class(ModSpectral_t), intent(in) :: mod_spectral

        integer, intent(in) :: nstdia ! Period (number of steps) for diagnostic print

        integer :: k, m, n, kk
        complex(p) :: temp(mx, nx)
        real(p) :: diag(kx, 3)

        ! 1. Get global-mean temperature and compute eddy kinetic energy
        do k = 1, kx
            diag(k, 1) = 0.0
            diag(k, 2) = 0.0
            diag(k, 3) = sqrt(0.5) * real(t(1, 1, k), p)

            temp = mod_spectral%laplacian_inv(vor(:, :, k))

            do m = 2, mx
                do n = 1, nx
                    diag(k, 1) = diag(k, 1) - real(temp(m, n) * conjg(vor(m, n, k)), p)
                end do
            end do

            temp = mod_spectral%laplacian_inv(div(:, :, k))

            do m = 2, mx
                do n = 1, nx
                    diag(k, 2) = diag(k, 2) - real(temp(m, n) * conjg(div(m, n, k)), p)
                end do
            end do
        end do

        ! 3. Stop integration if model variables are out of range
        do k = 1, kx
            if ((diag(k, 1) > 500.0) &
                    .or. (diag(k, 2) > 500.0) &
                    .or. (diag(k, 3) < 180.0) &
                    .or. (diag(k, 3) > 320.0)) then

                !print 2001, istep, (diag(kk, 1), kk=1, kx)
                !print 2002, (diag(kk, 2), kk=1, kx)
                !print 2003, (diag(kk, 3), kk=1, kx)
                write(0, *) "Model variables out of accepted range"
                write(0, *) "step =", istep
                stop
            end if
        end do

    end subroutine
end module
