!> author: Sam Hatfield, Fred Kucharski, Franco Molteni
!  date: 01/05/2019
!  For checking model diagnostics in case of numerical instability.
module diagnostics
    use types, only : p

    implicit none

    private
    public check_diagnostics

contains
    !> Compute the global mean of eddy kinetic energy and temperature.
    !  Stops the integration if the computed diagnostics are outside of
    !  allowable ranges.
    subroutine check_diagnostics(state, time_lev, error_code)
        use params
        use spectral, only : ModSpectral_t
        use model_state, only : ModelState_t
        use spectral, only : ModSpectral_t
        use error_codes, only : SUCCESS, E_DIAGNOSTICS_OUTSIDE_RANGE

        type(ModelState_t), intent(inout), target :: state
        integer, intent(in) :: time_lev  !! Time level (1 or 2)
        integer, intent(out) :: error_code ! 0: No error, 1: Error.

        integer :: k, m, n
        complex(p) :: temp(mx, nx)
        real(p) :: diag(kx, 3)

        class(ModSpectral_t), pointer :: mod_spectral

        mod_spectral => state%mod_spectral
        error_code = SUCCESS

        ! 1. Get global-mean temperature and compute eddy kinetic energy
        do k = 1, kx
            diag(k, 1) = 0.0
            diag(k, 2) = 0.0
            diag(k, 3) = sqrt(0.5) * real(state%t(1, 1, k, time_lev), p)

            temp = mod_spectral%laplacian_inv(state%vor(:, :, k, time_lev))

            do m = 2, mx
                do n = 1, nx
                    diag(k, 1) = diag(k, 1) - real(temp(m, n) * conjg(state%vor(m, n, k, time_lev)), p)
                end do
            end do

            temp = mod_spectral%laplacian_inv(state%div(:, :, k, time_lev))

            do m = 2, mx
                do n = 1, nx
                    diag(k, 2) = diag(k, 2) - real(temp(m, n) * conjg(state%div(m, n, k, time_lev)), p)
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
                write(0, *) "step =", state%current_step
                error_code = E_DIAGNOSTICS_OUTSIDE_RANGE
                return
            end if
        end do

    end subroutine
end module
