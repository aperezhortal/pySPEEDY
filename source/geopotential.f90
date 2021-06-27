!> author: Sam Hatfield, Fred Kucharski, Franco Molteni
!  date: 07/05/2019
!  For performing geopotential calculations.
module geopotential
    use types, only: p
    use params

    implicit none

    private
    public initialize_geopotential, get_geopotential

    real(p) :: xgeop1(kx) !! Constants for hydrostatic equation
    real(p) :: xgeop2(kx) !! Constants for hydrostatic equation

contains
    !> Initializes the arrays used for geopotential calculations
    subroutine initialize_geopotential(state)
        use physical_constants, only: rgas
        use model_state, only: ModelState_t

        type(ModelState_t), intent(inout) :: state

        integer :: k

        ! Coefficients to compute geopotential
        do k = 1, kx
            xgeop1(k) = rgas*log(state%hsg(k + 1)/state%fsg(k))
            if (k /= kx) xgeop2(k + 1) = rgas*log(state%fsg(k + 1)/state%hsg(k + 1))
        end do
    end subroutine

    !> Computes spectral geopotential from spectral temperature T and spectral
    !  topography phis, as in GFDL Climate Group GCM.
    function get_geopotential(temperature_3d, state) result(phi_out)
        use model_state, only: ModelState_t

        type(ModelState_t), intent(inout) :: state
        complex(p), intent(in) :: temperature_3d(mx, nx, kx) !! Spectral temperature
        complex(p) :: phi_out(mx, nx, kx)           !! Spectral geopotential

        integer :: k
        real(p) :: corf

        ! 1. Bottom layer (integration over half a layer)
        phi_out(:, :, kx) = state%phis + xgeop1(kx)*temperature_3d(:, :, kx)

        ! 2. Other layers (integration two half-layers)
        do k = kx - 1, 1, -1
            phi_out(:, :, k) = phi_out(:, :, k + 1) + xgeop2(k + 1)*temperature_3d(:, :, k + 1)&
                & + xgeop1(k)*temperature_3d(:, :, k)
        end do

        ! 3. lapse-rate correction in the free troposphere
        do k = 2, kx - 1
            corf = xgeop1(k)*0.5*log(state%hsg(k + 1)/state%fsg(k))/log(state%fsg(k + 1)/state%fsg(k - 1))
            phi_out(1, :, k) = phi_out(1, :, k) &
                           + corf*(temperature_3d(1, :, k + 1) - temperature_3d(1, :, k - 1))
        end do
    end function
end module
