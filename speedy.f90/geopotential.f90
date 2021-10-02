!> author: Sam Hatfield, Fred Kucharski, Franco Molteni
!  date: 07/05/2019
!  For performing geopotential calculations.
module geopotential
    use types, only : p
    use params
    use geometry, only : ModGeometry_t

    implicit none

    private
    public initialize_geopotential, set_geopotential

contains
    !> Initializes the arrays used for geopotential calculations
    subroutine initialize_geopotential(state)
        use physical_constants, only : rgas
        use model_state, only : ModelState_t
        type(ModelState_t), intent(inout), target :: state

        integer :: k
        class(ModGeometry_t), pointer :: mod_geometry
        mod_geometry => state%mod_geometry

        ! Coefficients to compute geopotential
        do k = 1, kx
            state%xgeop1(k) = rgas * log(mod_geometry%hsg(k + 1) / mod_geometry%fsg(k))
            if (k /= kx) state%xgeop2(k + 1) = rgas * log(mod_geometry%fsg(k + 1) / mod_geometry%hsg(k + 1))
        end do

    end subroutine

    !> Computes spectral geopotential from spectral temperature T and spectral
    ! topography phis, as in GFDL Climate Group GCM.
    ! The result is updated in the state%phi variable.
    subroutine set_geopotential(state, time_level)
        use physical_constants, only : rgas
        use model_state, only : ModelState_t

        type(ModelState_t), intent(inout) :: state
        integer, intent(in) :: time_level

        state%phi = get_geopotential(state%t(:, :, :, time_level), &
                state%phis, state%xgeop1, state%xgeop2, &
                state%mod_geometry%hsg, state%mod_geometry%fsg)

    end subroutine set_geopotential

    function get_geopotential(t, phis, xgeop1, xgeop2, hsg, fsg) result(phi)

        complex(p), intent(in) :: t(mx, nx, kx) !! Spectral temperature
        complex(p), intent(in) :: phis(mx, nx) !! Spectral surface geopotential
        real(p), intent(in) :: xgeop1(kx) !! Constants for hydrostatic equation
        real(p), intent(in) :: xgeop2(kx) !! Constants for hydrostatic equation
        real(p), intent(in) :: hsg(kx + 1) !! Half sigma levels
        real(p), intent(in) :: fsg(kx)   !! Full sigma levels

        complex(p) :: phi(mx, nx, kx)  !! Spectral geopotential

        integer :: k
        real(p) :: corf

        ! 1. Bottom layer (integration over half a layer)
        phi(:, :, kx) = phis + xgeop1(kx) * t(:, :, kx)

        ! 2. Other layers (integration two half-layers)
        do k = kx - 1, 1, -1
            phi(:, :, k) = phi(:, :, k + 1) + xgeop2(k + 1) * t(:, :, k + 1)&
                    & + xgeop1(k) * t(:, :, k)
        end do

        ! 3. lapse-rate correction in the free troposphere
        do k = 2, kx - 1
            corf = xgeop1(k) * 0.5 * log(hsg(k + 1) / fsg(k)) / log(fsg(k + 1) / fsg(k - 1))
            phi(1, :, k) = phi(1, :, k) + corf * (t(1, :, k + 1) - t(1, :, k - 1))
        end do
    end function
end module
