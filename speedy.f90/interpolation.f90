!> author: Sam Hatfield, Fred Kucharski, Franco Molteni
!  date: 09/05/2019
!  For interpolating fields in time.
module interpolation
    use types, only: p
    use params, only: ix, il

    implicit none

    private
    public forint, forin5, monthly_interp

contains

    !> Performs linear interpolation on the time dimension.
    ! This function does not wrap the indexes around the month 1 or 12.
    subroutine monthly_interp(month_idx, in_field, out_field, month_fraction, n_months)
        integer, intent(in) :: month_idx                    !! The month
        integer, intent(in) :: n_months
        real(p), intent(in) :: in_field(ix, il, 0:n_months-1) !! The input field
        real(p), intent(inout) :: out_field(ix, il)   !! The output field
        real(p), intent(in)    :: month_fraction      !! The fraction of the current month elapsed

        integer :: imon2
        real(p) :: wmon

        if (month_fraction <= 0.5) then
            imon2 = month_idx - 1
            wmon = 0.5 - month_fraction
        else
            imon2 = month_idx + 1
            wmon = month_fraction - 0.5
        end if

        out_field = in_field(:, :, month_idx) &
                    + wmon*(in_field(:, :, imon2) - in_field(:, :, month_idx))
    end subroutine

    !> Performs linear interpolation of monthly-mean forcing fields.
    subroutine forint(imon, for12, for1, tmonth)
        integer, intent(in) :: imon            !! The month
        real(p), intent(in) :: for12(ix*il, *) !! The input field
        real(p), intent(inout) :: for1(ix*il)  !! The output field
        real(p), intent(in)    :: tmonth       !! The fraction of the current month elapsed
        integer :: imon2
        real(p) :: wmon

        if (tmonth <= 0.5) then
            imon2 = imon - 1
            if (imon == 1) imon2 = 12
            wmon = 0.5 - tmonth
        else
            imon2 = imon + 1
            if (imon == 12) imon2 = 1
            wmon = tmonth - 0.5
        end if

        for1 = for12(:, imon) + wmon*(for12(:, imon2) - for12(:, imon))
    end subroutine

    !> Performs nonlinear, mean-conserving interpolation of monthly-mean forcing fields.
    subroutine forin5(imon, for12, for1, tmonth)
        integer, intent(in) :: imon         !! The month
        real(p), intent(in) :: for12(ix*il, 12) !! The input field
        real(p), intent(inout) :: for1(ix*il)  !! The output field
        real(p), intent(in)    :: tmonth      !! The fraction of the current month elapsed

        integer :: im1, im2, ip1, ip2
        real(p) :: c0, t0, t1, t2, wm1, wm2, w0, wp1, wp2

        im2 = imon - 2
        im1 = imon - 1
        ip1 = imon + 1
        ip2 = imon + 2

        if (im2 < 1) im2 = im2 + 12
        if (im1 < 1) im1 = im1 + 12
        if (ip1 > 12) ip1 = ip1 - 12
        if (ip2 > 12) ip2 = ip2 - 12

        c0 = 1.0/12.0
        t0 = c0*tmonth
        t1 = c0*(1.0 - tmonth)
        t2 = 0.25*tmonth*(1 - tmonth)

        wm2 = -t1 + t2
        wm1 = -c0 + 8*t1 - 6*t2
        w0 = 7*c0 + 10*t2
        wp1 = -c0 + 8*t0 - 6*t2
        wp2 = -t0 + t2

        for1 = wm2*for12(:, im2) + wm1*for12(:, im1) + w0*for12(:, imon) + &
               wp1*for12(:, ip1) + wp2*for12(:, ip2)
    end subroutine
end module
