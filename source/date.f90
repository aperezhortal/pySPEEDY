!> author: Sam Hatfield, Fred Kucharski, Franco Molteni
!  date: 01/05/2019
!  For keeping track of the model's date and time.
module date
    use types, only: p
    implicit none

    private
    public datetime_equal, initialize_date, advance_date, ControlParams_t, Datetime_t

    !> For storing dates and times.
    type Datetime_t
        integer :: year
        integer :: month
        integer :: day
        integer :: hour
        integer :: minute
    end type

    !> For storing dates and times.
    type ControlParams_t
        ! Date and time variables
        type(Datetime_t)     :: model_datetime !! The model's current datetime (continuously updated)
        type(Datetime_t)     :: start_datetime !! The start datetime
        type(Datetime_t)     :: end_datetime   !! The end datetime
        integer            :: imont1         !! The month used for computing seasonal forcing fields
        real(p)            :: tmonth         !! The fraction of the current month elapsed
        real(p)            :: tyear          !! The fraction of the current year elapsed
        integer            :: isst0          !! Initial month of SST anomalies
        integer            :: ndaycal(12, 2)  !! The model calendar
    end type

    integer, parameter :: ncal = 365     !! The number of days in a year

    !> The number of days in each month
    integer :: ncal365(12) = (/31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31/)

contains
    !> Checks whether two datetimes are equal.
    logical function datetime_equal(datetime1, datetime2)
        type(Datetime_t), intent(in) :: datetime1, datetime2

        if (datetime1%year == datetime2%year .and. &
            datetime1%month == datetime2%month .and. &
            datetime1%day == datetime2%day .and. &
            datetime1%hour == datetime2%hour .and. &
            datetime1%minute == datetime2%minute) then
            datetime_equal = .true.
        else
            datetime_equal = .false.
        end if
    end function

    !> Initializes model date and calendar.
    subroutine initialize_date(control_params)
        use params, only: iseasc
        type(ControlParams_t), target  :: control_params

        type(Datetime_t), pointer  :: model_datetime
        type(Datetime_t)     :: start_datetime, end_datetime
        integer, pointer   :: imont1
        real(p), pointer   :: tmonth
        real(p), pointer   :: tyear
        integer, pointer   :: ndaycal(:, :)

        namelist /date/ start_datetime, end_datetime
        integer :: jm
        logical :: namelist_file_exists

        ! Set default values for start and end datetime
        control_params%start_datetime = Datetime_t(1982, 1, 1, 0, 0)
        control_params%end_datetime = Datetime_t(1982, 2, 2, 0, 0)

        ! Some mappings to improve code redability
        ndaycal => control_params%ndaycal
        tyear => control_params%tyear
        tmonth => control_params%tmonth
        imont1 => control_params%imont1
        model_datetime => control_params%model_datetime

        ! Read namelist file
        inquire (file="namelist.nml", exist=namelist_file_exists)
        if (namelist_file_exists) then
            open (10, file="namelist.nml")
            read (10, nml=date)
            close (10)
        end if

        control_params%start_datetime = start_datetime
        control_params%end_datetime = end_datetime

        ! Current model datetime is start datetime
        model_datetime = start_datetime

        ! Print values to screen
        write (*, '(A12,I4,A,I0.2,A,I0.2,A,I0.2,A,I0.2)') 'Start date: ', &
            & start_datetime%year, '/', start_datetime%month, '/', start_datetime%day, ' ', &
            & start_datetime%hour, ':', start_datetime%minute
        write (*, '(A12,I4,A,I0.2,A,I0.2,A,I0.2,A,I0.2)') 'End date: ', &
            & end_datetime%year, '/', end_datetime%month, '/', end_datetime%day, ' ', &
            & end_datetime%hour, ':', end_datetime%minute

        ! Set calendar
        if (ncal == 365) then
            ndaycal(:, 1) = ncal365(:)
        else
            ndaycal(:, 1) = 30
        end if

        ndaycal(1, 2) = 0
        do jm = 2, 12
            ndaycal(jm, 2) = ndaycal(jm - 1, 1) + ndaycal(jm - 1, 2)
        end do

        call update_forcing_params(control_params)

    end subroutine

    !> Updates the current datetime and related date variables.
    subroutine advance_date(control_params)
        use params, only: iseasc, nsteps
        type(ControlParams_t), target, intent(inout)  :: control_params
        type(Datetime_t), pointer  :: model_datetime
        integer, pointer   :: imont1
        real(p), pointer   :: tmonth
        real(p), pointer   :: tyear
        integer, pointer   :: ndaycal(:, :)

        ! Some mappings to improve code redability
        ndaycal => control_params%ndaycal
        tyear => control_params%tyear
        tmonth => control_params%tmonth
        imont1 => control_params%imont1
        model_datetime => control_params%model_datetime

        ! Increment minute counter
        model_datetime%minute = model_datetime%minute + int(24*60/nsteps)

        ! Increment hour counter if necessary
        if (model_datetime%minute >= 60) then
            model_datetime%minute = mod(model_datetime%minute, 60)
            model_datetime%hour = model_datetime%hour + 1
        end if

        ! Increment day counter if necessary
        if (model_datetime%hour >= 24) then
            model_datetime%hour = mod(model_datetime%hour, 24)
            model_datetime%day = model_datetime%day + 1
        end if

        ! Increment month counter if necessary
        ! Leap year and February?
        if (mod(model_datetime%year, 4) == 0 .and. model_datetime%month == 2) then
            if (model_datetime%day > 29) then
                model_datetime%day = 1
                model_datetime%month = model_datetime%month + 1
            end if
        else
            if (model_datetime%day > ndaycal(model_datetime%month, 1)) then
                model_datetime%day = 1
                model_datetime%month = model_datetime%month + 1
            end if
        end if

        ! Increment year counter if necessary
        if (model_datetime%month > 12) then
            model_datetime%month = 1
            model_datetime%year = model_datetime%year + 1
        end if

        call update_forcing_params(control_params)

    end subroutine

    subroutine update_forcing_params(control_params)
        use params, only: iseasc
        type(ControlParams_t), target, intent(inout)  :: control_params

        type(Datetime_t), pointer  :: model_datetime
        integer, pointer   :: imont1
        real(p), pointer   :: tmonth
        real(p), pointer   :: tyear
        integer, pointer   :: ndaycal(:, :)

        ! Some mappings to improve code redability
        ndaycal => control_params%ndaycal
        tyear => control_params%tyear
        tmonth => control_params%tmonth
        imont1 => control_params%imont1
        model_datetime => control_params%model_datetime

        ! additional variables to define forcing terms and boundary cond.
        if (iseasc >= 1) then
            imont1 = model_datetime%month
            tmonth = (model_datetime%day - 0.5)/float(ndaycal(model_datetime%month, 1))
            tyear = (ndaycal(model_datetime%month, 2) + model_datetime%day - 0.5)/float(ncal)
        end if
    end subroutine

end module
