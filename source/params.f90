!> author: Sam Hatfield, Fred Kucharski, Franco Molteni
!  date: 04/07/2019
!  For storing and initializing model parameters.
module params
    use types, only: p

    implicit none

    private
    public trunc, ix, iy, il, kx, nx, mx, ntr, t_levs, aux_dim
    public nsteps, iseasc, nstrad, sppt_on, issty0, delt, rob, wil, alph
    public initialize_params
    public UserParams_t

    ! =========================================================================
    ! Constant parameters
    ! =========================================================================

    ! Model geometry parameters
    integer, parameter :: trunc = 30   !! Spectral truncation total wavenumber
    integer, parameter :: ix = 96      !! Number of longitudes
    integer, parameter :: iy = 24      !! Number of latitudes in hemisphere
    integer, parameter :: il = 2*iy    !! Number of latitudes in full sphere
    integer, parameter :: kx = 8       !! Number of vertical levels
    integer, parameter :: nx = trunc + 2 !! Number of total wavenumbers for spectral storage arrays
    integer, parameter :: mx = trunc + 1 !! Number of zonal wavenumbers for spectral storage arrays
    integer, parameter :: ntr = 1      !! Number of tracers (specific humidity is considered a
                                       !! tracer)

    integer, parameter :: t_levs = 2   !! Number of time levels for the variable.
    integer, parameter :: aux_dim = 3  !! Auxiliary dimension: 1:land, 2:sea, 3: weighted average

    ! Time stepping parameters
    integer, parameter    :: nsteps = 36           !! Number of time steps in one day
    real(p), parameter    :: delt = 86400.0/nsteps !! Time step in seconds
    real(p), parameter    :: rob = 0.05            !! Damping factor in Robert time filter
    real(p), parameter    :: wil = 0.53            !! Parameter of Williams filter
    real(p), parameter    :: alph = 0.5            !! Coefficient for semi-implicit computations
                                                   !! 0 -> forward step for gravity wave terms,
                                                   !! 1 -> backward implicit, 0.5 -> centered
                                                   !! implicit

    ! Physics parameters
    integer, parameter :: iseasc = 1        !! Seasonal cycle flag (0=no, 1=yes)
    integer, parameter :: nstrad = 3        !! Period (number of steps) for shortwave radiation
    logical, parameter :: sppt_on = .false. !! Turn on SPPT?
    integer, parameter :: issty0 = 1979     !! Starting year for SST anomaly file

    ! =========================================================================
    ! User-specified parameters (through the namelist file)
    ! =========================================================================
    type UserParams_t
        integer :: nstdia     !! Period (number of steps) for diagnostic print-out
        integer :: nsteps_out !! Number of time steps between outputs
    end type

contains
    !> Initializes user-defined parameters from namelist file.
    subroutine initialize_params(user_params)
        class(UserParams_t), intent(out) :: user_params
        integer :: nstdia !! Period (number of steps) for diagnostic print
        integer :: nsteps_out !! Number of time steps between outputs

        namelist /params/ nsteps_out, nstdia
        logical :: namelist_file_exists

        ! Set default values
        nsteps_out = 1
        nstdia = 36*5

        ! Read namelist file, if it exists
        inquire (file="namelist.nml", exist=namelist_file_exists)
        if (namelist_file_exists) then
            open (10, file="namelist.nml")
            read (10, nml=params)
            close (10)
        end if

        user_params%nsteps_out = nsteps_out
        user_params%nstdia = nstdia

        ! Print values to screen
        write (*, '(A,I5)') 'nsteps_out (frequency of output)=', user_params%nsteps_out
        write (*, '(A,I5)') 'nstdia (frequency of diagnostics)=', user_params%nstdia
    end subroutine
end module
