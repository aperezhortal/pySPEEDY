!> author: Sam Hatfield, Fred Kucharski, Franco Molteni
!  date: 04/07/2019
!  For storing and initializing model parameters.
module params
    use types, only : p

    implicit none

    private
    public trunc, ix, iy, il, kx, nx, mx, ntr, t_levs, aux_dim
    public nsteps, iseasc, nstrad, sppt_on, delt, rob, wil, alph

    ! =========================================================================
    ! Constant parameters
    ! =========================================================================

    ! Model geometry parameters
    integer, parameter :: trunc = 30   !! Spectral truncation total wavenumber
    integer, parameter :: ix = 96      !! Number of longitudes
    integer, parameter :: iy = 24      !! Number of latitudes in hemisphere
    integer, parameter :: il = 2 * iy    !! Number of latitudes in full sphere
    integer, parameter :: kx = 8       !! Number of vertical levels
    integer, parameter :: nx = trunc + 2 !! Number of total wavenumbers for spectral storage arrays
    integer, parameter :: mx = trunc + 1 !! Number of zonal wavenumbers for spectral storage arrays
    integer, parameter :: ntr = 1      !! Number of tracers (specific humidity is considered a
    !! tracer)

    integer, parameter :: t_levs = 2   !! Number of time levels for the variable.
    integer, parameter :: aux_dim = 3  !! Auxiliary dimension: 1:land, 2:sea, 3: weighted average

    ! Time stepping parameters
    integer, parameter :: nsteps = 36           !! Number of time steps in one day. Must be an even number?
    real(p), parameter :: delt = 86400.0 / nsteps !! Time step in seconds
    real(p), parameter :: rob = 0.05            !! Damping factor in Robert time filter
    real(p), parameter :: wil = 0.53            !! Parameter of Williams filter
    real(p), parameter :: alph = 0.5            !! Coefficient for semi-implicit computations
    !! 0 -> forward step for gravity wave terms,
    !! 1 -> backward implicit, 0.5 -> centered
    !! implicit

    ! Physics parameters
    integer, parameter :: iseasc = 1        !! Seasonal cycle flag (0=no, 1=yes)
    integer, parameter :: nstrad = 3        !! Period (number of steps) for shortwave radiation
    logical, parameter :: sppt_on = .false. !! Turn on SPPT?

end module
