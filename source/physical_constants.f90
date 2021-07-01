!> author: Sam Hatfield, Fred Kucharski, Franco Molteni
!  date: 04/07/2019
!  For storing and initializing physical constants.
module physical_constants
    use types, only: p
    use params

    implicit none

    private
    public rearth, omega, grav
    public p0, cp, akap, rgas, alhc, alhs, sbc    

    ! Physical constants for dynamics
    real(p), parameter :: rearth = 6.371e+6    !! Radius of Earth (m)
    real(p), parameter :: omega  = 7.292e-05   !! Rotation rate of Earth (rad/s)
    real(p), parameter :: grav   = 9.81        !! Gravitational acceleration (m/s/s)

    ! Physical constants for thermodynamics
    real(p), parameter :: p0   = 1.e+5   !! Reference pressure (Pa)
    real(p), parameter :: cp   = 1004.0  !! Specific heat at constant pressure (J/K/kg)
    real(p), parameter :: akap = 2.0/7.0 !! 1 - 1/gamma where gamma is the heat capacity ratio of a
                                         !! perfect diatomic gas (7/5)
    real(p), parameter :: rgas = akap*cp !! Gas constant per unit mass for dry air (J/K/kg)
    real(p), parameter :: alhc = 2501.0  !! Latent heat of condensation, in J/g for consistency with
                                         !! specific humidity in g/Kg
    real(p), parameter :: alhs = 2801.0  !! Latent heat of sublimation
    real(p), parameter :: sbc  = 5.67e-8 !! Stefan-Boltzmann constant

end module
