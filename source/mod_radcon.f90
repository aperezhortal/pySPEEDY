!> Radiation and cloud constants
module mod_radcon
    use types, only : p
    use params

    implicit none

    private
    public albsea, albice, albsn, epslw, emisfc

    real(p), parameter :: albsea = 0.07 !! Albedo over sea
    real(p), parameter :: albice = 0.60 !! Albedo over ice  (for ice fraction = 1)
    real(p), parameter :: albsn = 0.60 !! Albedo over snow (for snow cover = 1)

    real(p), parameter :: epslw = 0.05 !! Fraction of blackbody spectrum absorbed/emitted by PBL only
    real(p), parameter :: emisfc = 0.98 !! Longwave surface emissivity

end module
