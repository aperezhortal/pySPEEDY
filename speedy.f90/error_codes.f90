!> author: Andres Perez Hortal
!  date: 02/07/2021
!  Module defining the erorr codes used by the SPEEDY model
module error_codes
    implicit none

    integer, parameter :: SUCCESS=0
    integer, parameter :: E_STATE_NOT_INITIALIZED=-1
    integer, parameter :: E_DIAGNOSTICS_OUTSIDE_RANGE=-2

end module