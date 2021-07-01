!> author: Sam Hatfield, Fred Kucharski, Franco Molteni
!  date: 01/05/2019
!  For storing all variables related to the model's grid space.

module geometry
    use types, only: p
    use params

    implicit none

    private
    public initialize_geometry
    public hsg, dhs, fsg, dhsr, fsgr, radang, coriol, sia, coa, sia_half, coa_half, &
        cosg, cosgr, cosgr2

    ! Vertical level parameters
    real(p),save :: hsg(kx+1) !! Half sigma levels
    real(p),save :: dhs(kx)   !! Sigma level thicknesses
    real(p),save :: fsg(kx)   !! Full sigma levels
    real(p),save :: dhsr(kx)  !! 1/(2*sigma level thicknesses)
    real(p),save :: fsgr(kx)  !! akap/(2*full sigma levels)

    ! Functions of latitude and longitude
    real(p),save, dimension(il) :: radang   !! Latitudes in radians
    real(p),save, dimension(il) :: coriol   !! Coriolis parameter as a function of latitude
    real(p),save, dimension(il) :: sia      !! sine(latitude)
    real(p),save, dimension(il) :: coa      !! cosine(latitude)
    real(p),save, dimension(iy) :: sia_half !! sine(latitude) over one hemisphere only
    real(p),save, dimension(il) :: coa_half !! cosine(latitude) over one hemisphere only
    real(p),save, dimension(il) :: cosg     !! Same as coa (TODO: remove)
    real(p),save, dimension(il) :: cosgr    !! 1/coa
    real(p),save, dimension(il) :: cosgr2   !! 1/coa^2

    logical, save :: geometry_mod_initialized_flag = .false.

contains
    !> Initializes all of the model geometry variables.
    subroutine initialize_geometry
        use physical_constants, only: akap, omega
        
        integer j, jj, k

        if (geometry_mod_initialized_flag) then
            !Do nothing
            return
        end if

        ! Definition of model levels
        ! Half (vertical velocity) levels
        if (kx == 5) then
            hsg(:6) = (/ 0.000, 0.150, 0.350, 0.650, 0.900, 1.000 /)
        else if (kx == 7) then
            hsg(:8) = (/ 0.020, 0.140, 0.260, 0.420, 0.600, 0.770, 0.900, 1.000 /)
        else if (kx == 8) then
            hsg(:9) = (/ 0.000, 0.050, 0.140, 0.260, 0.420, 0.600, 0.770, 0.900, 1.000 /)
        end if

        ! Layer thicknesses and full (u,v,T) levels
        do k = 1, kx
            dhs(k) = hsg(k+1)-hsg(k)
            fsg(k) = 0.5*(hsg(k+1)+hsg(k))
        end do

        ! Additional functions of sigma
        do k = 1, kx
            dhsr(k) = 0.5/dhs(k)
            fsgr(k) = akap/(2.*fsg(k))
        end do

        ! Horizontal functions

        ! Latitudes and functions of latitude
        ! NB: J=1 is Southernmost point!
        do j = 1, iy
            jj = il + 1 - j
            sia_half(j) = cos(3.141592654*(j - 0.25)/(il + 0.5))
            coa_half(j) = sqrt(1.0 - sia_half(j)**2.0)
            sia(j)  = -sia_half(j)
            sia(jj) =  sia_half(j)
            coa(j)  = coa_half(j)
            coa(jj) = coa_half(j)
            radang(j)  = -asin(sia_half(j))
            radang(jj) =  asin(sia_half(j))
        end do

        ! Expand cosine and its reciprocal to cover both hemispheres
        do j=1,iy
            jj=il+1-j
            cosg(j)=coa_half(j)
            cosg(jj)=coa_half(j)
            cosgr(j)=1./coa_half(j)
            cosgr(jj)=1./coa_half(j)
            cosgr2(j)=1./(coa_half(j)*coa_half(j))
            cosgr2(jj)=1./(coa_half(j)*coa_half(j))
        end do

        coriol = 2.0*omega*sia

        geometry_mod_initialized_flag = .true.
    end subroutine
end module