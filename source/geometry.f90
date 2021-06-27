!> author: Sam Hatfield, Fred Kucharski, Franco Molteni
!  date: 01/05/2019
!  For storing all variables related to the model's grid space.
module geometry
    use types, only: p
    use params

    implicit none

    private
    public initialize_geometry

contains
    !> Initializes all of the model geometry variables.
    subroutine initialize_geometry(state)
        use model_state, only: ModelState_t
        use physical_constants, only: akap, omega

        type(ModelState_t), intent(inout):: state

        integer j, jj, k

        ! Definition of model levels
        ! Half (vertical velocity) levels
        if (kx == 5) then
            state%hsg(:6) = (/ 0.000, 0.150, 0.350, 0.650, 0.900, 1.000 /)
        else if (kx == 7) then
            state%hsg(:8) = (/ 0.020, 0.140, 0.260, 0.420, 0.600, 0.770, 0.900, 1.000 /)
        else if (kx == 8) then
            state%hsg(:9) = (/ 0.000, 0.050, 0.140, 0.260, 0.420, 0.600, 0.770, 0.900, 1.000 /)
        end if

        ! Layer thicknesses and full (u,v,T) levels
        do k = 1, kx
            state%dhs(k) = state%hsg(k+1)-state%hsg(k)
            state%fsg(k) = 0.5*(state%hsg(k+1)+state%hsg(k))
        end do

        ! Additional functions of sigma
        do k = 1, kx
            state%dhsr(k) = 0.5/state%dhs(k)
            state%fsgr(k) = akap/(2.*state%fsg(k))
        end do

        ! Horizontal functions

        ! Latitudes and functions of latitude
        ! NB: J=1 is Southernmost point!
        do j = 1, iy
            jj = il + 1 - j
            state%sia_half(j) = cos(3.141592654*(j - 0.25)/(il + 0.5))
            state%coa_half(j) = sqrt(1.0 - state%sia_half(j)**2.0)
            state%sia(j)  = -state%sia_half(j)
            state%sia(jj) =  state%sia_half(j)
            state%coa(j)  = state%coa_half(j)
            state%coa(jj) = state%coa_half(j)
            state%radang(j)  = -asin(state%sia_half(j))
            state%radang(jj) =  asin(state%sia_half(j))
        end do

        ! Expand cosine and its reciprocal to cover both hemispheres
        do j=1,iy
            jj=il+1-j
            state%cosg(j)=state%coa_half(j)
            state%cosg(jj)=state%coa_half(j)
            state%cosgr(j)=1./state%coa_half(j)
            state%cosgr(jj)=1./state%coa_half(j)
            state%cosgr2(j)=1./(state%coa_half(j)*state%coa_half(j))
            state%cosgr2(jj)=1./(state%coa_half(j)*state%coa_half(j))
        end do

        state%coriol = 2.0*omega*state%sia
    end subroutine
end module