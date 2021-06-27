!> author: Sam Hatfield, Fred Kucharski, Franco Molteni
!  date: 08/05/2019
!  For initializing and performing implicit computations.
module implicit
    use types, only: p
    use params

    implicit none

    private
    public initialize_implicit, implicit_terms
    public tref, tref2, tref3

    real(p), dimension(kx) :: tref  ! Temperature profile for semi-implicit scheme
    real(p), dimension(kx) :: tref1 ! Gas constant * tref
    real(p), dimension(kx) :: tref2 ! akap * tref
    real(p), dimension(kx) :: tref3 ! Full sigma-levels * tref

    real(p), dimension(kx,kx) :: xa, xb, xc, xd, xe
    real(p), dimension(kx,kx,mx+nx+1) :: xf, xj
    real(p) :: dhsx(kx), elz(mx,nx)

contains
    !> Initialize constants for implicit computation of horizontal diffusion and
    !  gravity waves.
    !
    !  Initialize_implicit initializes constants for the implicit gravity wave computation.
    !  It is assumed that that all implicit steps are of length 2*delt and use
    !  the forward/backward parameter alph. initialize_implicit has to be re-called
    !  whenever either of these two parameters is changed. initialize_implicit should
    !  be called even if the explicit option is chosen for the gravity wave
    !  terms (the reference state temperature tref is subtracted from some
    !  terms anyway to reduce roundoff error; also the constants needed for
    !  the biharmonic diffusion, which is assumed always to be backwards
    !  implicit, are defined in initialize_implicit).
    subroutine initialize_implicit(dt)
        use dynamical_constants, only: gamma
        use physical_constants, only: akap, rgas, grav, rearth
        use geometry, only: hsg, dhs, fsg, fsgr
        use horizontal_diffusion, only: dmp, dmpd, dmps, dmp1, dmp1d, dmp1s
        use matrix_inversion, only: inv

        real(p), intent(in) :: dt !! Time step

        real(p) :: dsum(kx), ya(kx,kx)
        integer :: indx(kx), m, n, k, k1, k2, l
        real(p) :: rgam, xi, xxi, xxx

        ! 1. Constants for backwards implicit biharmonic diffusion
        do m=1,mx
            do n=1,nx
                dmp1 (m,n)=1./(1.+dmp (m,n)*dt)
                dmp1d(m,n)=1./(1.+dmpd(m,n)*dt)
                dmp1s(m,n)=1./(1.+dmps(m,n)*dt)
            end do
        end do

        ! 1. Constants for implicit gravity wave computation
        ! reference atmosphere, function of sigma only
        rgam = rgas*gamma/(1000.*grav)

        do k=1,kx
            tref(k)=288.*max(0.2,fsg(k))**rgam
            tref1(k)=rgas*tref(k)
            tref2(k)=akap*tref(k)
            tref3(k)=fsgr(k)*tref(k)
        end do

        ! Other constants
        xi=dt*alph
        xxi = xi/(rearth*rearth)

        dhsx = xi * dhs

        do n=1,nx
            do m=1,mx
                elz(m,n)=float(m+n-2)*float(m+n-1)*xxi
            end do
        end do

        !T(K) = TEX(K)+YA(K,K')*D(K') + XA(K,K')*SIG(K')

        xa(:kx,:kx-1) = 0.0

        do k=1,kx
            do k1=1,kx
                ya(k,k1)=-akap*tref(k)*dhs(k1)
            end do
        end do

        do k=2,kx
            xa(k,k-1)=0.5*(akap*tref(k)/fsg(k)-(tref(k)-tref(k-1))/dhs(k))
        end do

        do k=1,kx-1
            xa(k,k)=0.5*(akap*tref(k)/fsg(k)-(tref(k+1)-tref(k))/dhs(k))
        end do

        !sig(k)=xb(k,k')*d(k')
        dsum(1)=dhs(1)
        do k=2,kx
            dsum(k)=dsum(k-1)+dhs(k)
        end do

        do k=1,kx-1
            do k1=1,kx
                xb(k,k1)=dhs(k1)*dsum(k)
                if(k1.le.k) xb(k,k1)=xb(k,k1)-dhs(k1)
            end do
        end do

        !t(k)=tex(k)+xc(k,k')*d(k')
        do k=1,kx
            do k1=1,kx
                xc(k,k1)=ya(k,k1)
                do k2=1,kx-1
                    xc(k,k1)=xc(k,k1)+xa(k,k2)*xb(k2,k1)
                end do
            end do
        end do

        !P(K)=XD(K,K')*T(K')
        xd = 0.0

        do k=1,kx
            do k1=k+1,kx
                xd(k,k1)=rgas*log(hsg(k1+1)/hsg(k1))
            end do
        end do
        do k=1,kx
            xd(k,k)=rgas*log(hsg(k+1)/fsg(k))
        end do

        !P(K)=YE(K)+XE(K,K')*D(K')
        do k=1,kx
            do k1=1,kx
                xe(k,k1)=0.
                do k2=1,kx
                    xe(k,k1)=xe(k,k1)+xd(k,k2)*xc(k2,k1)
                end do
            end do
        end do

        do l=1,mx + nx + 1
            xxx=(float(l)*float(l+1))/(rearth*rearth)
            do k=1,kx
                do k1=1,kx
                    xf(k,k1,l)=xi*xi*xxx*(rgas*tref(k)*dhs(k1)-xe(k,k1))
                end do
            end do
            do k=1,kx
                xf(k,k,l)=xf(k,k,l)+1.
            end do
        end do

        do l=1,mx + nx + 1
            call inv(xf(1,1,l),xj(1,1,l),indx,kx)
        end do

        do k=1,kx
            do k1=1,kx
                xc(k,k1)=xc(k,k1)*xi
            end do
        end do
    end subroutine

    !> Correct tendencies for implicit gravity wave model
    subroutine implicit_terms(divdt,tdt,psdt)
        complex(p), intent(inout) :: divdt(mx,nx,kx) !! Divergence tendency
        complex(p), intent(inout) :: tdt(mx,nx,kx)   !! Temperature tendency
        complex(p), intent(inout) :: psdt(mx,nx)     !! log(surface pressure) tendency

        complex(p) ::  ye(mx,nx,kx), yf(mx,nx,kx)
        integer :: k1, k, m, n

        ye(:,:,:) = (0.0, 0.0)

        do k1=1,kx
            do k=1,kx
                ye(:,:,k) = ye(:,:,k) + xd(k,k1) * tdt(:,:,k1)
            end do
        end do

        do k=1,kx
            ye(:,:,k) = ye(:,:,k) + tref1(k) * psdt
        end do

        do k=1,kx
            do m=1,mx
                do n=1,nx
                    yf(m,n,k)=divdt(m,n,k)+elz(m,n)*ye(m,n,k)
                end do
            end do
        end do

        divdt(:,:,:) = (0.0, 0.0)

        do n=1,nx
            do m=1,mx
                if((m + n - 2) /= 0) then
                    do k1=1,kx
                        divdt(m,n,:) = divdt(m,n,:) + xj(:,k1,m+n-2) * yf(m,n,k1)
                    end do
                endif
            end do
        end do

        do k=1,kx
            psdt = psdt - divdt(:,:,k) * dhsx(k)
        end do

        do k=1,kx
            do k1=1,kx
                tdt(:,:,k) = tdt(:,:,k) + xc(k,k1) * divdt(:,:,k1)
            end do
        end do
    end
end module
