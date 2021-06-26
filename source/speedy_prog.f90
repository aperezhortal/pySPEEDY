!> author: Sam Hatfield, Fred Kucharski, Franco Molteni
!> date: 29/04/2019
!> The top-level program. Here we initialize the model and run the main loop
!> until the (continually updated) model datetime (`model_datetime`) equals the
!> final datetime (`end_datetime`).
program prog_speedy
    use types, only: p
    use speedy, only: run_speedy
    use model_vars, only: ModelVars_allocate, ModelVars_t
    use params, only: UserParams_t, kx, ntr, ix, il

    implicit none

    real(p), allocatable :: vor(:,:,:)   !! Vorticity
    real(p), allocatable :: div(:,:,:)    !! Divergence
    real(p), allocatable :: t(:,:,:)      !! Absolute temperature
    real(p), allocatable :: ps(:,:)      !! Log of (normalised) surface pressure (p_s/p0)
    real(p), allocatable :: tr(:,:,:,:) !! Tracers (tr(1): specific humidity in g/kg)
    real(p), allocatable :: phi(:,:,:)  !! Atmospheric geopotential

    allocate(vor(ix, il, kx))
    allocate(div(ix, il, kx))
    allocate(t(ix, il, kx))
    allocate(ps(ix, il))
    allocate(tr(ix, il, kx,ntr))
    allocate(phi(ix, il, kx))
    
    call run_speedy(vor, div, t, ps, tr, phi, ix, il, kx, ntr)
    
    ! call run_speedy( &
    !     ! Prognostic variables
    !     vor, &
    !     div, &
    !     t, &
    !     ps, &
    !     tr, &
    !     phi, &
    !     phis, &
    !     ! Auxiliary variables
    !     model_vars%precnv, &
    !     model_vars%precls, &
    !     model_vars%snowcv, &
    !     model_vars%snowls, &
    !     model_vars%cbmf, &
    !     model_vars%tsr, &
    !     model_vars%ssrd, &
    !     model_vars%ssr, &
    !     model_vars%slrd, &
    !     model_vars%slr, &
    !     model_vars%olr, &
    !     model_vars%slru, &
    !     model_vars%ustr, &
    !     model_vars%vstr, &
    !     model_vars%shf, &
    !     model_vars%evap, &
    !     model_vars%hfluxn, &
    !     ! Shape of the arrays. Needed for the f2py interface.
    !     kx, ntr, ix, il)
end

