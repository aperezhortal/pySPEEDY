!> author: Sam Hatfield, Fred Kucharski, Franco Molteni
!> date: 29/04/2019
!> The top-level program. Here we initialize the model and run the main loop
!> until the (continually updated) model datetime (`model_datetime`) equals the
!> final datetime (`end_datetime`).
program prog_speedy
    use speedy, only: run_speedy
    use model_variables, only: ModelVars_allocate, ModelVars_t
    use params, only: UserParams_t, mx, nx, kx, ntr,  ix, iy, il

    implicit none

    type(UserParams_t) :: user_params
    type(ModelVars_t) :: model_variables

    ! Initialization
    call ModelVars_allocate(model_variables)

    call run_speedy( &
        ! Prognostic variables
        model_variables%vor, &
        model_variables%div, &
        model_variables%t, &
        model_variables%ps, &
        model_variables%tr, &
        model_variables%phi, &
        model_variables%phis, &
        ! Auxiliary variables
        model_variables%precnv, &
        model_variables%precls, &
        model_variables%snowcv, &
        model_variables%snowls, &
        model_variables%cbmf, &
        model_variables%tsr, &
        model_variables%ssrd, &
        model_variables%ssr, &
        model_variables%slrd, &        
        model_variables%slr, &
        model_variables%olr, &
        model_variables%slru, &
        model_variables%ustr, &
        model_variables%vstr, &
        model_variables%shf, &
        model_variables%evap, &
        model_variables%hfluxn, &
        ! Shape of the arrays. Needed for the f2py interface.
        mx, nx, kx, ntr, ix, iy, il)
end

