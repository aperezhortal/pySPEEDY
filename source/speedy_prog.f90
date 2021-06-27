!> author: Sam Hatfield, Fred Kucharski, Franco Molteni
!> date: 29/04/2019
!> The top-level program. Here we initialize the model and run the main loop
!> until the (continually updated) model datetime (`model_datetime`) equals the
!> final datetime (`end_datetime`).
program prog_speedy
    use types, only: p
    use speedy, only: run_speedy
    use model_state, only: ModelState_t, ModelState_allocate, ModelState_deallocate

    implicit none
    type(ModelState_t) :: state
    call ModelState_allocate(state)
    call run_speedy(state)
    call ModelState_deallocate(state)
    end

