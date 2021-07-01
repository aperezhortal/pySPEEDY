!> author: Sam Hatfield, Fred Kucharski, Franco Molteni
!  date: 29/04/2019
!  Highest level interface to land and sea models.
module coupler
    implicit none

    private
    public initialize_coupler, couple_sea_land

contains
    !> Initialize both land and sea models.
    subroutine initialize_coupler(state, control_params)
        use land_model, only: land_model_init, couple_land_atm
        use sea_model, only: sea_model_init, couple_sea_atm
        use model_control, only: ControlParams_t
        use model_state, only: ModelState_t

        type(ModelState_t) :: state
        type(ControlParams_t), intent(in)  :: control_params

        ! Initialize land model constants
        call land_model_init(state)

        ! Initialize land model variables
        call couple_land_atm(state, 0, control_params%imont1, control_params%tmonth)

        ! Initialize sea and ice model constants
        call sea_model_init(state, control_params%isst0)

        ! Initialize sea and ice model variables
        call couple_sea_atm(state, 0, control_params%model_datetime, &
                            control_params%start_datetime, control_params%imont1, &
                            control_params%tmonth)
    end subroutine

    !> Exchange fluxes between atmosphere and land/sea.
    subroutine couple_sea_land(model_vars, day, control_params)
        use land_model, only: couple_land_atm
        use sea_model, only: couple_sea_atm
        use model_control, only: ControlParams_t
        use model_state, only: ModelState_t

        type(ModelState_t) :: model_vars

        integer, intent(in) :: day !! The current day of the model integration (starting from 0)
        type(ControlParams_t), intent(in)  :: control_params

        call couple_land_atm(model_vars, day, control_params%imont1, control_params%tmonth)
        call couple_sea_atm(model_vars, &
                            day, control_params%model_datetime, &
                            control_params%start_datetime, control_params%imont1, &
                            control_params%tmonth)
    end subroutine
end module
