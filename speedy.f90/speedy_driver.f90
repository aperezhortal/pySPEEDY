!
!  THIS CODE IS AUTOMATICALLY GENERATED.
!  DO NOT MODIFY IT DIRECTLY.
!
! Interface for the fortran speedy module.
!
!
! This interface follows the ideas presented in this paper:
! "Exposing Fortran Derived Types to C and Other Languages"
! S. Kruger, S. Muszala, A. Pletzer, S. Vadlamani and D. McCune,
! Computing in Science & Engineering, vol. 10, no. 04, pp. 86-92, 2008.
! doi: 10.1109/MCSE.2008.94
!
! https://doi.ieeecomputersociety.org/10.1109/MCSE.2008.94

module speedy_driver
    use model_state, only: ModelState_allocate, ModelState_deallocate, ModelState_t, ModelState_Ptr_t
    use model_control, only: ControlParams_Ptr_t, ControlParams_t, initialize_control
    use model_control, only: Datetime_t, Datetime_Ptr_t
    use types, only : p

    implicit none

contains

    ! =========================================================================
    ! Speedy interface
    ! =========================================================================
    subroutine init(state_container, control_container, error_code)
        use initialization, only: initialize_state
        
        integer(8), intent(in) :: state_container, control_container
        integer, intent(out) :: error_code

        type(ModelState_Ptr_t):: state_ptr
        type(ControlParams_Ptr_t):: control_ptr

        state_ptr = transfer(state_container, state_ptr)
        control_ptr = transfer(control_container, control_ptr)
        call initialize_state(state_ptr%p, control_ptr%p, error_code)
    end subroutine
   
    subroutine step(state_container, control_container, error_code)
        use speedy, only: do_single_step
        
        integer(8), intent(in) :: state_container, control_container
        integer, intent(out)   :: error_code

        type(ModelState_Ptr_t):: state_ptr
        type(ControlParams_Ptr_t):: control_ptr

        state_ptr = transfer(state_container, state_ptr)
        control_ptr = transfer(control_container, control_ptr)
        call do_single_step(state_ptr%p, control_ptr%p, error_code)
    end subroutine

    !> Parallel execution of the step function for an ensemble of states.
    subroutine parallel_step(state_containers, control_containers, error_codes, n_members)
        !f2py threadsafe
        use speedy, only: do_single_step
        Use omp_lib

        integer, intent(in) :: n_members
        integer(8), dimension(n_members), intent(in) :: state_containers, control_containers
        integer, dimension(n_members), intent(out)   :: error_codes

        integer :: m
        type(ModelState_Ptr_t):: state_ptr
        type(ControlParams_Ptr_t):: control_ptr

        !$OMP PARALLEL DO default(shared) PRIVATE (state_ptr ,control_ptr,m) SCHEDULE(dynamic)
        do m=1,n_members
            state_ptr = transfer(state_containers(m), state_ptr)
            control_ptr = transfer(control_containers(m), control_ptr)
            call do_single_step(state_ptr%p, control_ptr%p, error_codes(m))
        enddo
        !$OMP END PARALLEL DO

    end subroutine

    subroutine check(state_container, error_code)
        use diagnostics, only : check_diagnostics

        integer(8), intent(in) :: state_container
        integer, intent(out)   :: error_code

        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_container, state_ptr)
        call check_diagnostics(state_ptr%p, time_lev=1, error_code=error_code)
    end subroutine


    subroutine transform_spectral2grid(state_container)
        use prognostics, only: spectral2grid

        integer(8), intent(in) :: state_container

        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_container, state_ptr)
        call spectral2grid(state_ptr%p)
    end subroutine

    subroutine transform_grid2spectral(state_container)
        use prognostics, only: grid2spectral

        integer(8), intent(in) :: state_container

        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_container, state_ptr)
        call grid2spectral(state_ptr%p)
    end subroutine

    subroutine apply_grid_filter(state_container)
        use prognostics, only: grid_filter

        integer(8), intent(in) :: state_container

        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_container, state_ptr)
        call grid_filter(state_ptr%p)
    end subroutine

    ! =========================================================================
    ! ControlParams interface
    ! =========================================================================
    !> Initialize the ControlParams and return the container with the object.
    subroutine controlparams_init(control_cnt, start_datetime_cnt, end_datetime_cnt)

        integer(8), intent(out) :: control_cnt !! Container for the ControlParams_Ptr_t object.
        integer(8), intent(in) :: start_datetime_cnt
        integer(8), intent(in) :: end_datetime_cnt

        type(ControlParams_Ptr_t) :: control_ptr
        type(Datetime_Ptr_t) :: start_datetime_ptr, end_datetime_ptr

        control_ptr = transfer(control_cnt, control_ptr)
        start_datetime_ptr = transfer(start_datetime_cnt, start_datetime_ptr)
        end_datetime_ptr = transfer(end_datetime_cnt, end_datetime_ptr)

        allocate (control_ptr%p)
        call initialize_control(control_ptr%p, start_datetime_ptr%p, end_datetime_ptr%p)

        control_cnt = transfer(control_ptr, control_cnt)
    end subroutine

    !> Deallocate the ControlParams
    subroutine controlparams_close(this)        
        integer(8), intent(in) :: this

        type(ControlParams_Ptr_t):: this_ptr

        this_ptr = transfer(this, this_ptr)
        deallocate(this_ptr%p)
    end subroutine

    ! =========================================================================
    ! Datetime interface
    ! =========================================================================
    subroutine create_datetime(year, month, day, hour, minute, datetime_cnt)
        integer, intent(in) :: year, month, day,hour, minute
        integer(8), intent(out) :: datetime_cnt       

        type(Datetime_Ptr_t):: datetime_out
        type(Datetime_t), pointer :: my_datetime

        ! 1. Create the fortran datetime_t object.
        ! We make it allocatable because we want to return the reference pointing to
        ! this object.        
        allocate(my_datetime)
        my_datetime%allocated = .true.
        my_datetime%year = year
        my_datetime%month = month
        my_datetime%day = day
        my_datetime%hour = hour
        my_datetime%minute = minute
        
        ! 2. Make the container point to that object.
        datetime_out%p => my_datetime

        ! 3. Return the container with the memory address to the datetime object.
        datetime_cnt = transfer(datetime_out, datetime_cnt)
    end subroutine

    subroutine get_datetime(this, year, month, day, hour, minute)       
        integer(8), intent(in) :: this
        
        integer, intent(out) :: year, month, day, hour, minute
        type(Datetime_Ptr_t):: this_ptr

        this_ptr = transfer(this, this_ptr)

        year = this_ptr%p%year
        month = this_ptr%p%month
        day = this_ptr%p%day
        hour = this_ptr%p%hour
        minute = this_ptr%p%minute       
    end subroutine

    subroutine close_datetime(this)  
        integer(8), intent(in) :: this

        type(Datetime_Ptr_t):: this_ptr

        this_ptr = transfer(this, this_ptr)
        if (this_ptr%p%allocated) deallocate(this_ptr%p)
    end subroutine

    ! =========================================================================
    ! ModelState interface
    ! =========================================================================  
    !> Initialize the ModelState and return the container with the object.
    subroutine modelstate_init(state_cnt)
        integer(8), intent(out) :: state_cnt !! Container for the ModelState_Ptr_t object.

        type(ModelState_Ptr_t) :: state_ptr
        allocate (state_ptr%p)
        call ModelState_allocate(state_ptr%p)
        state_cnt = transfer(state_ptr, state_cnt)
    end subroutine
    
    !> Allocate sst_anom
    subroutine modelstate_init_sst_anom(state_cnt, n_months)
        use model_state, only: ModelState_allocate_sst_anom
        integer(8), intent(in) :: state_cnt !! Container for the ModelState_Ptr_t object.
        integer, intent(in) :: n_months

        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        call ModelState_allocate_sst_anom(state_ptr%p, n_months)
    end subroutine

    !> Deallocate the ModelState
    subroutine modelstate_close(state_cnt)        
        integer(8), intent(in) :: state_cnt

        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        call ModelState_deallocate(state_ptr%p)
        deallocate(state_ptr%p)
    end subroutine


    subroutine get_vor(state_cnt, vor_out )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> Vorticity
        complex(8), intent(out) :: vor_out(mx, nx, kx, t_levs)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        vor_out=state_ptr%p%vor
    end subroutine

    subroutine set_vor(state_cnt, vor_in )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> Vorticity
        complex(8), intent(in) :: vor_in(mx, nx, kx, t_levs)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        state_ptr%p%vor=vor_in
    end subroutine
    
    subroutine get_vor_shape(state_cnt, array_shape)
        use params
        integer(8), intent(in) :: state_cnt
        !> Vorticity
        integer, intent(out) :: array_shape(4)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        if (state_ptr%p%vor_initialized) then
            array_shape=shape(state_ptr%p%vor)
        else
            array_shape = 0             
        endif
    end subroutine

    subroutine get_div(state_cnt, div_out )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> Divergence
        complex(8), intent(out) :: div_out(mx, nx, kx, t_levs)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        div_out=state_ptr%p%div
    end subroutine

    subroutine set_div(state_cnt, div_in )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> Divergence
        complex(8), intent(in) :: div_in(mx, nx, kx, t_levs)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        state_ptr%p%div=div_in
    end subroutine
    
    subroutine get_div_shape(state_cnt, array_shape)
        use params
        integer(8), intent(in) :: state_cnt
        !> Divergence
        integer, intent(out) :: array_shape(4)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        if (state_ptr%p%div_initialized) then
            array_shape=shape(state_ptr%p%div)
        else
            array_shape = 0             
        endif
    end subroutine

    subroutine get_t(state_cnt, t_out )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> Temperature
        complex(8), intent(out) :: t_out(mx, nx, kx, t_levs)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        t_out=state_ptr%p%t
    end subroutine

    subroutine set_t(state_cnt, t_in )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> Temperature
        complex(8), intent(in) :: t_in(mx, nx, kx, t_levs)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        state_ptr%p%t=t_in
    end subroutine
    
    subroutine get_t_shape(state_cnt, array_shape)
        use params
        integer(8), intent(in) :: state_cnt
        !> Temperature
        integer, intent(out) :: array_shape(4)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        if (state_ptr%p%t_initialized) then
            array_shape=shape(state_ptr%p%t)
        else
            array_shape = 0             
        endif
    end subroutine

    subroutine get_ps(state_cnt, ps_out )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> Log of (normalised) surface pressure
        complex(8), intent(out) :: ps_out(mx, nx, t_levs)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        ps_out=state_ptr%p%ps
    end subroutine

    subroutine set_ps(state_cnt, ps_in )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> Log of (normalised) surface pressure
        complex(8), intent(in) :: ps_in(mx, nx, t_levs)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        state_ptr%p%ps=ps_in
    end subroutine
    
    subroutine get_ps_shape(state_cnt, array_shape)
        use params
        integer(8), intent(in) :: state_cnt
        !> Log of (normalised) surface pressure
        integer, intent(out) :: array_shape(3)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        if (state_ptr%p%ps_initialized) then
            array_shape=shape(state_ptr%p%ps)
        else
            array_shape = 0             
        endif
    end subroutine

    subroutine get_tr(state_cnt, tr_out )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> Tracers (tr(1): specific humidity in g/kg)
        complex(8), intent(out) :: tr_out(mx, nx, kx, t_levs,ntr)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        tr_out=state_ptr%p%tr
    end subroutine

    subroutine set_tr(state_cnt, tr_in )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> Tracers (tr(1): specific humidity in g/kg)
        complex(8), intent(in) :: tr_in(mx, nx, kx, t_levs,ntr)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        state_ptr%p%tr=tr_in
    end subroutine
    
    subroutine get_tr_shape(state_cnt, array_shape)
        use params
        integer(8), intent(in) :: state_cnt
        !> Tracers (tr(1): specific humidity in g/kg)
        integer, intent(out) :: array_shape(5)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        if (state_ptr%p%tr_initialized) then
            array_shape=shape(state_ptr%p%tr)
        else
            array_shape = 0             
        endif
    end subroutine

    subroutine get_phi(state_cnt, phi_out )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> Atmospheric geopotential
        complex(8), intent(out) :: phi_out(mx, nx, kx)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        phi_out=state_ptr%p%phi
    end subroutine

    subroutine set_phi(state_cnt, phi_in )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> Atmospheric geopotential
        complex(8), intent(in) :: phi_in(mx, nx, kx)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        state_ptr%p%phi=phi_in
    end subroutine
    
    subroutine get_phi_shape(state_cnt, array_shape)
        use params
        integer(8), intent(in) :: state_cnt
        !> Atmospheric geopotential
        integer, intent(out) :: array_shape(3)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        if (state_ptr%p%phi_initialized) then
            array_shape=shape(state_ptr%p%phi)
        else
            array_shape = 0             
        endif
    end subroutine

    subroutine get_phis(state_cnt, phis_out )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> Surface geopotential
        complex(8), intent(out) :: phis_out(mx, nx)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        phis_out=state_ptr%p%phis
    end subroutine

    subroutine set_phis(state_cnt, phis_in )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> Surface geopotential
        complex(8), intent(in) :: phis_in(mx, nx)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        state_ptr%p%phis=phis_in
    end subroutine
    
    subroutine get_phis_shape(state_cnt, array_shape)
        use params
        integer(8), intent(in) :: state_cnt
        !> Surface geopotential
        integer, intent(out) :: array_shape(2)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        if (state_ptr%p%phis_initialized) then
            array_shape=shape(state_ptr%p%phis)
        else
            array_shape = 0             
        endif
    end subroutine

    subroutine get_u_grid(state_cnt, u_grid_out )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> eastward_wind
        real(8), intent(out) :: u_grid_out(ix, il, kx)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        u_grid_out=state_ptr%p%u_grid
    end subroutine

    subroutine set_u_grid(state_cnt, u_grid_in )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> eastward_wind
        real(8), intent(in) :: u_grid_in(ix, il, kx)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        state_ptr%p%u_grid=u_grid_in
    end subroutine
    
    subroutine get_u_grid_shape(state_cnt, array_shape)
        use params
        integer(8), intent(in) :: state_cnt
        !> eastward_wind
        integer, intent(out) :: array_shape(3)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        if (state_ptr%p%u_grid_initialized) then
            array_shape=shape(state_ptr%p%u_grid)
        else
            array_shape = 0             
        endif
    end subroutine

    subroutine get_v_grid(state_cnt, v_grid_out )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> northward_wind
        real(8), intent(out) :: v_grid_out(ix, il, kx)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        v_grid_out=state_ptr%p%v_grid
    end subroutine

    subroutine set_v_grid(state_cnt, v_grid_in )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> northward_wind
        real(8), intent(in) :: v_grid_in(ix, il, kx)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        state_ptr%p%v_grid=v_grid_in
    end subroutine
    
    subroutine get_v_grid_shape(state_cnt, array_shape)
        use params
        integer(8), intent(in) :: state_cnt
        !> northward_wind
        integer, intent(out) :: array_shape(3)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        if (state_ptr%p%v_grid_initialized) then
            array_shape=shape(state_ptr%p%v_grid)
        else
            array_shape = 0             
        endif
    end subroutine

    subroutine get_t_grid(state_cnt, t_grid_out )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> air_temperature
        real(8), intent(out) :: t_grid_out(ix, il, kx)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        t_grid_out=state_ptr%p%t_grid
    end subroutine

    subroutine set_t_grid(state_cnt, t_grid_in )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> air_temperature
        real(8), intent(in) :: t_grid_in(ix, il, kx)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        state_ptr%p%t_grid=t_grid_in
    end subroutine
    
    subroutine get_t_grid_shape(state_cnt, array_shape)
        use params
        integer(8), intent(in) :: state_cnt
        !> air_temperature
        integer, intent(out) :: array_shape(3)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        if (state_ptr%p%t_grid_initialized) then
            array_shape=shape(state_ptr%p%t_grid)
        else
            array_shape = 0             
        endif
    end subroutine

    subroutine get_q_grid(state_cnt, q_grid_out )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> specific_humidity
        real(8), intent(out) :: q_grid_out(ix, il, kx)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        q_grid_out=state_ptr%p%q_grid
    end subroutine

    subroutine set_q_grid(state_cnt, q_grid_in )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> specific_humidity
        real(8), intent(in) :: q_grid_in(ix, il, kx)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        state_ptr%p%q_grid=q_grid_in
    end subroutine
    
    subroutine get_q_grid_shape(state_cnt, array_shape)
        use params
        integer(8), intent(in) :: state_cnt
        !> specific_humidity
        integer, intent(out) :: array_shape(3)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        if (state_ptr%p%q_grid_initialized) then
            array_shape=shape(state_ptr%p%q_grid)
        else
            array_shape = 0             
        endif
    end subroutine

    subroutine get_phi_grid(state_cnt, phi_grid_out )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> geopotential_height
        real(8), intent(out) :: phi_grid_out(ix, il, kx)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        phi_grid_out=state_ptr%p%phi_grid
    end subroutine

    subroutine set_phi_grid(state_cnt, phi_grid_in )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> geopotential_height
        real(8), intent(in) :: phi_grid_in(ix, il, kx)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        state_ptr%p%phi_grid=phi_grid_in
    end subroutine
    
    subroutine get_phi_grid_shape(state_cnt, array_shape)
        use params
        integer(8), intent(in) :: state_cnt
        !> geopotential_height
        integer, intent(out) :: array_shape(3)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        if (state_ptr%p%phi_grid_initialized) then
            array_shape=shape(state_ptr%p%phi_grid)
        else
            array_shape = 0             
        endif
    end subroutine

    subroutine get_ps_grid(state_cnt, ps_grid_out )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> surface_air_pressure
        real(8), intent(out) :: ps_grid_out(ix, il)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        ps_grid_out=state_ptr%p%ps_grid
    end subroutine

    subroutine set_ps_grid(state_cnt, ps_grid_in )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> surface_air_pressure
        real(8), intent(in) :: ps_grid_in(ix, il)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        state_ptr%p%ps_grid=ps_grid_in
    end subroutine
    
    subroutine get_ps_grid_shape(state_cnt, array_shape)
        use params
        integer(8), intent(in) :: state_cnt
        !> surface_air_pressure
        integer, intent(out) :: array_shape(2)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        if (state_ptr%p%ps_grid_initialized) then
            array_shape=shape(state_ptr%p%ps_grid)
        else
            array_shape = 0             
        endif
    end subroutine

    subroutine get_precnv(state_cnt, precnv_out )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> Convective precipitation, total
        real(8), intent(out) :: precnv_out(ix, il)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        precnv_out=state_ptr%p%precnv
    end subroutine

    subroutine set_precnv(state_cnt, precnv_in )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> Convective precipitation, total
        real(8), intent(in) :: precnv_in(ix, il)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        state_ptr%p%precnv=precnv_in
    end subroutine
    
    subroutine get_precnv_shape(state_cnt, array_shape)
        use params
        integer(8), intent(in) :: state_cnt
        !> Convective precipitation, total
        integer, intent(out) :: array_shape(2)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        if (state_ptr%p%precnv_initialized) then
            array_shape=shape(state_ptr%p%precnv)
        else
            array_shape = 0             
        endif
    end subroutine

    subroutine get_precls(state_cnt, precls_out )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> Large-scale precipitation, total
        real(8), intent(out) :: precls_out(ix, il)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        precls_out=state_ptr%p%precls
    end subroutine

    subroutine set_precls(state_cnt, precls_in )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> Large-scale precipitation, total
        real(8), intent(in) :: precls_in(ix, il)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        state_ptr%p%precls=precls_in
    end subroutine
    
    subroutine get_precls_shape(state_cnt, array_shape)
        use params
        integer(8), intent(in) :: state_cnt
        !> Large-scale precipitation, total
        integer, intent(out) :: array_shape(2)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        if (state_ptr%p%precls_initialized) then
            array_shape=shape(state_ptr%p%precls)
        else
            array_shape = 0             
        endif
    end subroutine

    subroutine get_snowcv(state_cnt, snowcv_out )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> Convective precipitation, snow only
        real(8), intent(out) :: snowcv_out(ix, il)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        snowcv_out=state_ptr%p%snowcv
    end subroutine

    subroutine set_snowcv(state_cnt, snowcv_in )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> Convective precipitation, snow only
        real(8), intent(in) :: snowcv_in(ix, il)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        state_ptr%p%snowcv=snowcv_in
    end subroutine
    
    subroutine get_snowcv_shape(state_cnt, array_shape)
        use params
        integer(8), intent(in) :: state_cnt
        !> Convective precipitation, snow only
        integer, intent(out) :: array_shape(2)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        if (state_ptr%p%snowcv_initialized) then
            array_shape=shape(state_ptr%p%snowcv)
        else
            array_shape = 0             
        endif
    end subroutine

    subroutine get_snowls(state_cnt, snowls_out )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> Large-scale precipitation, snow only
        real(8), intent(out) :: snowls_out(ix, il)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        snowls_out=state_ptr%p%snowls
    end subroutine

    subroutine set_snowls(state_cnt, snowls_in )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> Large-scale precipitation, snow only
        real(8), intent(in) :: snowls_in(ix, il)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        state_ptr%p%snowls=snowls_in
    end subroutine
    
    subroutine get_snowls_shape(state_cnt, array_shape)
        use params
        integer(8), intent(in) :: state_cnt
        !> Large-scale precipitation, snow only
        integer, intent(out) :: array_shape(2)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        if (state_ptr%p%snowls_initialized) then
            array_shape=shape(state_ptr%p%snowls)
        else
            array_shape = 0             
        endif
    end subroutine

    subroutine get_cbmf(state_cnt, cbmf_out )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> Cloud-base mass flux
        real(8), intent(out) :: cbmf_out(ix, il)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        cbmf_out=state_ptr%p%cbmf
    end subroutine

    subroutine set_cbmf(state_cnt, cbmf_in )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> Cloud-base mass flux
        real(8), intent(in) :: cbmf_in(ix, il)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        state_ptr%p%cbmf=cbmf_in
    end subroutine
    
    subroutine get_cbmf_shape(state_cnt, array_shape)
        use params
        integer(8), intent(in) :: state_cnt
        !> Cloud-base mass flux
        integer, intent(out) :: array_shape(2)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        if (state_ptr%p%cbmf_initialized) then
            array_shape=shape(state_ptr%p%cbmf)
        else
            array_shape = 0             
        endif
    end subroutine

    subroutine get_tsr(state_cnt, tsr_out )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> Top-of-atmosphere shortwave radiation (downward)
        real(8), intent(out) :: tsr_out(ix, il)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        tsr_out=state_ptr%p%tsr
    end subroutine

    subroutine set_tsr(state_cnt, tsr_in )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> Top-of-atmosphere shortwave radiation (downward)
        real(8), intent(in) :: tsr_in(ix, il)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        state_ptr%p%tsr=tsr_in
    end subroutine
    
    subroutine get_tsr_shape(state_cnt, array_shape)
        use params
        integer(8), intent(in) :: state_cnt
        !> Top-of-atmosphere shortwave radiation (downward)
        integer, intent(out) :: array_shape(2)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        if (state_ptr%p%tsr_initialized) then
            array_shape=shape(state_ptr%p%tsr)
        else
            array_shape = 0             
        endif
    end subroutine

    subroutine get_ssrd(state_cnt, ssrd_out )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> Surface shortwave radiation (downward-only)
        real(8), intent(out) :: ssrd_out(ix, il)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        ssrd_out=state_ptr%p%ssrd
    end subroutine

    subroutine set_ssrd(state_cnt, ssrd_in )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> Surface shortwave radiation (downward-only)
        real(8), intent(in) :: ssrd_in(ix, il)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        state_ptr%p%ssrd=ssrd_in
    end subroutine
    
    subroutine get_ssrd_shape(state_cnt, array_shape)
        use params
        integer(8), intent(in) :: state_cnt
        !> Surface shortwave radiation (downward-only)
        integer, intent(out) :: array_shape(2)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        if (state_ptr%p%ssrd_initialized) then
            array_shape=shape(state_ptr%p%ssrd)
        else
            array_shape = 0             
        endif
    end subroutine

    subroutine get_ssr(state_cnt, ssr_out )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> Surface shortwave radiation (net downward)
        real(8), intent(out) :: ssr_out(ix, il)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        ssr_out=state_ptr%p%ssr
    end subroutine

    subroutine set_ssr(state_cnt, ssr_in )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> Surface shortwave radiation (net downward)
        real(8), intent(in) :: ssr_in(ix, il)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        state_ptr%p%ssr=ssr_in
    end subroutine
    
    subroutine get_ssr_shape(state_cnt, array_shape)
        use params
        integer(8), intent(in) :: state_cnt
        !> Surface shortwave radiation (net downward)
        integer, intent(out) :: array_shape(2)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        if (state_ptr%p%ssr_initialized) then
            array_shape=shape(state_ptr%p%ssr)
        else
            array_shape = 0             
        endif
    end subroutine

    subroutine get_slrd(state_cnt, slrd_out )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> Surface longwave radiation (downward-only)
        real(8), intent(out) :: slrd_out(ix, il)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        slrd_out=state_ptr%p%slrd
    end subroutine

    subroutine set_slrd(state_cnt, slrd_in )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> Surface longwave radiation (downward-only)
        real(8), intent(in) :: slrd_in(ix, il)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        state_ptr%p%slrd=slrd_in
    end subroutine
    
    subroutine get_slrd_shape(state_cnt, array_shape)
        use params
        integer(8), intent(in) :: state_cnt
        !> Surface longwave radiation (downward-only)
        integer, intent(out) :: array_shape(2)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        if (state_ptr%p%slrd_initialized) then
            array_shape=shape(state_ptr%p%slrd)
        else
            array_shape = 0             
        endif
    end subroutine

    subroutine get_slr(state_cnt, slr_out )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> Surface longwave radiation (net upward)
        real(8), intent(out) :: slr_out(ix, il)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        slr_out=state_ptr%p%slr
    end subroutine

    subroutine set_slr(state_cnt, slr_in )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> Surface longwave radiation (net upward)
        real(8), intent(in) :: slr_in(ix, il)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        state_ptr%p%slr=slr_in
    end subroutine
    
    subroutine get_slr_shape(state_cnt, array_shape)
        use params
        integer(8), intent(in) :: state_cnt
        !> Surface longwave radiation (net upward)
        integer, intent(out) :: array_shape(2)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        if (state_ptr%p%slr_initialized) then
            array_shape=shape(state_ptr%p%slr)
        else
            array_shape = 0             
        endif
    end subroutine

    subroutine get_olr(state_cnt, olr_out )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> Outgoing longwave radiation (upward)
        real(8), intent(out) :: olr_out(ix, il)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        olr_out=state_ptr%p%olr
    end subroutine

    subroutine set_olr(state_cnt, olr_in )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> Outgoing longwave radiation (upward)
        real(8), intent(in) :: olr_in(ix, il)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        state_ptr%p%olr=olr_in
    end subroutine
    
    subroutine get_olr_shape(state_cnt, array_shape)
        use params
        integer(8), intent(in) :: state_cnt
        !> Outgoing longwave radiation (upward)
        integer, intent(out) :: array_shape(2)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        if (state_ptr%p%olr_initialized) then
            array_shape=shape(state_ptr%p%olr)
        else
            array_shape = 0             
        endif
    end subroutine

    subroutine get_slru(state_cnt, slru_out )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> Surface longwave emission (upward)
        real(8), intent(out) :: slru_out(ix, il,aux_dim)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        slru_out=state_ptr%p%slru
    end subroutine

    subroutine set_slru(state_cnt, slru_in )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> Surface longwave emission (upward)
        real(8), intent(in) :: slru_in(ix, il,aux_dim)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        state_ptr%p%slru=slru_in
    end subroutine
    
    subroutine get_slru_shape(state_cnt, array_shape)
        use params
        integer(8), intent(in) :: state_cnt
        !> Surface longwave emission (upward)
        integer, intent(out) :: array_shape(3)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        if (state_ptr%p%slru_initialized) then
            array_shape=shape(state_ptr%p%slru)
        else
            array_shape = 0             
        endif
    end subroutine

    subroutine get_ustr(state_cnt, ustr_out )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> U-stress
        real(8), intent(out) :: ustr_out(ix, il,aux_dim)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        ustr_out=state_ptr%p%ustr
    end subroutine

    subroutine set_ustr(state_cnt, ustr_in )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> U-stress
        real(8), intent(in) :: ustr_in(ix, il,aux_dim)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        state_ptr%p%ustr=ustr_in
    end subroutine
    
    subroutine get_ustr_shape(state_cnt, array_shape)
        use params
        integer(8), intent(in) :: state_cnt
        !> U-stress
        integer, intent(out) :: array_shape(3)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        if (state_ptr%p%ustr_initialized) then
            array_shape=shape(state_ptr%p%ustr)
        else
            array_shape = 0             
        endif
    end subroutine

    subroutine get_vstr(state_cnt, vstr_out )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> Vstress
        real(8), intent(out) :: vstr_out(ix, il,aux_dim)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        vstr_out=state_ptr%p%vstr
    end subroutine

    subroutine set_vstr(state_cnt, vstr_in )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> Vstress
        real(8), intent(in) :: vstr_in(ix, il,aux_dim)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        state_ptr%p%vstr=vstr_in
    end subroutine
    
    subroutine get_vstr_shape(state_cnt, array_shape)
        use params
        integer(8), intent(in) :: state_cnt
        !> Vstress
        integer, intent(out) :: array_shape(3)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        if (state_ptr%p%vstr_initialized) then
            array_shape=shape(state_ptr%p%vstr)
        else
            array_shape = 0             
        endif
    end subroutine

    subroutine get_shf(state_cnt, shf_out )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> Sensible heat flux
        real(8), intent(out) :: shf_out(ix, il,aux_dim)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        shf_out=state_ptr%p%shf
    end subroutine

    subroutine set_shf(state_cnt, shf_in )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> Sensible heat flux
        real(8), intent(in) :: shf_in(ix, il,aux_dim)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        state_ptr%p%shf=shf_in
    end subroutine
    
    subroutine get_shf_shape(state_cnt, array_shape)
        use params
        integer(8), intent(in) :: state_cnt
        !> Sensible heat flux
        integer, intent(out) :: array_shape(3)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        if (state_ptr%p%shf_initialized) then
            array_shape=shape(state_ptr%p%shf)
        else
            array_shape = 0             
        endif
    end subroutine

    subroutine get_evap(state_cnt, evap_out )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> Evaporation
        real(8), intent(out) :: evap_out(ix, il,aux_dim)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        evap_out=state_ptr%p%evap
    end subroutine

    subroutine set_evap(state_cnt, evap_in )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> Evaporation
        real(8), intent(in) :: evap_in(ix, il,aux_dim)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        state_ptr%p%evap=evap_in
    end subroutine
    
    subroutine get_evap_shape(state_cnt, array_shape)
        use params
        integer(8), intent(in) :: state_cnt
        !> Evaporation
        integer, intent(out) :: array_shape(3)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        if (state_ptr%p%evap_initialized) then
            array_shape=shape(state_ptr%p%evap)
        else
            array_shape = 0             
        endif
    end subroutine

    subroutine get_hfluxn(state_cnt, hfluxn_out )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> Net heat flux into surface
        real(8), intent(out) :: hfluxn_out(ix, il,aux_dim)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        hfluxn_out=state_ptr%p%hfluxn
    end subroutine

    subroutine set_hfluxn(state_cnt, hfluxn_in )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> Net heat flux into surface
        real(8), intent(in) :: hfluxn_in(ix, il,aux_dim)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        state_ptr%p%hfluxn=hfluxn_in
    end subroutine
    
    subroutine get_hfluxn_shape(state_cnt, array_shape)
        use params
        integer(8), intent(in) :: state_cnt
        !> Net heat flux into surface
        integer, intent(out) :: array_shape(3)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        if (state_ptr%p%hfluxn_initialized) then
            array_shape=shape(state_ptr%p%hfluxn)
        else
            array_shape = 0             
        endif
    end subroutine

    subroutine get_tt_rsw(state_cnt, tt_rsw_out )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> Flux of short-wave radiation absorbed in each atmospheric layer
        real(8), intent(out) :: tt_rsw_out(ix, il,kx)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        tt_rsw_out=state_ptr%p%tt_rsw
    end subroutine

    subroutine set_tt_rsw(state_cnt, tt_rsw_in )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> Flux of short-wave radiation absorbed in each atmospheric layer
        real(8), intent(in) :: tt_rsw_in(ix, il,kx)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        state_ptr%p%tt_rsw=tt_rsw_in
    end subroutine
    
    subroutine get_tt_rsw_shape(state_cnt, array_shape)
        use params
        integer(8), intent(in) :: state_cnt
        !> Flux of short-wave radiation absorbed in each atmospheric layer
        integer, intent(out) :: array_shape(3)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        if (state_ptr%p%tt_rsw_initialized) then
            array_shape=shape(state_ptr%p%tt_rsw)
        else
            array_shape = 0             
        endif
    end subroutine

    subroutine get_phi0(state_cnt, phi0_out )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> Unfiltered surface geopotential
        real(8), intent(out) :: phi0_out(ix, il)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        phi0_out=state_ptr%p%phi0
    end subroutine

    subroutine set_phi0(state_cnt, phi0_in )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> Unfiltered surface geopotential
        real(8), intent(in) :: phi0_in(ix, il)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        state_ptr%p%phi0=phi0_in
    end subroutine
    
    subroutine get_phi0_shape(state_cnt, array_shape)
        use params
        integer(8), intent(in) :: state_cnt
        !> Unfiltered surface geopotential
        integer, intent(out) :: array_shape(2)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        if (state_ptr%p%phi0_initialized) then
            array_shape=shape(state_ptr%p%phi0)
        else
            array_shape = 0             
        endif
    end subroutine

    subroutine get_orog(state_cnt, orog_out )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> Orography
        real(8), intent(out) :: orog_out(ix, il)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        orog_out=state_ptr%p%orog
    end subroutine

    subroutine set_orog(state_cnt, orog_in )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> Orography
        real(8), intent(in) :: orog_in(ix, il)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        state_ptr%p%orog=orog_in
    end subroutine
    
    subroutine get_orog_shape(state_cnt, array_shape)
        use params
        integer(8), intent(in) :: state_cnt
        !> Orography
        integer, intent(out) :: array_shape(2)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        if (state_ptr%p%orog_initialized) then
            array_shape=shape(state_ptr%p%orog)
        else
            array_shape = 0             
        endif
    end subroutine

    subroutine get_phis0(state_cnt, phis0_out )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> Spectrally-filtered surface geopotential
        real(8), intent(out) :: phis0_out(ix, il)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        phis0_out=state_ptr%p%phis0
    end subroutine

    subroutine set_phis0(state_cnt, phis0_in )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> Spectrally-filtered surface geopotential
        real(8), intent(in) :: phis0_in(ix, il)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        state_ptr%p%phis0=phis0_in
    end subroutine
    
    subroutine get_phis0_shape(state_cnt, array_shape)
        use params
        integer(8), intent(in) :: state_cnt
        !> Spectrally-filtered surface geopotential
        integer, intent(out) :: array_shape(2)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        if (state_ptr%p%phis0_initialized) then
            array_shape=shape(state_ptr%p%phis0)
        else
            array_shape = 0             
        endif
    end subroutine

    subroutine get_alb0(state_cnt, alb0_out )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> Bare-land annual-mean albedo
        real(8), intent(out) :: alb0_out(ix, il)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        alb0_out=state_ptr%p%alb0
    end subroutine

    subroutine set_alb0(state_cnt, alb0_in )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> Bare-land annual-mean albedo
        real(8), intent(in) :: alb0_in(ix, il)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        state_ptr%p%alb0=alb0_in
    end subroutine
    
    subroutine get_alb0_shape(state_cnt, array_shape)
        use params
        integer(8), intent(in) :: state_cnt
        !> Bare-land annual-mean albedo
        integer, intent(out) :: array_shape(2)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        if (state_ptr%p%alb0_initialized) then
            array_shape=shape(state_ptr%p%alb0)
        else
            array_shape = 0             
        endif
    end subroutine

    subroutine get_forog(state_cnt, forog_out )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> Orographic factor for land surface drag
        real(8), intent(out) :: forog_out(ix, il)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        forog_out=state_ptr%p%forog
    end subroutine

    subroutine set_forog(state_cnt, forog_in )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> Orographic factor for land surface drag
        real(8), intent(in) :: forog_in(ix, il)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        state_ptr%p%forog=forog_in
    end subroutine
    
    subroutine get_forog_shape(state_cnt, array_shape)
        use params
        integer(8), intent(in) :: state_cnt
        !> Orographic factor for land surface drag
        integer, intent(out) :: array_shape(2)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        if (state_ptr%p%forog_initialized) then
            array_shape=shape(state_ptr%p%forog)
        else
            array_shape = 0             
        endif
    end subroutine

    subroutine get_fmask_orig(state_cnt, fmask_orig_out )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> Original (fractional) land-sea mask
        real(8), intent(out) :: fmask_orig_out(ix, il)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        fmask_orig_out=state_ptr%p%fmask_orig
    end subroutine

    subroutine set_fmask_orig(state_cnt, fmask_orig_in )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> Original (fractional) land-sea mask
        real(8), intent(in) :: fmask_orig_in(ix, il)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        state_ptr%p%fmask_orig=fmask_orig_in
    end subroutine
    
    subroutine get_fmask_orig_shape(state_cnt, array_shape)
        use params
        integer(8), intent(in) :: state_cnt
        !> Original (fractional) land-sea mask
        integer, intent(out) :: array_shape(2)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        if (state_ptr%p%fmask_orig_initialized) then
            array_shape=shape(state_ptr%p%fmask_orig)
        else
            array_shape = 0             
        endif
    end subroutine

    subroutine get_xgeop1(state_cnt, xgeop1_out )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> Constant 1 for hydrostatic equation
        real(8), intent(out) :: xgeop1_out(kx)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        xgeop1_out=state_ptr%p%xgeop1
    end subroutine

    subroutine set_xgeop1(state_cnt, xgeop1_in )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> Constant 1 for hydrostatic equation
        real(8), intent(in) :: xgeop1_in(kx)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        state_ptr%p%xgeop1=xgeop1_in
    end subroutine
    
    subroutine get_xgeop1_shape(state_cnt, array_shape)
        use params
        integer(8), intent(in) :: state_cnt
        !> Constant 1 for hydrostatic equation
        integer, intent(out) :: array_shape(1)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        if (state_ptr%p%xgeop1_initialized) then
            array_shape=shape(state_ptr%p%xgeop1)
        else
            array_shape = 0             
        endif
    end subroutine

    subroutine get_xgeop2(state_cnt, xgeop2_out )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> Constant 2 for hydrostatic equation
        real(8), intent(out) :: xgeop2_out(kx)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        xgeop2_out=state_ptr%p%xgeop2
    end subroutine

    subroutine set_xgeop2(state_cnt, xgeop2_in )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> Constant 2 for hydrostatic equation
        real(8), intent(in) :: xgeop2_in(kx)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        state_ptr%p%xgeop2=xgeop2_in
    end subroutine
    
    subroutine get_xgeop2_shape(state_cnt, array_shape)
        use params
        integer(8), intent(in) :: state_cnt
        !> Constant 2 for hydrostatic equation
        integer, intent(out) :: array_shape(1)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        if (state_ptr%p%xgeop2_initialized) then
            array_shape=shape(state_ptr%p%xgeop2)
        else
            array_shape = 0             
        endif
    end subroutine

    subroutine get_stl12(state_cnt, stl12_out )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> Land surface temperature monthly-mean climatology
        real(8), intent(out) :: stl12_out(ix, il, 12)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        stl12_out=state_ptr%p%stl12
    end subroutine

    subroutine set_stl12(state_cnt, stl12_in )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> Land surface temperature monthly-mean climatology
        real(8), intent(in) :: stl12_in(ix, il, 12)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        state_ptr%p%stl12=stl12_in
    end subroutine
    
    subroutine get_stl12_shape(state_cnt, array_shape)
        use params
        integer(8), intent(in) :: state_cnt
        !> Land surface temperature monthly-mean climatology
        integer, intent(out) :: array_shape(3)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        if (state_ptr%p%stl12_initialized) then
            array_shape=shape(state_ptr%p%stl12)
        else
            array_shape = 0             
        endif
    end subroutine

    subroutine get_snowd12(state_cnt, snowd12_out )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> Snow depth (water equivalent) monthly-mean climatology
        real(8), intent(out) :: snowd12_out(ix, il, 12)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        snowd12_out=state_ptr%p%snowd12
    end subroutine

    subroutine set_snowd12(state_cnt, snowd12_in )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> Snow depth (water equivalent) monthly-mean climatology
        real(8), intent(in) :: snowd12_in(ix, il, 12)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        state_ptr%p%snowd12=snowd12_in
    end subroutine
    
    subroutine get_snowd12_shape(state_cnt, array_shape)
        use params
        integer(8), intent(in) :: state_cnt
        !> Snow depth (water equivalent) monthly-mean climatology
        integer, intent(out) :: array_shape(3)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        if (state_ptr%p%snowd12_initialized) then
            array_shape=shape(state_ptr%p%snowd12)
        else
            array_shape = 0             
        endif
    end subroutine

    subroutine get_soilw12(state_cnt, soilw12_out )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> Soil water availability monthly-mean climatology
        real(8), intent(out) :: soilw12_out(ix, il, 12)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        soilw12_out=state_ptr%p%soilw12
    end subroutine

    subroutine set_soilw12(state_cnt, soilw12_in )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> Soil water availability monthly-mean climatology
        real(8), intent(in) :: soilw12_in(ix, il, 12)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        state_ptr%p%soilw12=soilw12_in
    end subroutine
    
    subroutine get_soilw12_shape(state_cnt, array_shape)
        use params
        integer(8), intent(in) :: state_cnt
        !> Soil water availability monthly-mean climatology
        integer, intent(out) :: array_shape(3)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        if (state_ptr%p%soilw12_initialized) then
            array_shape=shape(state_ptr%p%soilw12)
        else
            array_shape = 0             
        endif
    end subroutine

    subroutine get_veg_low(state_cnt, veg_low_out )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> Low vegetation fraction
        real(8), intent(out) :: veg_low_out(ix, il)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        veg_low_out=state_ptr%p%veg_low
    end subroutine

    subroutine set_veg_low(state_cnt, veg_low_in )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> Low vegetation fraction
        real(8), intent(in) :: veg_low_in(ix, il)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        state_ptr%p%veg_low=veg_low_in
    end subroutine
    
    subroutine get_veg_low_shape(state_cnt, array_shape)
        use params
        integer(8), intent(in) :: state_cnt
        !> Low vegetation fraction
        integer, intent(out) :: array_shape(2)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        if (state_ptr%p%veg_low_initialized) then
            array_shape=shape(state_ptr%p%veg_low)
        else
            array_shape = 0             
        endif
    end subroutine

    subroutine get_veg_high(state_cnt, veg_high_out )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> High vegetation fraction
        real(8), intent(out) :: veg_high_out(ix, il)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        veg_high_out=state_ptr%p%veg_high
    end subroutine

    subroutine set_veg_high(state_cnt, veg_high_in )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> High vegetation fraction
        real(8), intent(in) :: veg_high_in(ix, il)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        state_ptr%p%veg_high=veg_high_in
    end subroutine
    
    subroutine get_veg_high_shape(state_cnt, array_shape)
        use params
        integer(8), intent(in) :: state_cnt
        !> High vegetation fraction
        integer, intent(out) :: array_shape(2)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        if (state_ptr%p%veg_high_initialized) then
            array_shape=shape(state_ptr%p%veg_high)
        else
            array_shape = 0             
        endif
    end subroutine

    subroutine get_soil_wc_l1(state_cnt, soil_wc_l1_out )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> Soil water content: Layer 1
        real(8), intent(out) :: soil_wc_l1_out(ix, il, 12)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        soil_wc_l1_out=state_ptr%p%soil_wc_l1
    end subroutine

    subroutine set_soil_wc_l1(state_cnt, soil_wc_l1_in )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> Soil water content: Layer 1
        real(8), intent(in) :: soil_wc_l1_in(ix, il, 12)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        state_ptr%p%soil_wc_l1=soil_wc_l1_in
    end subroutine
    
    subroutine get_soil_wc_l1_shape(state_cnt, array_shape)
        use params
        integer(8), intent(in) :: state_cnt
        !> Soil water content: Layer 1
        integer, intent(out) :: array_shape(3)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        if (state_ptr%p%soil_wc_l1_initialized) then
            array_shape=shape(state_ptr%p%soil_wc_l1)
        else
            array_shape = 0             
        endif
    end subroutine

    subroutine get_soil_wc_l2(state_cnt, soil_wc_l2_out )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> Soil water content: Layer 2
        real(8), intent(out) :: soil_wc_l2_out(ix, il, 12)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        soil_wc_l2_out=state_ptr%p%soil_wc_l2
    end subroutine

    subroutine set_soil_wc_l2(state_cnt, soil_wc_l2_in )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> Soil water content: Layer 2
        real(8), intent(in) :: soil_wc_l2_in(ix, il, 12)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        state_ptr%p%soil_wc_l2=soil_wc_l2_in
    end subroutine
    
    subroutine get_soil_wc_l2_shape(state_cnt, array_shape)
        use params
        integer(8), intent(in) :: state_cnt
        !> Soil water content: Layer 2
        integer, intent(out) :: array_shape(3)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        if (state_ptr%p%soil_wc_l2_initialized) then
            array_shape=shape(state_ptr%p%soil_wc_l2)
        else
            array_shape = 0             
        endif
    end subroutine

    subroutine get_soil_wc_l3(state_cnt, soil_wc_l3_out )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> Soil water content: Layer 3
        real(8), intent(out) :: soil_wc_l3_out(ix, il, 12)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        soil_wc_l3_out=state_ptr%p%soil_wc_l3
    end subroutine

    subroutine set_soil_wc_l3(state_cnt, soil_wc_l3_in )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> Soil water content: Layer 3
        real(8), intent(in) :: soil_wc_l3_in(ix, il, 12)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        state_ptr%p%soil_wc_l3=soil_wc_l3_in
    end subroutine
    
    subroutine get_soil_wc_l3_shape(state_cnt, array_shape)
        use params
        integer(8), intent(in) :: state_cnt
        !> Soil water content: Layer 3
        integer, intent(out) :: array_shape(3)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        if (state_ptr%p%soil_wc_l3_initialized) then
            array_shape=shape(state_ptr%p%soil_wc_l3)
        else
            array_shape = 0             
        endif
    end subroutine

    subroutine get_sst12(state_cnt, sst12_out )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> Sea/ice surface temperature
        real(8), intent(out) :: sst12_out(ix, il, 12)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        sst12_out=state_ptr%p%sst12
    end subroutine

    subroutine set_sst12(state_cnt, sst12_in )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> Sea/ice surface temperature
        real(8), intent(in) :: sst12_in(ix, il, 12)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        state_ptr%p%sst12=sst12_in
    end subroutine
    
    subroutine get_sst12_shape(state_cnt, array_shape)
        use params
        integer(8), intent(in) :: state_cnt
        !> Sea/ice surface temperature
        integer, intent(out) :: array_shape(3)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        if (state_ptr%p%sst12_initialized) then
            array_shape=shape(state_ptr%p%sst12)
        else
            array_shape = 0             
        endif
    end subroutine

    subroutine get_sea_ice_frac12(state_cnt, sea_ice_frac12_out )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> Sea ice fraction
        real(8), intent(out) :: sea_ice_frac12_out(ix, il, 12)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        sea_ice_frac12_out=state_ptr%p%sea_ice_frac12
    end subroutine

    subroutine set_sea_ice_frac12(state_cnt, sea_ice_frac12_in )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> Sea ice fraction
        real(8), intent(in) :: sea_ice_frac12_in(ix, il, 12)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        state_ptr%p%sea_ice_frac12=sea_ice_frac12_in
    end subroutine
    
    subroutine get_sea_ice_frac12_shape(state_cnt, array_shape)
        use params
        integer(8), intent(in) :: state_cnt
        !> Sea ice fraction
        integer, intent(out) :: array_shape(3)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        if (state_ptr%p%sea_ice_frac12_initialized) then
            array_shape=shape(state_ptr%p%sea_ice_frac12)
        else
            array_shape = 0             
        endif
    end subroutine

    subroutine get_sst_anom(state_cnt, sst_anom_out , n_months )
        use params
        integer(8), intent(in) :: state_cnt
        integer, intent(in) :: n_months        
        !> Observed SST anomaly (input).
        real(8), intent(out) :: sst_anom_out(ix, il, 0:n_months+1)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        sst_anom_out=state_ptr%p%sst_anom
    end subroutine

    subroutine set_sst_anom(state_cnt, sst_anom_in , n_months )
        use params
        integer(8), intent(in) :: state_cnt
        integer, intent(in) :: n_months        
        !> Observed SST anomaly (input).
        real(8), intent(in) :: sst_anom_in(ix, il, 0:n_months+1)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        state_ptr%p%sst_anom=sst_anom_in
    end subroutine
    
    subroutine get_sst_anom_shape(state_cnt, array_shape)
        use params
        integer(8), intent(in) :: state_cnt
        !> Observed SST anomaly (input).
        integer, intent(out) :: array_shape(3)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        if (state_ptr%p%sst_anom_initialized) then
            array_shape=shape(state_ptr%p%sst_anom)
        else
            array_shape = 0             
        endif
    end subroutine

    subroutine get_flux_solar_in(state_cnt, flux_solar_in_out )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> Flux of incoming solar radiation
        real(8), intent(out) :: flux_solar_in_out(ix, il)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        flux_solar_in_out=state_ptr%p%flux_solar_in
    end subroutine

    subroutine set_flux_solar_in(state_cnt, flux_solar_in_in )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> Flux of incoming solar radiation
        real(8), intent(in) :: flux_solar_in_in(ix, il)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        state_ptr%p%flux_solar_in=flux_solar_in_in
    end subroutine
    
    subroutine get_flux_solar_in_shape(state_cnt, array_shape)
        use params
        integer(8), intent(in) :: state_cnt
        !> Flux of incoming solar radiation
        integer, intent(out) :: array_shape(2)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        if (state_ptr%p%flux_solar_in_initialized) then
            array_shape=shape(state_ptr%p%flux_solar_in)
        else
            array_shape = 0             
        endif
    end subroutine

    subroutine get_flux_ozone_lower(state_cnt, flux_ozone_lower_out )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> Flux absorbed by ozone (lower stratosphere)
        real(8), intent(out) :: flux_ozone_lower_out(ix, il)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        flux_ozone_lower_out=state_ptr%p%flux_ozone_lower
    end subroutine

    subroutine set_flux_ozone_lower(state_cnt, flux_ozone_lower_in )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> Flux absorbed by ozone (lower stratosphere)
        real(8), intent(in) :: flux_ozone_lower_in(ix, il)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        state_ptr%p%flux_ozone_lower=flux_ozone_lower_in
    end subroutine
    
    subroutine get_flux_ozone_lower_shape(state_cnt, array_shape)
        use params
        integer(8), intent(in) :: state_cnt
        !> Flux absorbed by ozone (lower stratosphere)
        integer, intent(out) :: array_shape(2)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        if (state_ptr%p%flux_ozone_lower_initialized) then
            array_shape=shape(state_ptr%p%flux_ozone_lower)
        else
            array_shape = 0             
        endif
    end subroutine

    subroutine get_flux_ozone_upper(state_cnt, flux_ozone_upper_out )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> Flux absorbed by ozone (upper stratosphere)
        real(8), intent(out) :: flux_ozone_upper_out(ix, il)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        flux_ozone_upper_out=state_ptr%p%flux_ozone_upper
    end subroutine

    subroutine set_flux_ozone_upper(state_cnt, flux_ozone_upper_in )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> Flux absorbed by ozone (upper stratosphere)
        real(8), intent(in) :: flux_ozone_upper_in(ix, il)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        state_ptr%p%flux_ozone_upper=flux_ozone_upper_in
    end subroutine
    
    subroutine get_flux_ozone_upper_shape(state_cnt, array_shape)
        use params
        integer(8), intent(in) :: state_cnt
        !> Flux absorbed by ozone (upper stratosphere)
        integer, intent(out) :: array_shape(2)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        if (state_ptr%p%flux_ozone_upper_initialized) then
            array_shape=shape(state_ptr%p%flux_ozone_upper)
        else
            array_shape = 0             
        endif
    end subroutine

    subroutine get_zenit_correction(state_cnt, zenit_correction_out )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> Zenith angle correction to (downward) absorptivity
        real(8), intent(out) :: zenit_correction_out(ix, il)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        zenit_correction_out=state_ptr%p%zenit_correction
    end subroutine

    subroutine set_zenit_correction(state_cnt, zenit_correction_in )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> Zenith angle correction to (downward) absorptivity
        real(8), intent(in) :: zenit_correction_in(ix, il)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        state_ptr%p%zenit_correction=zenit_correction_in
    end subroutine
    
    subroutine get_zenit_correction_shape(state_cnt, array_shape)
        use params
        integer(8), intent(in) :: state_cnt
        !> Zenith angle correction to (downward) absorptivity
        integer, intent(out) :: array_shape(2)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        if (state_ptr%p%zenit_correction_initialized) then
            array_shape=shape(state_ptr%p%zenit_correction)
        else
            array_shape = 0             
        endif
    end subroutine

    subroutine get_stratospheric_correction(state_cnt, stratospheric_correction_out )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> Stratospheric correction for polar night
        real(8), intent(out) :: stratospheric_correction_out(ix, il)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        stratospheric_correction_out=state_ptr%p%stratospheric_correction
    end subroutine

    subroutine set_stratospheric_correction(state_cnt, stratospheric_correction_in )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> Stratospheric correction for polar night
        real(8), intent(in) :: stratospheric_correction_in(ix, il)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        state_ptr%p%stratospheric_correction=stratospheric_correction_in
    end subroutine
    
    subroutine get_stratospheric_correction_shape(state_cnt, array_shape)
        use params
        integer(8), intent(in) :: state_cnt
        !> Stratospheric correction for polar night
        integer, intent(out) :: array_shape(2)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        if (state_ptr%p%stratospheric_correction_initialized) then
            array_shape=shape(state_ptr%p%stratospheric_correction)
        else
            array_shape = 0             
        endif
    end subroutine

    subroutine get_qcloud_equiv(state_cnt, qcloud_equiv_out )
        use params
        integer(8), intent(in) :: state_cnt
                
        !>  Equivalent specific humidity of clouds
        real(8), intent(out) :: qcloud_equiv_out(ix, il)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        qcloud_equiv_out=state_ptr%p%qcloud_equiv
    end subroutine

    subroutine set_qcloud_equiv(state_cnt, qcloud_equiv_in )
        use params
        integer(8), intent(in) :: state_cnt
                
        !>  Equivalent specific humidity of clouds
        real(8), intent(in) :: qcloud_equiv_in(ix, il)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        state_ptr%p%qcloud_equiv=qcloud_equiv_in
    end subroutine
    
    subroutine get_qcloud_equiv_shape(state_cnt, array_shape)
        use params
        integer(8), intent(in) :: state_cnt
        !>  Equivalent specific humidity of clouds
        integer, intent(out) :: array_shape(2)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        if (state_ptr%p%qcloud_equiv_initialized) then
            array_shape=shape(state_ptr%p%qcloud_equiv)
        else
            array_shape = 0             
        endif
    end subroutine

    subroutine get_rhcapl(state_cnt, rhcapl_out )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> 1/heat capacity (land)
        real(8), intent(out) :: rhcapl_out(ix, il)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        rhcapl_out=state_ptr%p%rhcapl
    end subroutine

    subroutine set_rhcapl(state_cnt, rhcapl_in )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> 1/heat capacity (land)
        real(8), intent(in) :: rhcapl_in(ix, il)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        state_ptr%p%rhcapl=rhcapl_in
    end subroutine
    
    subroutine get_rhcapl_shape(state_cnt, array_shape)
        use params
        integer(8), intent(in) :: state_cnt
        !> 1/heat capacity (land)
        integer, intent(out) :: array_shape(2)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        if (state_ptr%p%rhcapl_initialized) then
            array_shape=shape(state_ptr%p%rhcapl)
        else
            array_shape = 0             
        endif
    end subroutine

    subroutine get_cdland(state_cnt, cdland_out )
        use params
        integer(8), intent(in) :: state_cnt
                
        !>  1/dissipation time (land)
        real(8), intent(out) :: cdland_out(ix, il)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        cdland_out=state_ptr%p%cdland
    end subroutine

    subroutine set_cdland(state_cnt, cdland_in )
        use params
        integer(8), intent(in) :: state_cnt
                
        !>  1/dissipation time (land)
        real(8), intent(in) :: cdland_in(ix, il)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        state_ptr%p%cdland=cdland_in
    end subroutine
    
    subroutine get_cdland_shape(state_cnt, array_shape)
        use params
        integer(8), intent(in) :: state_cnt
        !>  1/dissipation time (land)
        integer, intent(out) :: array_shape(2)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        if (state_ptr%p%cdland_initialized) then
            array_shape=shape(state_ptr%p%cdland)
        else
            array_shape = 0             
        endif
    end subroutine

    subroutine get_stlcl_obs(state_cnt, stlcl_obs_out )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> Climatological land surface temperature
        real(8), intent(out) :: stlcl_obs_out(ix, il)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        stlcl_obs_out=state_ptr%p%stlcl_obs
    end subroutine

    subroutine set_stlcl_obs(state_cnt, stlcl_obs_in )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> Climatological land surface temperature
        real(8), intent(in) :: stlcl_obs_in(ix, il)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        state_ptr%p%stlcl_obs=stlcl_obs_in
    end subroutine
    
    subroutine get_stlcl_obs_shape(state_cnt, array_shape)
        use params
        integer(8), intent(in) :: state_cnt
        !> Climatological land surface temperature
        integer, intent(out) :: array_shape(2)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        if (state_ptr%p%stlcl_obs_initialized) then
            array_shape=shape(state_ptr%p%stlcl_obs)
        else
            array_shape = 0             
        endif
    end subroutine

    subroutine get_snowdcl_obs(state_cnt, snowdcl_obs_out )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> Climatological snow depth (water equivalent)
        real(8), intent(out) :: snowdcl_obs_out(ix, il)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        snowdcl_obs_out=state_ptr%p%snowdcl_obs
    end subroutine

    subroutine set_snowdcl_obs(state_cnt, snowdcl_obs_in )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> Climatological snow depth (water equivalent)
        real(8), intent(in) :: snowdcl_obs_in(ix, il)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        state_ptr%p%snowdcl_obs=snowdcl_obs_in
    end subroutine
    
    subroutine get_snowdcl_obs_shape(state_cnt, array_shape)
        use params
        integer(8), intent(in) :: state_cnt
        !> Climatological snow depth (water equivalent)
        integer, intent(out) :: array_shape(2)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        if (state_ptr%p%snowdcl_obs_initialized) then
            array_shape=shape(state_ptr%p%snowdcl_obs)
        else
            array_shape = 0             
        endif
    end subroutine

    subroutine get_soilwcl_obs(state_cnt, soilwcl_obs_out )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> Climatological soil water availability
        real(8), intent(out) :: soilwcl_obs_out(ix, il)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        soilwcl_obs_out=state_ptr%p%soilwcl_obs
    end subroutine

    subroutine set_soilwcl_obs(state_cnt, soilwcl_obs_in )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> Climatological soil water availability
        real(8), intent(in) :: soilwcl_obs_in(ix, il)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        state_ptr%p%soilwcl_obs=soilwcl_obs_in
    end subroutine
    
    subroutine get_soilwcl_obs_shape(state_cnt, array_shape)
        use params
        integer(8), intent(in) :: state_cnt
        !> Climatological soil water availability
        integer, intent(out) :: array_shape(2)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        if (state_ptr%p%soilwcl_obs_initialized) then
            array_shape=shape(state_ptr%p%soilwcl_obs)
        else
            array_shape = 0             
        endif
    end subroutine

    subroutine get_land_temp(state_cnt, land_temp_out )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> Land surface temperature
        real(8), intent(out) :: land_temp_out(ix, il)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        land_temp_out=state_ptr%p%land_temp
    end subroutine

    subroutine set_land_temp(state_cnt, land_temp_in )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> Land surface temperature
        real(8), intent(in) :: land_temp_in(ix, il)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        state_ptr%p%land_temp=land_temp_in
    end subroutine
    
    subroutine get_land_temp_shape(state_cnt, array_shape)
        use params
        integer(8), intent(in) :: state_cnt
        !> Land surface temperature
        integer, intent(out) :: array_shape(2)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        if (state_ptr%p%land_temp_initialized) then
            array_shape=shape(state_ptr%p%land_temp)
        else
            array_shape = 0             
        endif
    end subroutine

    subroutine get_snow_depth(state_cnt, snow_depth_out )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> Snow depth (water equivalent)
        real(8), intent(out) :: snow_depth_out(ix, il)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        snow_depth_out=state_ptr%p%snow_depth
    end subroutine

    subroutine set_snow_depth(state_cnt, snow_depth_in )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> Snow depth (water equivalent)
        real(8), intent(in) :: snow_depth_in(ix, il)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        state_ptr%p%snow_depth=snow_depth_in
    end subroutine
    
    subroutine get_snow_depth_shape(state_cnt, array_shape)
        use params
        integer(8), intent(in) :: state_cnt
        !> Snow depth (water equivalent)
        integer, intent(out) :: array_shape(2)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        if (state_ptr%p%snow_depth_initialized) then
            array_shape=shape(state_ptr%p%snow_depth)
        else
            array_shape = 0             
        endif
    end subroutine

    subroutine get_soil_avail_water(state_cnt, soil_avail_water_out )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> Soil water availability
        real(8), intent(out) :: soil_avail_water_out(ix, il)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        soil_avail_water_out=state_ptr%p%soil_avail_water
    end subroutine

    subroutine set_soil_avail_water(state_cnt, soil_avail_water_in )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> Soil water availability
        real(8), intent(in) :: soil_avail_water_in(ix, il)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        state_ptr%p%soil_avail_water=soil_avail_water_in
    end subroutine
    
    subroutine get_soil_avail_water_shape(state_cnt, array_shape)
        use params
        integer(8), intent(in) :: state_cnt
        !> Soil water availability
        integer, intent(out) :: array_shape(2)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        if (state_ptr%p%soil_avail_water_initialized) then
            array_shape=shape(state_ptr%p%soil_avail_water)
        else
            array_shape = 0             
        endif
    end subroutine

    subroutine get_stl_lm(state_cnt, stl_lm_out )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> Land-model surface temperature
        real(8), intent(out) :: stl_lm_out(ix, il)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        stl_lm_out=state_ptr%p%stl_lm
    end subroutine

    subroutine set_stl_lm(state_cnt, stl_lm_in )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> Land-model surface temperature
        real(8), intent(in) :: stl_lm_in(ix, il)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        state_ptr%p%stl_lm=stl_lm_in
    end subroutine
    
    subroutine get_stl_lm_shape(state_cnt, array_shape)
        use params
        integer(8), intent(in) :: state_cnt
        !> Land-model surface temperature
        integer, intent(out) :: array_shape(2)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        if (state_ptr%p%stl_lm_initialized) then
            array_shape=shape(state_ptr%p%stl_lm)
        else
            array_shape = 0             
        endif
    end subroutine

    subroutine get_fmask_land(state_cnt, fmask_land_out )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> Fraction of land
        real(8), intent(out) :: fmask_land_out(ix, il)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        fmask_land_out=state_ptr%p%fmask_land
    end subroutine

    subroutine set_fmask_land(state_cnt, fmask_land_in )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> Fraction of land
        real(8), intent(in) :: fmask_land_in(ix, il)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        state_ptr%p%fmask_land=fmask_land_in
    end subroutine
    
    subroutine get_fmask_land_shape(state_cnt, array_shape)
        use params
        integer(8), intent(in) :: state_cnt
        !> Fraction of land
        integer, intent(out) :: array_shape(2)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        if (state_ptr%p%fmask_land_initialized) then
            array_shape=shape(state_ptr%p%fmask_land)
        else
            array_shape = 0             
        endif
    end subroutine

    subroutine get_bmask_land(state_cnt, bmask_land_out )
        use params
        integer(8), intent(in) :: state_cnt
                
        !>  Binary land mask
        real(8), intent(out) :: bmask_land_out(ix, il)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        bmask_land_out=state_ptr%p%bmask_land
    end subroutine

    subroutine set_bmask_land(state_cnt, bmask_land_in )
        use params
        integer(8), intent(in) :: state_cnt
                
        !>  Binary land mask
        real(8), intent(in) :: bmask_land_in(ix, il)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        state_ptr%p%bmask_land=bmask_land_in
    end subroutine
    
    subroutine get_bmask_land_shape(state_cnt, array_shape)
        use params
        integer(8), intent(in) :: state_cnt
        !>  Binary land mask
        integer, intent(out) :: array_shape(2)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        if (state_ptr%p%bmask_land_initialized) then
            array_shape=shape(state_ptr%p%bmask_land)
        else
            array_shape = 0             
        endif
    end subroutine

    subroutine get_rhcaps(state_cnt, rhcaps_out )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> 1./heat_capacity (sea)
        real(p), intent(out) :: rhcaps_out(ix, il)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        rhcaps_out=state_ptr%p%rhcaps
    end subroutine

    subroutine set_rhcaps(state_cnt, rhcaps_in )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> 1./heat_capacity (sea)
        real(p), intent(in) :: rhcaps_in(ix, il)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        state_ptr%p%rhcaps=rhcaps_in
    end subroutine
    
    subroutine get_rhcaps_shape(state_cnt, array_shape)
        use params
        integer(8), intent(in) :: state_cnt
        !> 1./heat_capacity (sea)
        integer, intent(out) :: array_shape(2)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        if (state_ptr%p%rhcaps_initialized) then
            array_shape=shape(state_ptr%p%rhcaps)
        else
            array_shape = 0             
        endif
    end subroutine

    subroutine get_rhcapi(state_cnt, rhcapi_out )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> 1./heat_capacity (ice)
        real(p), intent(out) :: rhcapi_out(ix, il)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        rhcapi_out=state_ptr%p%rhcapi
    end subroutine

    subroutine set_rhcapi(state_cnt, rhcapi_in )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> 1./heat_capacity (ice)
        real(p), intent(in) :: rhcapi_in(ix, il)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        state_ptr%p%rhcapi=rhcapi_in
    end subroutine
    
    subroutine get_rhcapi_shape(state_cnt, array_shape)
        use params
        integer(8), intent(in) :: state_cnt
        !> 1./heat_capacity (ice)
        integer, intent(out) :: array_shape(2)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        if (state_ptr%p%rhcapi_initialized) then
            array_shape=shape(state_ptr%p%rhcapi)
        else
            array_shape = 0             
        endif
    end subroutine

    subroutine get_cdsea(state_cnt, cdsea_out )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> 1./dissip_time (sea)
        real(p), intent(out) :: cdsea_out(ix, il)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        cdsea_out=state_ptr%p%cdsea
    end subroutine

    subroutine set_cdsea(state_cnt, cdsea_in )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> 1./dissip_time (sea)
        real(p), intent(in) :: cdsea_in(ix, il)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        state_ptr%p%cdsea=cdsea_in
    end subroutine
    
    subroutine get_cdsea_shape(state_cnt, array_shape)
        use params
        integer(8), intent(in) :: state_cnt
        !> 1./dissip_time (sea)
        integer, intent(out) :: array_shape(2)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        if (state_ptr%p%cdsea_initialized) then
            array_shape=shape(state_ptr%p%cdsea)
        else
            array_shape = 0             
        endif
    end subroutine

    subroutine get_cdice(state_cnt, cdice_out )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> 1./dissip_time (ice)
        real(p), intent(out) :: cdice_out(ix, il)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        cdice_out=state_ptr%p%cdice
    end subroutine

    subroutine set_cdice(state_cnt, cdice_in )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> 1./dissip_time (ice)
        real(p), intent(in) :: cdice_in(ix, il)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        state_ptr%p%cdice=cdice_in
    end subroutine
    
    subroutine get_cdice_shape(state_cnt, array_shape)
        use params
        integer(8), intent(in) :: state_cnt
        !> 1./dissip_time (ice)
        integer, intent(out) :: array_shape(2)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        if (state_ptr%p%cdice_initialized) then
            array_shape=shape(state_ptr%p%cdice)
        else
            array_shape = 0             
        endif
    end subroutine

    subroutine get_fmask_sea(state_cnt, fmask_sea_out )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> Fraction of sea
        real(p), intent(out) :: fmask_sea_out(ix, il)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        fmask_sea_out=state_ptr%p%fmask_sea
    end subroutine

    subroutine set_fmask_sea(state_cnt, fmask_sea_in )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> Fraction of sea
        real(p), intent(in) :: fmask_sea_in(ix, il)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        state_ptr%p%fmask_sea=fmask_sea_in
    end subroutine
    
    subroutine get_fmask_sea_shape(state_cnt, array_shape)
        use params
        integer(8), intent(in) :: state_cnt
        !> Fraction of sea
        integer, intent(out) :: array_shape(2)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        if (state_ptr%p%fmask_sea_initialized) then
            array_shape=shape(state_ptr%p%fmask_sea)
        else
            array_shape = 0             
        endif
    end subroutine

    subroutine get_bmask_sea(state_cnt, bmask_sea_out )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> Binary sea mask
        real(p), intent(out) :: bmask_sea_out(ix, il)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        bmask_sea_out=state_ptr%p%bmask_sea
    end subroutine

    subroutine set_bmask_sea(state_cnt, bmask_sea_in )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> Binary sea mask
        real(p), intent(in) :: bmask_sea_in(ix, il)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        state_ptr%p%bmask_sea=bmask_sea_in
    end subroutine
    
    subroutine get_bmask_sea_shape(state_cnt, array_shape)
        use params
        integer(8), intent(in) :: state_cnt
        !> Binary sea mask
        integer, intent(out) :: array_shape(2)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        if (state_ptr%p%bmask_sea_initialized) then
            array_shape=shape(state_ptr%p%bmask_sea)
        else
            array_shape = 0             
        endif
    end subroutine

    subroutine get_deglat_s(state_cnt, deglat_s_out )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> Grid latitudes
        real(p), intent(out) :: deglat_s_out(il)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        deglat_s_out=state_ptr%p%deglat_s
    end subroutine

    subroutine set_deglat_s(state_cnt, deglat_s_in )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> Grid latitudes
        real(p), intent(in) :: deglat_s_in(il)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        state_ptr%p%deglat_s=deglat_s_in
    end subroutine
    
    subroutine get_deglat_s_shape(state_cnt, array_shape)
        use params
        integer(8), intent(in) :: state_cnt
        !> Grid latitudes
        integer, intent(out) :: array_shape(1)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        if (state_ptr%p%deglat_s_initialized) then
            array_shape=shape(state_ptr%p%deglat_s)
        else
            array_shape = 0             
        endif
    end subroutine

    subroutine get_hfseacl(state_cnt, hfseacl_out )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> Annual-mean heat flux into sea sfc.
        real(p), intent(out) :: hfseacl_out(ix, il)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        hfseacl_out=state_ptr%p%hfseacl
    end subroutine

    subroutine set_hfseacl(state_cnt, hfseacl_in )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> Annual-mean heat flux into sea sfc.
        real(p), intent(in) :: hfseacl_in(ix, il)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        state_ptr%p%hfseacl=hfseacl_in
    end subroutine
    
    subroutine get_hfseacl_shape(state_cnt, array_shape)
        use params
        integer(8), intent(in) :: state_cnt
        !> Annual-mean heat flux into sea sfc.
        integer, intent(out) :: array_shape(2)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        if (state_ptr%p%hfseacl_initialized) then
            array_shape=shape(state_ptr%p%hfseacl)
        else
            array_shape = 0             
        endif
    end subroutine

    subroutine get_sstom12(state_cnt, sstom12_out )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> Ocean model SST climatology
        real(p), intent(out) :: sstom12_out(ix, il, 12)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        sstom12_out=state_ptr%p%sstom12
    end subroutine

    subroutine set_sstom12(state_cnt, sstom12_in )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> Ocean model SST climatology
        real(p), intent(in) :: sstom12_in(ix, il, 12)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        state_ptr%p%sstom12=sstom12_in
    end subroutine
    
    subroutine get_sstom12_shape(state_cnt, array_shape)
        use params
        integer(8), intent(in) :: state_cnt
        !> Ocean model SST climatology
        integer, intent(out) :: array_shape(3)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        if (state_ptr%p%sstom12_initialized) then
            array_shape=shape(state_ptr%p%sstom12)
        else
            array_shape = 0             
        endif
    end subroutine

    subroutine get_sstcl_ob(state_cnt, sstcl_ob_out )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> Observed clim. SST
        real(p), intent(out) :: sstcl_ob_out(ix, il)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        sstcl_ob_out=state_ptr%p%sstcl_ob
    end subroutine

    subroutine set_sstcl_ob(state_cnt, sstcl_ob_in )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> Observed clim. SST
        real(p), intent(in) :: sstcl_ob_in(ix, il)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        state_ptr%p%sstcl_ob=sstcl_ob_in
    end subroutine
    
    subroutine get_sstcl_ob_shape(state_cnt, array_shape)
        use params
        integer(8), intent(in) :: state_cnt
        !> Observed clim. SST
        integer, intent(out) :: array_shape(2)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        if (state_ptr%p%sstcl_ob_initialized) then
            array_shape=shape(state_ptr%p%sstcl_ob)
        else
            array_shape = 0             
        endif
    end subroutine

    subroutine get_sicecl_ob(state_cnt, sicecl_ob_out )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> Clim. sea ice fraction
        real(p), intent(out) :: sicecl_ob_out(ix, il)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        sicecl_ob_out=state_ptr%p%sicecl_ob
    end subroutine

    subroutine set_sicecl_ob(state_cnt, sicecl_ob_in )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> Clim. sea ice fraction
        real(p), intent(in) :: sicecl_ob_in(ix, il)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        state_ptr%p%sicecl_ob=sicecl_ob_in
    end subroutine
    
    subroutine get_sicecl_ob_shape(state_cnt, array_shape)
        use params
        integer(8), intent(in) :: state_cnt
        !> Clim. sea ice fraction
        integer, intent(out) :: array_shape(2)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        if (state_ptr%p%sicecl_ob_initialized) then
            array_shape=shape(state_ptr%p%sicecl_ob)
        else
            array_shape = 0             
        endif
    end subroutine

    subroutine get_ticecl_ob(state_cnt, ticecl_ob_out )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> Clim. sea ice temperature
        real(p), intent(out) :: ticecl_ob_out(ix, il)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        ticecl_ob_out=state_ptr%p%ticecl_ob
    end subroutine

    subroutine set_ticecl_ob(state_cnt, ticecl_ob_in )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> Clim. sea ice temperature
        real(p), intent(in) :: ticecl_ob_in(ix, il)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        state_ptr%p%ticecl_ob=ticecl_ob_in
    end subroutine
    
    subroutine get_ticecl_ob_shape(state_cnt, array_shape)
        use params
        integer(8), intent(in) :: state_cnt
        !> Clim. sea ice temperature
        integer, intent(out) :: array_shape(2)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        if (state_ptr%p%ticecl_ob_initialized) then
            array_shape=shape(state_ptr%p%ticecl_ob)
        else
            array_shape = 0             
        endif
    end subroutine

    subroutine get_sstan_ob(state_cnt, sstan_ob_out )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> Daily observed SST anomaly
        real(p), intent(out) :: sstan_ob_out(ix, il)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        sstan_ob_out=state_ptr%p%sstan_ob
    end subroutine

    subroutine set_sstan_ob(state_cnt, sstan_ob_in )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> Daily observed SST anomaly
        real(p), intent(in) :: sstan_ob_in(ix, il)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        state_ptr%p%sstan_ob=sstan_ob_in
    end subroutine
    
    subroutine get_sstan_ob_shape(state_cnt, array_shape)
        use params
        integer(8), intent(in) :: state_cnt
        !> Daily observed SST anomaly
        integer, intent(out) :: array_shape(2)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        if (state_ptr%p%sstan_ob_initialized) then
            array_shape=shape(state_ptr%p%sstan_ob)
        else
            array_shape = 0             
        endif
    end subroutine

    subroutine get_sstcl_om(state_cnt, sstcl_om_out )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> Ocean model clim. SST
        real(p), intent(out) :: sstcl_om_out(ix, il)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        sstcl_om_out=state_ptr%p%sstcl_om
    end subroutine

    subroutine set_sstcl_om(state_cnt, sstcl_om_in )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> Ocean model clim. SST
        real(p), intent(in) :: sstcl_om_in(ix, il)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        state_ptr%p%sstcl_om=sstcl_om_in
    end subroutine
    
    subroutine get_sstcl_om_shape(state_cnt, array_shape)
        use params
        integer(8), intent(in) :: state_cnt
        !> Ocean model clim. SST
        integer, intent(out) :: array_shape(2)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        if (state_ptr%p%sstcl_om_initialized) then
            array_shape=shape(state_ptr%p%sstcl_om)
        else
            array_shape = 0             
        endif
    end subroutine

    subroutine get_sst_am(state_cnt, sst_am_out )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> SST (full-field)
        real(p), intent(out) :: sst_am_out(ix, il)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        sst_am_out=state_ptr%p%sst_am
    end subroutine

    subroutine set_sst_am(state_cnt, sst_am_in )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> SST (full-field)
        real(p), intent(in) :: sst_am_in(ix, il)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        state_ptr%p%sst_am=sst_am_in
    end subroutine
    
    subroutine get_sst_am_shape(state_cnt, array_shape)
        use params
        integer(8), intent(in) :: state_cnt
        !> SST (full-field)
        integer, intent(out) :: array_shape(2)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        if (state_ptr%p%sst_am_initialized) then
            array_shape=shape(state_ptr%p%sst_am)
        else
            array_shape = 0             
        endif
    end subroutine

    subroutine get_sstan_am(state_cnt, sstan_am_out )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> SST anomaly
        real(p), intent(out) :: sstan_am_out(ix, il)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        sstan_am_out=state_ptr%p%sstan_am
    end subroutine

    subroutine set_sstan_am(state_cnt, sstan_am_in )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> SST anomaly
        real(p), intent(in) :: sstan_am_in(ix, il)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        state_ptr%p%sstan_am=sstan_am_in
    end subroutine
    
    subroutine get_sstan_am_shape(state_cnt, array_shape)
        use params
        integer(8), intent(in) :: state_cnt
        !> SST anomaly
        integer, intent(out) :: array_shape(2)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        if (state_ptr%p%sstan_am_initialized) then
            array_shape=shape(state_ptr%p%sstan_am)
        else
            array_shape = 0             
        endif
    end subroutine

    subroutine get_sice_am(state_cnt, sice_am_out )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> Sea ice fraction
        real(p), intent(out) :: sice_am_out(ix, il)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        sice_am_out=state_ptr%p%sice_am
    end subroutine

    subroutine set_sice_am(state_cnt, sice_am_in )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> Sea ice fraction
        real(p), intent(in) :: sice_am_in(ix, il)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        state_ptr%p%sice_am=sice_am_in
    end subroutine
    
    subroutine get_sice_am_shape(state_cnt, array_shape)
        use params
        integer(8), intent(in) :: state_cnt
        !> Sea ice fraction
        integer, intent(out) :: array_shape(2)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        if (state_ptr%p%sice_am_initialized) then
            array_shape=shape(state_ptr%p%sice_am)
        else
            array_shape = 0             
        endif
    end subroutine

    subroutine get_tice_am(state_cnt, tice_am_out )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> Sea ice temperature
        real(p), intent(out) :: tice_am_out(ix, il)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        tice_am_out=state_ptr%p%tice_am
    end subroutine

    subroutine set_tice_am(state_cnt, tice_am_in )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> Sea ice temperature
        real(p), intent(in) :: tice_am_in(ix, il)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        state_ptr%p%tice_am=tice_am_in
    end subroutine
    
    subroutine get_tice_am_shape(state_cnt, array_shape)
        use params
        integer(8), intent(in) :: state_cnt
        !> Sea ice temperature
        integer, intent(out) :: array_shape(2)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        if (state_ptr%p%tice_am_initialized) then
            array_shape=shape(state_ptr%p%tice_am)
        else
            array_shape = 0             
        endif
    end subroutine

    subroutine get_sst_om(state_cnt, sst_om_out )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> Ocean model SST
        real(p), intent(out) :: sst_om_out(ix, il)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        sst_om_out=state_ptr%p%sst_om
    end subroutine

    subroutine set_sst_om(state_cnt, sst_om_in )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> Ocean model SST
        real(p), intent(in) :: sst_om_in(ix, il)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        state_ptr%p%sst_om=sst_om_in
    end subroutine
    
    subroutine get_sst_om_shape(state_cnt, array_shape)
        use params
        integer(8), intent(in) :: state_cnt
        !> Ocean model SST
        integer, intent(out) :: array_shape(2)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        if (state_ptr%p%sst_om_initialized) then
            array_shape=shape(state_ptr%p%sst_om)
        else
            array_shape = 0             
        endif
    end subroutine

    subroutine get_sice_om(state_cnt, sice_om_out )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> Model sea ice fraction
        real(p), intent(out) :: sice_om_out(ix, il)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        sice_om_out=state_ptr%p%sice_om
    end subroutine

    subroutine set_sice_om(state_cnt, sice_om_in )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> Model sea ice fraction
        real(p), intent(in) :: sice_om_in(ix, il)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        state_ptr%p%sice_om=sice_om_in
    end subroutine
    
    subroutine get_sice_om_shape(state_cnt, array_shape)
        use params
        integer(8), intent(in) :: state_cnt
        !> Model sea ice fraction
        integer, intent(out) :: array_shape(2)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        if (state_ptr%p%sice_om_initialized) then
            array_shape=shape(state_ptr%p%sice_om)
        else
            array_shape = 0             
        endif
    end subroutine

    subroutine get_tice_om(state_cnt, tice_om_out )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> Model sea ice temperature
        real(p), intent(out) :: tice_om_out(ix, il)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        tice_om_out=state_ptr%p%tice_om
    end subroutine

    subroutine set_tice_om(state_cnt, tice_om_in )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> Model sea ice temperature
        real(p), intent(in) :: tice_om_in(ix, il)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        state_ptr%p%tice_om=tice_om_in
    end subroutine
    
    subroutine get_tice_om_shape(state_cnt, array_shape)
        use params
        integer(8), intent(in) :: state_cnt
        !> Model sea ice temperature
        integer, intent(out) :: array_shape(2)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        if (state_ptr%p%tice_om_initialized) then
            array_shape=shape(state_ptr%p%tice_om)
        else
            array_shape = 0             
        endif
    end subroutine

    subroutine get_ssti_om(state_cnt, ssti_om_out )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> Model SST + sea ice temp.
        real(p), intent(out) :: ssti_om_out(ix, il)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        ssti_om_out=state_ptr%p%ssti_om
    end subroutine

    subroutine set_ssti_om(state_cnt, ssti_om_in )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> Model SST + sea ice temp.
        real(p), intent(in) :: ssti_om_in(ix, il)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        state_ptr%p%ssti_om=ssti_om_in
    end subroutine
    
    subroutine get_ssti_om_shape(state_cnt, array_shape)
        use params
        integer(8), intent(in) :: state_cnt
        !> Model SST + sea ice temp.
        integer, intent(out) :: array_shape(2)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        if (state_ptr%p%ssti_om_initialized) then
            array_shape=shape(state_ptr%p%ssti_om)
        else
            array_shape = 0             
        endif
    end subroutine

    subroutine get_wsst_ob(state_cnt, wsst_ob_out )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> Weight for obs. SST anomaly in coupled runs
        real(p), intent(out) :: wsst_ob_out(ix, il)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        wsst_ob_out=state_ptr%p%wsst_ob
    end subroutine

    subroutine set_wsst_ob(state_cnt, wsst_ob_in )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> Weight for obs. SST anomaly in coupled runs
        real(p), intent(in) :: wsst_ob_in(ix, il)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        state_ptr%p%wsst_ob=wsst_ob_in
    end subroutine
    
    subroutine get_wsst_ob_shape(state_cnt, array_shape)
        use params
        integer(8), intent(in) :: state_cnt
        !> Weight for obs. SST anomaly in coupled runs
        integer, intent(out) :: array_shape(2)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        if (state_ptr%p%wsst_ob_initialized) then
            array_shape=shape(state_ptr%p%wsst_ob)
        else
            array_shape = 0             
        endif
    end subroutine

    subroutine get_fband(state_cnt, fband_out )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> Energy fraction emitted in each LW band = f(T)
        real(8), intent(out) :: fband_out(100:400,4)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        fband_out=state_ptr%p%fband
    end subroutine

    subroutine set_fband(state_cnt, fband_in )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> Energy fraction emitted in each LW band = f(T)
        real(8), intent(in) :: fband_in(100:400,4)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        state_ptr%p%fband=fband_in
    end subroutine
    
    subroutine get_fband_shape(state_cnt, array_shape)
        use params
        integer(8), intent(in) :: state_cnt
        !> Energy fraction emitted in each LW band = f(T)
        integer, intent(out) :: array_shape(2)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        if (state_ptr%p%fband_initialized) then
            array_shape=shape(state_ptr%p%fband)
        else
            array_shape = 0             
        endif
    end subroutine

    subroutine get_alb_land(state_cnt, alb_land_out )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> Daily-mean albedo over land (bare-land + snow)
        real(8), intent(out) :: alb_land_out(ix,il)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        alb_land_out=state_ptr%p%alb_land
    end subroutine

    subroutine set_alb_land(state_cnt, alb_land_in )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> Daily-mean albedo over land (bare-land + snow)
        real(8), intent(in) :: alb_land_in(ix,il)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        state_ptr%p%alb_land=alb_land_in
    end subroutine
    
    subroutine get_alb_land_shape(state_cnt, array_shape)
        use params
        integer(8), intent(in) :: state_cnt
        !> Daily-mean albedo over land (bare-land + snow)
        integer, intent(out) :: array_shape(2)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        if (state_ptr%p%alb_land_initialized) then
            array_shape=shape(state_ptr%p%alb_land)
        else
            array_shape = 0             
        endif
    end subroutine

    subroutine get_alb_sea(state_cnt, alb_sea_out )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> Daily-mean albedo over sea  (open sea + sea ice)
        real(8), intent(out) :: alb_sea_out(ix,il)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        alb_sea_out=state_ptr%p%alb_sea
    end subroutine

    subroutine set_alb_sea(state_cnt, alb_sea_in )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> Daily-mean albedo over sea  (open sea + sea ice)
        real(8), intent(in) :: alb_sea_in(ix,il)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        state_ptr%p%alb_sea=alb_sea_in
    end subroutine
    
    subroutine get_alb_sea_shape(state_cnt, array_shape)
        use params
        integer(8), intent(in) :: state_cnt
        !> Daily-mean albedo over sea  (open sea + sea ice)
        integer, intent(out) :: array_shape(2)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        if (state_ptr%p%alb_sea_initialized) then
            array_shape=shape(state_ptr%p%alb_sea)
        else
            array_shape = 0             
        endif
    end subroutine

    subroutine get_alb_surface(state_cnt, alb_surface_out )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> Combined surface albedo (land + sea)
        real(8), intent(out) :: alb_surface_out(ix,il)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        alb_surface_out=state_ptr%p%alb_surface
    end subroutine

    subroutine set_alb_surface(state_cnt, alb_surface_in )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> Combined surface albedo (land + sea)
        real(8), intent(in) :: alb_surface_in(ix,il)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        state_ptr%p%alb_surface=alb_surface_in
    end subroutine
    
    subroutine get_alb_surface_shape(state_cnt, array_shape)
        use params
        integer(8), intent(in) :: state_cnt
        !> Combined surface albedo (land + sea)
        integer, intent(out) :: array_shape(2)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        if (state_ptr%p%alb_surface_initialized) then
            array_shape=shape(state_ptr%p%alb_surface)
        else
            array_shape = 0             
        endif
    end subroutine

    subroutine get_snowc(state_cnt, snowc_out )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> Effective snow cover (fraction)
        real(8), intent(out) :: snowc_out(ix,il)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        snowc_out=state_ptr%p%snowc
    end subroutine

    subroutine set_snowc(state_cnt, snowc_in )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> Effective snow cover (fraction)
        real(8), intent(in) :: snowc_in(ix,il)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        state_ptr%p%snowc=snowc_in
    end subroutine
    
    subroutine get_snowc_shape(state_cnt, array_shape)
        use params
        integer(8), intent(in) :: state_cnt
        !> Effective snow cover (fraction)
        integer, intent(out) :: array_shape(2)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        if (state_ptr%p%snowc_initialized) then
            array_shape=shape(state_ptr%p%snowc)
        else
            array_shape = 0             
        endif
    end subroutine

    subroutine get_rad_flux(state_cnt, rad_flux_out )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> Radiative flux in different spectral bands
        real(8), intent(out) :: rad_flux_out(ix,il,4)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        rad_flux_out=state_ptr%p%rad_flux
    end subroutine

    subroutine set_rad_flux(state_cnt, rad_flux_in )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> Radiative flux in different spectral bands
        real(8), intent(in) :: rad_flux_in(ix,il,4)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        state_ptr%p%rad_flux=rad_flux_in
    end subroutine
    
    subroutine get_rad_flux_shape(state_cnt, array_shape)
        use params
        integer(8), intent(in) :: state_cnt
        !> Radiative flux in different spectral bands
        integer, intent(out) :: array_shape(3)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        if (state_ptr%p%rad_flux_initialized) then
            array_shape=shape(state_ptr%p%rad_flux)
        else
            array_shape = 0             
        endif
    end subroutine

    subroutine get_rad_tau2(state_cnt, rad_tau2_out )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> Transmissivity of atmospheric layers
        real(8), intent(out) :: rad_tau2_out(ix,il,kx,4)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        rad_tau2_out=state_ptr%p%rad_tau2
    end subroutine

    subroutine set_rad_tau2(state_cnt, rad_tau2_in )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> Transmissivity of atmospheric layers
        real(8), intent(in) :: rad_tau2_in(ix,il,kx,4)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        state_ptr%p%rad_tau2=rad_tau2_in
    end subroutine
    
    subroutine get_rad_tau2_shape(state_cnt, array_shape)
        use params
        integer(8), intent(in) :: state_cnt
        !> Transmissivity of atmospheric layers
        integer, intent(out) :: array_shape(4)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        if (state_ptr%p%rad_tau2_initialized) then
            array_shape=shape(state_ptr%p%rad_tau2)
        else
            array_shape = 0             
        endif
    end subroutine

    subroutine get_rad_st4a(state_cnt, rad_st4a_out )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> Blackbody emission from full and half atmospheric levels
        real(8), intent(out) :: rad_st4a_out(ix,il,kx,2)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        rad_st4a_out=state_ptr%p%rad_st4a
    end subroutine

    subroutine set_rad_st4a(state_cnt, rad_st4a_in )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> Blackbody emission from full and half atmospheric levels
        real(8), intent(in) :: rad_st4a_in(ix,il,kx,2)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        state_ptr%p%rad_st4a=rad_st4a_in
    end subroutine
    
    subroutine get_rad_st4a_shape(state_cnt, array_shape)
        use params
        integer(8), intent(in) :: state_cnt
        !> Blackbody emission from full and half atmospheric levels
        integer, intent(out) :: array_shape(4)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        if (state_ptr%p%rad_st4a_initialized) then
            array_shape=shape(state_ptr%p%rad_st4a)
        else
            array_shape = 0             
        endif
    end subroutine

    subroutine get_rad_strat_corr(state_cnt, rad_strat_corr_out )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> Stratospheric correction term
        real(8), intent(out) :: rad_strat_corr_out(ix,il,2)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        rad_strat_corr_out=state_ptr%p%rad_strat_corr
    end subroutine

    subroutine set_rad_strat_corr(state_cnt, rad_strat_corr_in )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> Stratospheric correction term
        real(8), intent(in) :: rad_strat_corr_in(ix,il,2)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        state_ptr%p%rad_strat_corr=rad_strat_corr_in
    end subroutine
    
    subroutine get_rad_strat_corr_shape(state_cnt, array_shape)
        use params
        integer(8), intent(in) :: state_cnt
        !> Stratospheric correction term
        integer, intent(out) :: array_shape(3)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        if (state_ptr%p%rad_strat_corr_initialized) then
            array_shape=shape(state_ptr%p%rad_strat_corr)
        else
            array_shape = 0             
        endif
    end subroutine

    subroutine get_lon(state_cnt, lon_out )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> longitude
        real, intent(out) :: lon_out(ix)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        lon_out=state_ptr%p%lon
    end subroutine

    subroutine set_lon(state_cnt, lon_in )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> longitude
        real, intent(in) :: lon_in(ix)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        state_ptr%p%lon=lon_in
    end subroutine
    
    subroutine get_lon_shape(state_cnt, array_shape)
        use params
        integer(8), intent(in) :: state_cnt
        !> longitude
        integer, intent(out) :: array_shape(1)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        if (state_ptr%p%lon_initialized) then
            array_shape=shape(state_ptr%p%lon)
        else
            array_shape = 0             
        endif
    end subroutine

    subroutine get_lat(state_cnt, lat_out )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> latitude
        real, intent(out) :: lat_out(il)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        lat_out=state_ptr%p%lat
    end subroutine

    subroutine set_lat(state_cnt, lat_in )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> latitude
        real, intent(in) :: lat_in(il)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        state_ptr%p%lat=lat_in
    end subroutine
    
    subroutine get_lat_shape(state_cnt, array_shape)
        use params
        integer(8), intent(in) :: state_cnt
        !> latitude
        integer, intent(out) :: array_shape(1)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        if (state_ptr%p%lat_initialized) then
            array_shape=shape(state_ptr%p%lat)
        else
            array_shape = 0             
        endif
    end subroutine

    subroutine get_lev(state_cnt, lev_out )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> Vertical sigma coordinate
        real, intent(out) :: lev_out(kx)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        lev_out=state_ptr%p%lev
    end subroutine

    subroutine set_lev(state_cnt, lev_in )
        use params
        integer(8), intent(in) :: state_cnt
                
        !> Vertical sigma coordinate
        real, intent(in) :: lev_in(kx)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        state_ptr%p%lev=lev_in
    end subroutine
    
    subroutine get_lev_shape(state_cnt, array_shape)
        use params
        integer(8), intent(in) :: state_cnt
        !> Vertical sigma coordinate
        integer, intent(out) :: array_shape(1)
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        if (state_ptr%p%lev_initialized) then
            array_shape=shape(state_ptr%p%lev)
        else
            array_shape = 0             
        endif
    end subroutine

    !==============
    ! State scalars
    !==============

    subroutine get_current_step(state_cnt, current_step_out)
        use params
        integer(8), intent(in) :: state_cnt
        !> Current model step.
        integer, intent(out) :: current_step_out
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        current_step_out=state_ptr%p%current_step
    end subroutine

    subroutine set_current_step(state_cnt, current_step_in)
        use params
        integer(8), intent(in) :: state_cnt       
        !> Current model step.
        integer, intent(in) :: current_step_in
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        state_ptr%p%current_step=current_step_in
    end subroutine

    subroutine get_increase_co2(state_cnt, increase_co2_out)
        use params
        integer(8), intent(in) :: state_cnt
        !>  Flag for CO2 optical thickness increase
        logical, intent(out) :: increase_co2_out
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        increase_co2_out=state_ptr%p%increase_co2
    end subroutine

    subroutine set_increase_co2(state_cnt, increase_co2_in)
        use params
        integer(8), intent(in) :: state_cnt       
        !>  Flag for CO2 optical thickness increase
        logical, intent(in) :: increase_co2_in
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        state_ptr%p%increase_co2=increase_co2_in
    end subroutine

    subroutine get_compute_shortwave(state_cnt, compute_shortwave_out)
        use params
        integer(8), intent(in) :: state_cnt
        !> Flag for shortwave radiation routine (turned on and off in main loop depending on the value of nstrad)
        logical, intent(out) :: compute_shortwave_out
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        compute_shortwave_out=state_ptr%p%compute_shortwave
    end subroutine

    subroutine set_compute_shortwave(state_cnt, compute_shortwave_in)
        use params
        integer(8), intent(in) :: state_cnt       
        !> Flag for shortwave radiation routine (turned on and off in main loop depending on the value of nstrad)
        logical, intent(in) :: compute_shortwave_in
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        state_ptr%p%compute_shortwave=compute_shortwave_in
    end subroutine

    subroutine get_air_absortivity_co2(state_cnt, air_absortivity_co2_out)
        use params
        integer(8), intent(in) :: state_cnt
        !> Absorptivity of air in CO2 band
        real(8), intent(out) :: air_absortivity_co2_out
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        air_absortivity_co2_out=state_ptr%p%air_absortivity_co2
    end subroutine

    subroutine set_air_absortivity_co2(state_cnt, air_absortivity_co2_in)
        use params
        integer(8), intent(in) :: state_cnt       
        !> Absorptivity of air in CO2 band
        real(8), intent(in) :: air_absortivity_co2_in
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        state_ptr%p%air_absortivity_co2=air_absortivity_co2_in
    end subroutine

    subroutine get_land_coupling_flag(state_cnt, land_coupling_flag_out)
        use params
        integer(8), intent(in) :: state_cnt
        !> Flag for land-coupling (0: off, 1: on)
        logical, intent(out) :: land_coupling_flag_out
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        land_coupling_flag_out=state_ptr%p%land_coupling_flag
    end subroutine

    subroutine set_land_coupling_flag(state_cnt, land_coupling_flag_in)
        use params
        integer(8), intent(in) :: state_cnt       
        !> Flag for land-coupling (0: off, 1: on)
        logical, intent(in) :: land_coupling_flag_in
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        state_ptr%p%land_coupling_flag=land_coupling_flag_in
    end subroutine

    subroutine get_sst_anomaly_coupling_flag(state_cnt, sst_anomaly_coupling_flag_out)
        use params
        integer(8), intent(in) :: state_cnt
        !> Weight for obs. SST anomaly in coupled runs
        logical, intent(out) :: sst_anomaly_coupling_flag_out
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        sst_anomaly_coupling_flag_out=state_ptr%p%sst_anomaly_coupling_flag
    end subroutine

    subroutine set_sst_anomaly_coupling_flag(state_cnt, sst_anomaly_coupling_flag_in)
        use params
        integer(8), intent(in) :: state_cnt       
        !> Weight for obs. SST anomaly in coupled runs
        logical, intent(in) :: sst_anomaly_coupling_flag_in
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        state_ptr%p%sst_anomaly_coupling_flag=sst_anomaly_coupling_flag_in
    end subroutine

    subroutine get_ablco2_ref(state_cnt, ablco2_ref_out)
        use params
        integer(8), intent(in) :: state_cnt
        !> Initial absorptivity of air in CO2 band (t=t0)
        real(8), intent(out) :: ablco2_ref_out
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        ablco2_ref_out=state_ptr%p%ablco2_ref
    end subroutine

    subroutine set_ablco2_ref(state_cnt, ablco2_ref_in)
        use params
        integer(8), intent(in) :: state_cnt       
        !> Initial absorptivity of air in CO2 band (t=t0)
        real(8), intent(in) :: ablco2_ref_in
        type(ModelState_Ptr_t):: state_ptr

        state_ptr = transfer(state_cnt, state_ptr)
        state_ptr%p%ablco2_ref=ablco2_ref_in
    end subroutine

    subroutine is_array_vor(var_is_array)
        logical, intent(out) :: var_is_array
        var_is_array=.true.
    end subroutine

    subroutine is_array_div(var_is_array)
        logical, intent(out) :: var_is_array
        var_is_array=.true.
    end subroutine

    subroutine is_array_t(var_is_array)
        logical, intent(out) :: var_is_array
        var_is_array=.true.
    end subroutine

    subroutine is_array_ps(var_is_array)
        logical, intent(out) :: var_is_array
        var_is_array=.true.
    end subroutine

    subroutine is_array_tr(var_is_array)
        logical, intent(out) :: var_is_array
        var_is_array=.true.
    end subroutine

    subroutine is_array_phi(var_is_array)
        logical, intent(out) :: var_is_array
        var_is_array=.true.
    end subroutine

    subroutine is_array_phis(var_is_array)
        logical, intent(out) :: var_is_array
        var_is_array=.true.
    end subroutine

    subroutine is_array_u_grid(var_is_array)
        logical, intent(out) :: var_is_array
        var_is_array=.true.
    end subroutine

    subroutine is_array_v_grid(var_is_array)
        logical, intent(out) :: var_is_array
        var_is_array=.true.
    end subroutine

    subroutine is_array_t_grid(var_is_array)
        logical, intent(out) :: var_is_array
        var_is_array=.true.
    end subroutine

    subroutine is_array_q_grid(var_is_array)
        logical, intent(out) :: var_is_array
        var_is_array=.true.
    end subroutine

    subroutine is_array_phi_grid(var_is_array)
        logical, intent(out) :: var_is_array
        var_is_array=.true.
    end subroutine

    subroutine is_array_ps_grid(var_is_array)
        logical, intent(out) :: var_is_array
        var_is_array=.true.
    end subroutine

    subroutine is_array_precnv(var_is_array)
        logical, intent(out) :: var_is_array
        var_is_array=.true.
    end subroutine

    subroutine is_array_precls(var_is_array)
        logical, intent(out) :: var_is_array
        var_is_array=.true.
    end subroutine

    subroutine is_array_snowcv(var_is_array)
        logical, intent(out) :: var_is_array
        var_is_array=.true.
    end subroutine

    subroutine is_array_snowls(var_is_array)
        logical, intent(out) :: var_is_array
        var_is_array=.true.
    end subroutine

    subroutine is_array_cbmf(var_is_array)
        logical, intent(out) :: var_is_array
        var_is_array=.true.
    end subroutine

    subroutine is_array_tsr(var_is_array)
        logical, intent(out) :: var_is_array
        var_is_array=.true.
    end subroutine

    subroutine is_array_ssrd(var_is_array)
        logical, intent(out) :: var_is_array
        var_is_array=.true.
    end subroutine

    subroutine is_array_ssr(var_is_array)
        logical, intent(out) :: var_is_array
        var_is_array=.true.
    end subroutine

    subroutine is_array_slrd(var_is_array)
        logical, intent(out) :: var_is_array
        var_is_array=.true.
    end subroutine

    subroutine is_array_slr(var_is_array)
        logical, intent(out) :: var_is_array
        var_is_array=.true.
    end subroutine

    subroutine is_array_olr(var_is_array)
        logical, intent(out) :: var_is_array
        var_is_array=.true.
    end subroutine

    subroutine is_array_slru(var_is_array)
        logical, intent(out) :: var_is_array
        var_is_array=.true.
    end subroutine

    subroutine is_array_ustr(var_is_array)
        logical, intent(out) :: var_is_array
        var_is_array=.true.
    end subroutine

    subroutine is_array_vstr(var_is_array)
        logical, intent(out) :: var_is_array
        var_is_array=.true.
    end subroutine

    subroutine is_array_shf(var_is_array)
        logical, intent(out) :: var_is_array
        var_is_array=.true.
    end subroutine

    subroutine is_array_evap(var_is_array)
        logical, intent(out) :: var_is_array
        var_is_array=.true.
    end subroutine

    subroutine is_array_hfluxn(var_is_array)
        logical, intent(out) :: var_is_array
        var_is_array=.true.
    end subroutine

    subroutine is_array_tt_rsw(var_is_array)
        logical, intent(out) :: var_is_array
        var_is_array=.true.
    end subroutine

    subroutine is_array_phi0(var_is_array)
        logical, intent(out) :: var_is_array
        var_is_array=.true.
    end subroutine

    subroutine is_array_orog(var_is_array)
        logical, intent(out) :: var_is_array
        var_is_array=.true.
    end subroutine

    subroutine is_array_phis0(var_is_array)
        logical, intent(out) :: var_is_array
        var_is_array=.true.
    end subroutine

    subroutine is_array_alb0(var_is_array)
        logical, intent(out) :: var_is_array
        var_is_array=.true.
    end subroutine

    subroutine is_array_forog(var_is_array)
        logical, intent(out) :: var_is_array
        var_is_array=.true.
    end subroutine

    subroutine is_array_fmask_orig(var_is_array)
        logical, intent(out) :: var_is_array
        var_is_array=.true.
    end subroutine

    subroutine is_array_xgeop1(var_is_array)
        logical, intent(out) :: var_is_array
        var_is_array=.true.
    end subroutine

    subroutine is_array_xgeop2(var_is_array)
        logical, intent(out) :: var_is_array
        var_is_array=.true.
    end subroutine

    subroutine is_array_stl12(var_is_array)
        logical, intent(out) :: var_is_array
        var_is_array=.true.
    end subroutine

    subroutine is_array_snowd12(var_is_array)
        logical, intent(out) :: var_is_array
        var_is_array=.true.
    end subroutine

    subroutine is_array_soilw12(var_is_array)
        logical, intent(out) :: var_is_array
        var_is_array=.true.
    end subroutine

    subroutine is_array_veg_low(var_is_array)
        logical, intent(out) :: var_is_array
        var_is_array=.true.
    end subroutine

    subroutine is_array_veg_high(var_is_array)
        logical, intent(out) :: var_is_array
        var_is_array=.true.
    end subroutine

    subroutine is_array_soil_wc_l1(var_is_array)
        logical, intent(out) :: var_is_array
        var_is_array=.true.
    end subroutine

    subroutine is_array_soil_wc_l2(var_is_array)
        logical, intent(out) :: var_is_array
        var_is_array=.true.
    end subroutine

    subroutine is_array_soil_wc_l3(var_is_array)
        logical, intent(out) :: var_is_array
        var_is_array=.true.
    end subroutine

    subroutine is_array_sst12(var_is_array)
        logical, intent(out) :: var_is_array
        var_is_array=.true.
    end subroutine

    subroutine is_array_sea_ice_frac12(var_is_array)
        logical, intent(out) :: var_is_array
        var_is_array=.true.
    end subroutine

    subroutine is_array_sst_anom(var_is_array)
        logical, intent(out) :: var_is_array
        var_is_array=.true.
    end subroutine

    subroutine is_array_flux_solar_in(var_is_array)
        logical, intent(out) :: var_is_array
        var_is_array=.true.
    end subroutine

    subroutine is_array_flux_ozone_lower(var_is_array)
        logical, intent(out) :: var_is_array
        var_is_array=.true.
    end subroutine

    subroutine is_array_flux_ozone_upper(var_is_array)
        logical, intent(out) :: var_is_array
        var_is_array=.true.
    end subroutine

    subroutine is_array_zenit_correction(var_is_array)
        logical, intent(out) :: var_is_array
        var_is_array=.true.
    end subroutine

    subroutine is_array_stratospheric_correction(var_is_array)
        logical, intent(out) :: var_is_array
        var_is_array=.true.
    end subroutine

    subroutine is_array_qcloud_equiv(var_is_array)
        logical, intent(out) :: var_is_array
        var_is_array=.true.
    end subroutine

    subroutine is_array_rhcapl(var_is_array)
        logical, intent(out) :: var_is_array
        var_is_array=.true.
    end subroutine

    subroutine is_array_cdland(var_is_array)
        logical, intent(out) :: var_is_array
        var_is_array=.true.
    end subroutine

    subroutine is_array_stlcl_obs(var_is_array)
        logical, intent(out) :: var_is_array
        var_is_array=.true.
    end subroutine

    subroutine is_array_snowdcl_obs(var_is_array)
        logical, intent(out) :: var_is_array
        var_is_array=.true.
    end subroutine

    subroutine is_array_soilwcl_obs(var_is_array)
        logical, intent(out) :: var_is_array
        var_is_array=.true.
    end subroutine

    subroutine is_array_land_temp(var_is_array)
        logical, intent(out) :: var_is_array
        var_is_array=.true.
    end subroutine

    subroutine is_array_snow_depth(var_is_array)
        logical, intent(out) :: var_is_array
        var_is_array=.true.
    end subroutine

    subroutine is_array_soil_avail_water(var_is_array)
        logical, intent(out) :: var_is_array
        var_is_array=.true.
    end subroutine

    subroutine is_array_stl_lm(var_is_array)
        logical, intent(out) :: var_is_array
        var_is_array=.true.
    end subroutine

    subroutine is_array_fmask_land(var_is_array)
        logical, intent(out) :: var_is_array
        var_is_array=.true.
    end subroutine

    subroutine is_array_bmask_land(var_is_array)
        logical, intent(out) :: var_is_array
        var_is_array=.true.
    end subroutine

    subroutine is_array_rhcaps(var_is_array)
        logical, intent(out) :: var_is_array
        var_is_array=.true.
    end subroutine

    subroutine is_array_rhcapi(var_is_array)
        logical, intent(out) :: var_is_array
        var_is_array=.true.
    end subroutine

    subroutine is_array_cdsea(var_is_array)
        logical, intent(out) :: var_is_array
        var_is_array=.true.
    end subroutine

    subroutine is_array_cdice(var_is_array)
        logical, intent(out) :: var_is_array
        var_is_array=.true.
    end subroutine

    subroutine is_array_fmask_sea(var_is_array)
        logical, intent(out) :: var_is_array
        var_is_array=.true.
    end subroutine

    subroutine is_array_bmask_sea(var_is_array)
        logical, intent(out) :: var_is_array
        var_is_array=.true.
    end subroutine

    subroutine is_array_deglat_s(var_is_array)
        logical, intent(out) :: var_is_array
        var_is_array=.true.
    end subroutine

    subroutine is_array_hfseacl(var_is_array)
        logical, intent(out) :: var_is_array
        var_is_array=.true.
    end subroutine

    subroutine is_array_sstom12(var_is_array)
        logical, intent(out) :: var_is_array
        var_is_array=.true.
    end subroutine

    subroutine is_array_sstcl_ob(var_is_array)
        logical, intent(out) :: var_is_array
        var_is_array=.true.
    end subroutine

    subroutine is_array_sicecl_ob(var_is_array)
        logical, intent(out) :: var_is_array
        var_is_array=.true.
    end subroutine

    subroutine is_array_ticecl_ob(var_is_array)
        logical, intent(out) :: var_is_array
        var_is_array=.true.
    end subroutine

    subroutine is_array_sstan_ob(var_is_array)
        logical, intent(out) :: var_is_array
        var_is_array=.true.
    end subroutine

    subroutine is_array_sstcl_om(var_is_array)
        logical, intent(out) :: var_is_array
        var_is_array=.true.
    end subroutine

    subroutine is_array_sst_am(var_is_array)
        logical, intent(out) :: var_is_array
        var_is_array=.true.
    end subroutine

    subroutine is_array_sstan_am(var_is_array)
        logical, intent(out) :: var_is_array
        var_is_array=.true.
    end subroutine

    subroutine is_array_sice_am(var_is_array)
        logical, intent(out) :: var_is_array
        var_is_array=.true.
    end subroutine

    subroutine is_array_tice_am(var_is_array)
        logical, intent(out) :: var_is_array
        var_is_array=.true.
    end subroutine

    subroutine is_array_sst_om(var_is_array)
        logical, intent(out) :: var_is_array
        var_is_array=.true.
    end subroutine

    subroutine is_array_sice_om(var_is_array)
        logical, intent(out) :: var_is_array
        var_is_array=.true.
    end subroutine

    subroutine is_array_tice_om(var_is_array)
        logical, intent(out) :: var_is_array
        var_is_array=.true.
    end subroutine

    subroutine is_array_ssti_om(var_is_array)
        logical, intent(out) :: var_is_array
        var_is_array=.true.
    end subroutine

    subroutine is_array_wsst_ob(var_is_array)
        logical, intent(out) :: var_is_array
        var_is_array=.true.
    end subroutine

    subroutine is_array_fband(var_is_array)
        logical, intent(out) :: var_is_array
        var_is_array=.true.
    end subroutine

    subroutine is_array_alb_land(var_is_array)
        logical, intent(out) :: var_is_array
        var_is_array=.true.
    end subroutine

    subroutine is_array_alb_sea(var_is_array)
        logical, intent(out) :: var_is_array
        var_is_array=.true.
    end subroutine

    subroutine is_array_alb_surface(var_is_array)
        logical, intent(out) :: var_is_array
        var_is_array=.true.
    end subroutine

    subroutine is_array_snowc(var_is_array)
        logical, intent(out) :: var_is_array
        var_is_array=.true.
    end subroutine

    subroutine is_array_rad_flux(var_is_array)
        logical, intent(out) :: var_is_array
        var_is_array=.true.
    end subroutine

    subroutine is_array_rad_tau2(var_is_array)
        logical, intent(out) :: var_is_array
        var_is_array=.true.
    end subroutine

    subroutine is_array_rad_st4a(var_is_array)
        logical, intent(out) :: var_is_array
        var_is_array=.true.
    end subroutine

    subroutine is_array_rad_strat_corr(var_is_array)
        logical, intent(out) :: var_is_array
        var_is_array=.true.
    end subroutine

    subroutine is_array_lon(var_is_array)
        logical, intent(out) :: var_is_array
        var_is_array=.true.
    end subroutine

    subroutine is_array_lat(var_is_array)
        logical, intent(out) :: var_is_array
        var_is_array=.true.
    end subroutine

    subroutine is_array_lev(var_is_array)
        logical, intent(out) :: var_is_array
        var_is_array=.true.
    end subroutine


    subroutine is_array_current_step(var_is_array)
        logical, intent(out) :: var_is_array
        var_is_array=.false.
    end subroutine

    subroutine is_array_increase_co2(var_is_array)
        logical, intent(out) :: var_is_array
        var_is_array=.false.
    end subroutine

    subroutine is_array_compute_shortwave(var_is_array)
        logical, intent(out) :: var_is_array
        var_is_array=.false.
    end subroutine

    subroutine is_array_air_absortivity_co2(var_is_array)
        logical, intent(out) :: var_is_array
        var_is_array=.false.
    end subroutine

    subroutine is_array_land_coupling_flag(var_is_array)
        logical, intent(out) :: var_is_array
        var_is_array=.false.
    end subroutine

    subroutine is_array_sst_anomaly_coupling_flag(var_is_array)
        logical, intent(out) :: var_is_array
        var_is_array=.false.
    end subroutine

    subroutine is_array_ablco2_ref(var_is_array)
        logical, intent(out) :: var_is_array
        var_is_array=.false.
    end subroutine

    
end module
