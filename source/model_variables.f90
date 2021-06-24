!> author: Andres Perez Hortal, Sam Hatfield, Fred Kucharski, Franco Molteni
!  date: 23/06/2021
!  Module used to declare the prognostic spectral variables for model dynamics,
!  the geopotential, and the auxiliary variables used by multiple physics schemes.
!
!  All the Speedy model variables are defined inside the ModelVars_t type that holds
!  all the information.
module model_variables
    use types, only: p

    implicit none

    private

    public ModelVars_t, ModelVars_allocate, ModelVars_deallocate

    type ModelVars_t
        ! Prognostic and auxiliary variables

        ! Here only the pointers to the data arrays are declared.
        ! The instances of this data type need to be initialized (allocated) before
        ! hand.

        ! Why pointers?
        ! This is to make things easier for the Python-Fortran brigde.
        ! f2py does not support derived type declararions.
        ! Hence, the prognostic variables needs to be passed to the main driver of the
        ! speedy model (speedy.90).
        ! To avoid coping the input variables into the attributes of this structure,
        ! we declare them as pointers.
        ! Then, we can make the structure attributes to point to the input variables.
        ! See the "Step 0" in the run_speedy subroutine in the speedy.f90 module.

        !===========================================================================================
        ! Prognostic variables
        !===========================================================================================
        ! kx Number of vertical levels
        ! nx Number of total wavenumbers for spectral storage arrays
        ! mx Number of zonal wavenumbers for spectral storage arrays
        complex(p), pointer :: vor(:, :, :, :)   !! Vorticity
        complex(p), pointer :: div(:, :, :, :)   !! Divergence
        complex(p), pointer :: t(:, :, :, :)     !! Absolute temperature
        complex(p), pointer :: ps(:, :, :)       !! Log of (normalised) surface pressure (p_s/p0)
        complex(p), pointer :: tr(:, :, :, :, :) !! Tracers (tr(1): specific humidity in g/kg)

        ! Geopotential
        complex(p), pointer :: phi(:, :, :) !! Atmospheric geopotential
        complex(p), pointer :: phis(:, :)   !! Surface geopotential

        !===========================================================================================
        ! Auxiliary variables used by the physic schemes
        !===========================================================================================
        ! Physical variables shared among all physics schemes
        real(p), pointer, dimension(:, :)   :: precnv !! Convective precipitation  [g/(m^2 s)], total
        real(p), pointer, dimension(:, :)   :: precls !! Large-scale precipitation [g/(m^2 s)], total
        real(p), pointer, dimension(:, :)   :: snowcv !! Convective precipitation  [g/(m^2 s)], snow only
        real(p), pointer, dimension(:, :)   :: snowls !! Large-scale precipitation [g/(m^2 s)], snow only
        real(p), pointer, dimension(:, :)   :: cbmf   !! Cloud-base mass flux
        real(p), pointer, dimension(:, :)   :: tsr    !! Top-of-atmosphere shortwave radiation (downward)
        real(p), pointer, dimension(:, :)   :: ssrd   !! Surface shortwave radiation (downward-only)
        real(p), pointer, dimension(:, :)   :: ssr    !! Surface shortwave radiation (net downward)
        real(p), pointer, dimension(:, :)   :: slrd   !! Surface longwave radiation (downward-only)
        real(p), pointer, dimension(:, :)   :: slr    !! Surface longwave radiation (net upward)
        real(p), pointer, dimension(:, :)   :: olr    !! Outgoing longwave radiation (upward)

        ! Third dimension -> 1:land, 2:sea, 3: weighted average
        real(p), pointer, dimension(:, :, :) :: slru   !! Surface longwave emission (upward)
        real(p), pointer, dimension(:, :, :) :: ustr   !! U-stress
        real(p), pointer, dimension(:, :, :) :: vstr   !! V-stress
        real(p), pointer, dimension(:, :, :) :: shf    !! Sensible heat flux
        real(p), pointer, dimension(:, :, :) :: evap   !! Evaporation [g/(m^2 s)]
        real(p), pointer, dimension(:, :, :) :: hfluxn !! Net heat flux into surface
    end type

contains

    !> Allocate PrognosticVars attributes.
    subroutine ModelVars_allocate(model_vars)
        use params, only: mx, nx, kx, ntr, ix, il
        type(ModelVars_t), intent(out) :: model_vars

        !========================================================
        ! Allocate prognostic variables
        allocate (model_vars%vor(mx, nx, kx, 2))
        allocate (model_vars%div(mx, nx, kx, 2))
        allocate (model_vars%t(mx, nx, kx, 2))
        allocate (model_vars%ps(mx, nx, 2))
        allocate (model_vars%tr(mx, nx, kx, 2, ntr))

        allocate (model_vars%phi(mx, nx, kx))
        allocate (model_vars%phis(mx, nx))

        !========================================================
        ! Allocate auxiliary variables used by the physic schemes
        allocate (model_vars%precnv(ix, il))
        allocate (model_vars%precls(ix, il))
        allocate (model_vars%snowcv(ix, il))
        allocate (model_vars%snowls(ix, il))
        allocate (model_vars%cbmf(ix, il))
        allocate (model_vars%tsr(ix, il))
        allocate (model_vars%ssrd(ix, il))
        allocate (model_vars%ssr(ix, il))
        allocate (model_vars%slr(ix, il))
        allocate (model_vars%slrd(ix, il))        
        allocate (model_vars%olr(ix, il))
        ! Third dimension -> 1:land, 2:sea, 3: weighted average
        allocate (model_vars%slru(ix, il, 3))
        allocate (model_vars%ustr(ix, il, 3))
        allocate (model_vars%vstr(ix, il, 3))
        allocate (model_vars%shf(ix, il, 3))
        allocate (model_vars%evap(ix, il, 3))
        allocate (model_vars%hfluxn(ix, il, 3))

        !=====================================
        ! Initialize to 0 the allocated arrays
        model_vars%vor = 0
        model_vars%div = 0
        model_vars%t = 0
        model_vars%ps = 0
        model_vars%tr = 0
        model_vars%phi = 0
        model_vars%phis = 0

        model_vars%precnv = 0
        model_vars%precls = 0
        model_vars%snowcv = 0
        model_vars%snowls = 0
        model_vars%cbmf = 0
        model_vars%tsr = 0
        model_vars%ssrd = 0
        model_vars%ssr = 0
        model_vars%slr = 0
        model_vars%slrd = 0        
        model_vars%olr = 0
        model_vars%slru = 0
        model_vars%ustr = 0
        model_vars%vstr = 0
        model_vars%shf = 0
        model_vars%evap = 0
        model_vars%hfluxn = 0
    end subroutine

    !> Deallocate the memory reserved for the prognostic variables.
    subroutine ModelVars_deallocate(model_vars)
        type(ModelVars_t), intent(inout) :: model_vars
        ! Deallocate variables
        deallocate (model_vars%vor)
        deallocate (model_vars%div)
        deallocate (model_vars%t)
        deallocate (model_vars%ps)
        deallocate (model_vars%tr)
        deallocate (model_vars%phi)
        deallocate (model_vars%phis)

        deallocate (model_vars%precnv)
        deallocate (model_vars%precls)
        deallocate (model_vars%snowcv)
        deallocate (model_vars%snowls)
        deallocate (model_vars%cbmf)
        deallocate (model_vars%tsr)
        deallocate (model_vars%ssrd)
        deallocate (model_vars%ssr)
        deallocate (model_vars%slr)
        deallocate (model_vars%olr)
        deallocate (model_vars%slru)
        deallocate (model_vars%ustr)
        deallocate (model_vars%vstr)
        deallocate (model_vars%shf)
        deallocate (model_vars%evap)
        deallocate (model_vars%hfluxn)
    end subroutine

end module
