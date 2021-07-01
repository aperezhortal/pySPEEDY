!> author: Sam Hatfield, Fred Kucharski, Franco Molteni
!  date: 29/04/2019
!
!  contributor: Andres Perez Hortal
!     
!  This module initialize the land-sea mask, the surface geopotential 
!  (i.e. the orography), the filtered surface geopotential (i.e. the smoothed orography)
!   and the bare-land annual-mean albedo.
module boundaries
    use types, only: p
    use params

    implicit none

    private
    public initialize_boundaries, fillsf, check_surface_fields
   

contains
    !> Initialize boundary conditions (land-sea mask, surface geopotential
    !  and surface albedo).
    subroutine initialize_boundaries(state)
        use physical_constants, only: grav
        use input_output, only: load_boundary_file
        use model_state, only: ModelState_t
        type(ModelState_t), intent(inout) :: state

        state%phi0 = grav*state%orog
        
        ! IMPORTANT: The following variables show be initilized in the model state
        ! before calling this function. 
        ! - Annual-mean surface albedo (state%alb0)
        ! - Land-sea mask (state%fmask_orig)
        ! - Annual-mean surface albedo (state%alb0)

        ! Initialize the spectrally truncated surface geopotential
        call spectral_truncation(state%phi0, state%phis0)        
    end subroutine

    !> Check consistency of surface fields with land-sea mask and set undefined
    !  values to a constant (to avoid over/underflow).
    subroutine check_surface_fields(fmask, nf, fmin, fmax, fset, field)
        real(p), intent(in)    :: fmask(ix,il)    !! The fractional land-sea mask
        integer, intent(in)    :: nf              !! The number of input 2D fields
        real(p), intent(in)    :: fmin            !! The minimum allowable value
        real(p), intent(in)    :: fmax            !! The maximum allowable value
        real(p), intent(in)    :: fset            !! Replacement for undefined values
        real(p), intent(inout) :: field(ix,il,nf) !! The output field

        integer :: i, j, jf, nfault
        do jf = 1, nf
            nfault = 0
            do i = 1, ix
                do j = 1, il
                    if (fmask(i,j) > 0.0) then
                        if (field(i,j,jf) < fmin .or. field(i,j,jf) > fmax) then
                            nfault = nfault + 1
                        end if
                    else
                        field(i,j,jf) = fset
                    end if
                end do
            end do
        end do
    end subroutine

    !> Compute a spectrally-filtered grid-point field.
    subroutine spectral_truncation(fg1, fg2)
        use spectral, only: grid_to_spec, spec_to_grid

        real(p), intent(inout) :: fg1(ix,il) !! Original grid-point field
        real(p), intent(inout) :: fg2(ix,il) !! Filtered grid-point field

        complex(p) :: fsp(mx,nx)
        integer :: n, m, total_wavenumber

        fsp = grid_to_spec(fg1)

        do n = 1, nx
            do m = 1, mx
                total_wavenumber = m + n - 2
                if (total_wavenumber > trunc) fsp(m,n) = (0.0, 0.0)
            end do
        end do

        fg2 = spec_to_grid(fsp, 1)
    end subroutine

    !> Replace missing values in surface fields.
    !  @note It is assumed that non-missing values exist near the Equator.
    subroutine fillsf(sf, fmis)
        real(p), intent(inout) :: sf(ix,il) !! Field to replace missing values in
        real(p), intent(in)    :: fmis      !! Replacement for missing values

        real(p) :: sf2(0:ix+1)

        integer :: hemisphere, j, j1, j2, j3, i, nmis
        real(p) :: fmean = 0 

        do hemisphere = 1, 2
           if (hemisphere == 1) then
                j1 = il/2
                j2 = 1
                j3 = -1
            else
                j1 = j1+1
                j2 = il
                j3 = 1
            end if

            do j = j1, j2, j3
                sf2(1:ix) = sf(:,j)

                nmis = 0
                do i = 1, ix
                    if (sf(i,j) < fmis) then
                        nmis = nmis + 1
                        sf2(i) = 0.0
                    end if
                end do

                if (nmis < ix) fmean = sum(sf2(1:ix))/float(ix - nmis)

                do i = 1, ix
                    if (sf(i,j) < fmis) sf2(i) = fmean
                end do

                sf2(0)      = sf2(ix)
                sf2(ix+1) = sf2(1)
                do i = 1, ix
                    if (sf(i,j) < fmis) sf(i,j) = 0.5*(sf2(i-1) + sf2(i+1))
                end do
            end do
        end do
    end subroutine
end module
