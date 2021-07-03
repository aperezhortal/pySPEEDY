# Set variables depending on target
default : COMPOPTS = $(OPT) $(BASE) -fPIC  -g -fbacktrace -fcheck=bounds
profile : COMPOPTS = -pg $(BASE)

# Default
default : base_target

# For profiling
profile : base_target

# Base compiler options (always used)
BASE=-fconvert=swap -Wall -cpp

# Optimisation flags (disabled for debugging, profiling etc.)
OPT= -Ofast

# # Location of NetCDF module (netcdf.mod)
INC=-I/usr/include
INC=-I/home/aperez/.conda/envs/speedy/include

# -I/usr/include

# Library flags
#-L/usr/lib
LIB=-L$/usr/lib -lnetcdff -lnetcdf
LIB=-L/home/aperez/.conda/envs/speedy/lib -lnetcdff -lnetcdf

FILES= \
       error_codes.o \
       model_state.o \
	   boundaries.o \
	   convection.o \
       coupler.o \
	   model_control.o \
	   diagnostics.o \
	   dynamical_constants.o \
	   forcing.o \
	   fourier.o \
	   geometry.o \
 	   geopotential.o \
	   horizontal_diffusion.o \
	   humidity.o \
	   implicit.o \
       initialization.o \
	   land_model.o \
	   large_scale_condensation.o \
	   legendre.o \
	   longwave_radiation.o \
	   matrix_inversion.o \
	   input_output.o \
	   interpolation.o \
	   mod_radcon.o \
	   params.o \
	   physics.o \
	   physical_constants.o \
	   prognostics.o \
	   sea_model.o \
	   shortwave_radiation.o \
	   spectral.o \
       fftpack.o \
	   sppt.o \
	   surface_fluxes.o \
	   tendencies.o \
	   time_stepping.o \
	   types.o \
	   vertical_diffusion.o \
	   speedy.o \
	   speedy_driver.o


%.o: %.f90
	gfortran $(COMPOPTS) -c $< $(INC)

base_target: $(FILES)
	ar rcs libspeedy.a $(FILES)

.PHONY: clean
clean:
	rm -f *.o *.mod speedy libspeedy.so libspeedy.a

error_codes.o          :
speedy_driver.o        : model_state.o speedy.o
speedy.o               : params.o model_control.o prognostics.o input_output.o \
				         shortwave_radiation.o time_stepping.o error_codes.o \
                         diagnostics.o model_state.o 
model_state.o          : types.o params.o
boundaries.o           : physical_constants.o params.o input_output.o spectral.o types.o
convection.o           : params.o physical_constants.o types.o
coupler.o              : land_model.o sea_model.o
model_control.o        : types.o
dynamical_constants.o  : types.o
fourier.o              : params.o geometry.o fftpack.o types.o
geometry.o             : params.o physical_constants.o types.o
geopotential.o         : params.o physical_constants.o geometry.o
horizontal_diffusion.o : params.o dynamical_constants.o types.o
humidity.o             : params.o types.o
implicit.o             : params.o dynamical_constants.o physical_constants.o geometry.o\
                         horizontal_diffusion.o matrix_inversion.o types.o
initialization.o       : coupler.o params.o model_control.o input_output.o time_stepping.o boundaries.o\
                         spectral.o sea_model.o physics.o geopotential.o \
						 prognostics.o forcing.o model_state.o
forcing.o              : dynamical_constants.o shortwave_radiation.o params.o \
                         physical_constants.o boundaries.o model_control.o land_model.o mod_radcon.o\
						 surface_fluxes.o model_control.o sea_model.o longwave_radiation.o humidity.o\
						 horizontal_diffusion.o types.o
land_model.o           : params.o model_control.o interpolation.o input_output.o boundaries.o\
                         types.o model_state.o
large_scale_condensation.o : params.o physical_constants.o types.o
legendre.o             : params.o physical_constants.o geometry.o types.o
matrix_inversion.o     : types.o
diagnostics.o          : params.o spectral.o types.o
prognostics.o          : params.o dynamical_constants.o physical_constants.o geometry.o\
                         boundaries.o diagnostics.o spectral.o input_output.o types.o
input_output.o         : params.o  physical_constants.o model_control.o spectral.o geometry.o types.o
interpolation.o        : params.o model_control.o types.o
physical_constants.o   : params.o types.o
mod_radcon.o           : params.o types.o
params.o               : types.o
physics.o              : params.o coupler.o physical_constants.o boundaries.o land_model.o\
                         sea_model.o sppt.o convection.o large_scale_condensation.o surface_fluxes.o\
                         vertical_diffusion.o shortwave_radiation.o longwave_radiation.o humidity.o\
                         geometry.o  types.o model_state.o
longwave_radiation.o   : params.o physical_constants.o mod_radcon.o geometry.o types.o \
						 model_state.o
sea_model.o            : params.o input_output.o boundaries.o geometry.o interpolation.o\
 						 model_control.o  mod_radcon.o types.o model_state.o
shortwave_radiation.o  : params.o mod_radcon.o geometry.o types.o model_state.o
surface_fluxes.o       : params.o physical_constants.o mod_radcon.o land_model.o humidity.o types.o
spectral.o             : params.o physical_constants.o legendre.o fourier.o types.o
sppt.o                 : params.o physical_constants.o spectral.o types.o
tendencies.o           : params.o implicit.o prognostics.o physical_constants.o geometry.o\
                         physics.o spectral.o geopotential.o types.o
time_stepping.o        : dynamical_constants.o params.o prognostics.o tendencies.o\
                         horizontal_diffusion.o types.o
vertical_diffusion     : params.o physical_constants.o geometry.o types.o
