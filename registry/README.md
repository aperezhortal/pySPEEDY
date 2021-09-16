# Model variables registry

In the PySPEEDY project we use a "registry" to define the model state variables in
a similar was as the WRF model. This registry is then use to generate the fortran
boilerplate code needed for the project.

In a nutshell, the `model_state_def.py` scripts does the following:

1. Declare the model state variables (name, description, dimension, etc.)
2. Use jinja2 templates to generate:
   - the `model_state.f90` module.
   - the `pyspeedy.f90` module used for the python-fortran interface with the speedy
     model.
