========
pySPEEDY
========


What is pySPEEDY?
=================

pySPEEDY is a fork of the `SPEEDY.f90 <https://github.com/samhatfield/speedy.f90>`_ atmospheric model written
by `Sam Hatfield <https://samhatfield.co.uk/>`_ that uses a python interface for running and controlling
the SPEEDY.f90 model.

The SPEEDY.f90 is a modern Fortran implementation of the intermediate complexity atmospheric
general circulation
`SPEEDY model <http://users.ictp.it/~kucharsk/speedy_description/km_ver41_appendixA.pdf>`_,
developed by Fred Kucharski, Franco Molteni, and Martin P. King.


Documentation
=============

Check the latest documentation of the project `here <https://pyspeedy.readthedocs.io/en/latest/index.html>`_.


What are the differences between pySPEEDY and SPEEDY.f90?
=========================================================

The pySPEEDY model is built on top of the SPEEDY.f90. However, to control the SPEEDY.f90 model from python,
the original SPEEDY.f90 code was considerably refactored to make it thread-safe and allow multiples instances of the
Speedy model to run in parallel.

Perhaps the most significant change is the encapsulation of all the model state variables in a Fortran data type,
called `ModelState_t`, and the use of the jinja template engine to generate the model state declarations and the
python interface.

Additionally, to expose the state variables to python and facilitate memory management,
pySPEEDY uses **variable registry** (the `registry/model_state_def.py` file) to define all the state variables.
These changes were inspired by the "grid" data type and the registry used by the Weather Research and Forecasting (WRF)
model for managing the state variables.

The pySPEEDY **variable registry** and the Jinja template engine are used to generate the necessary Fortran sources
for defining the model state, the allocation/deallocation functions, and the python interface.
One of the reasons to use programmatic generation of the sources is to facilitate the maintenance of the code.

Another important element regarding the python interface is how python communicates back and forth with Fortran.
The interface mostly build using the
`F2PY–Fortran to Python interface generator <https://numpy.org/doc/stable/f2py/>`_. However, since F2PY does not
support derived data types like the one used in the *Model State*, the variables contained in the *Model State* were
exposed using the "container" approach described in
`Pletzer et al., 2008 <https://doi.org/10.1109/MCSE.2008.94>`_.

Finally, to allow running different instances of the speedy model in parallel, each Speedy instance
was made self-contained.
That means that all the variables needed to run the model are contained inside the "Model State".
To make each model instance self-contained, all the global variables defined across the modules were removed and moved to the **ModelState_t** structure.
This was done in two parts. First, the model state variables were directly added as attributes in the **ModelState_t**
structure. The other change was encapsulating the Legendre, Fourier, Spectral, Diffusion, Geometry, and Implicit
Fortran modules into a Fortran data type and add an instance of each module to the model state.

Simple usage example
====================

The following example runs the SPEEDY model for four days, from 1982/01/01 to 1982/01/04,
using the default initial and boundary conditions::

    from datetime import datetime
    from pyspeedy.speedy import Speedy

    work_dir = "./data"
    model = Speedy(
        output_dir=work_dir, start_date=datetime(1982, 1, 1), end_date=datetime(1982, 1, 4)
    )
    model.set_sst_anomalies()
    model.default_init()
    model.run()

The data is saved inside the `./data` in Netcdf format (CF convention).

Check a more detailed tutorial
`here <https://colab.research.google.com/github/pySTEPS/pysteps/blob/master/examples/my_first_nowcast.ipynb>`_.

Installation
============

To install the pySPEEDY model, you need jinja2 and numpy installed already in your system.
To install the package from source::

    # Clone the repo
    git clone https://github.com/aperezhortal/pySPEEDY.git

    # Install the package
    pip install ./pySPEEDY


Runtime dependencies
====================

These are the minimal dependencies needed to run pySPEEDY:

- numpy
- xarray
- python-dateutil
