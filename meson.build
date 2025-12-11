project('pyspeedy', 'c', 'fortran', version: '0.2.0')

py = import('python').find_installation(required: true, pure: false)
numpy_dep = dependency('numpy', modules: ['numpy'], required: true)

incdir_numpy = run_command(py,
  ['-c', 'import os; os.chdir(".."); import numpy; print(numpy.get_include())'],
  check : true
).stdout().strip()

incdir_f2py = run_command(py,
    ['-c', 'import os; os.chdir(".."); import numpy.f2py; print(numpy.f2py.get_include())'],
    check : true
).stdout().strip()

inc_np = include_directories(incdir_numpy, incdir_f2py)

# Unlike distutils, meson doesn't yet include some of the f2py stuff
fortranobject_c = incdir_f2py / 'fortranobject.c'

fortranobject_lib = static_library('_fortranobject',
  fortranobject_c,
  dependencies: py.dependency(),
  include_directories: [incdir_numpy, incdir_f2py])
  
netcdf_dep_c = dependency('netcdf', language: 'c')
netcdf_dep_f = dependency('netcdf', language: 'fortran')

# --------------------------------------------------
# Step 1: Build the Fortran library using Makefile
# --------------------------------------------------
speedy_lib = custom_target(
  'speedy_library',
  command: ['make', '-d', '-C', meson.current_source_dir(), 'all'],
  output: 'makefile.output',
  capture: true

)

speedy_lib_dep= declare_dependency (sources: speedy_lib)

# --------------------------------------------------
# Step 2: Python extension module
# --------------------------------------------------

# --------------------------------------------------
# PySpeedy driver sources
# --------------------------------------------------
speedy_sources = files(
    'speedy.f90/types.f90',
    'speedy.f90/params.f90',
    'speedy.f90/speedy_driver.f90',
)

# --------------------------------------------------
# Python extension module
# --------------------------------------------------
f2py = find_program('f2py')

speedy_driver_module = custom_target(
    'speedy_driver_module',
    input: speedy_sources,
    output: ['speedy_drivermodule.c', 'speedy_driver-f2pywrappers2.f90'],
    command: [
        f2py,
        '@INPUT@',
        '-m', 'speedy_driver',
        '--f2cmap', meson.current_source_dir()+'/speedy.f90/.f2py_f2cmap'
    ],
    depends: [speedy_lib]
)

# Generate the Python extension module
py.extension_module(
  'speedy_driver',
  [speedy_sources, speedy_driver_module, incdir_f2py / 'fortranobject.c'],
  dependencies : [numpy_dep, netcdf_dep_c, netcdf_dep_f],
  include_directories: inc_np,
  link_with: fortranobject_lib,
  link_args: [
    '-L' + meson.current_source_dir(),  # -L<source_dir>
    '-lspeedy',                         # -lspeedy
    '-lnetcdf',                         # -lnetcdf
    '-lnetcdff',                        # -lnetcdff
    '-lgomp',                           # -lgomp
  ],
  install : true,
  install_dir: py.get_install_dir() / 'pyspeedy'
)


# --------------------------------------------------
# Step 3: Install Python modules
# --------------------------------------------------
install_subdir('pyspeedy',
    install_dir: py.get_install_dir(),
)

