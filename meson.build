project('pyspeedy', 'c', 'fortran', version: '0.2.0')

py = import('python').find_installation(required: true, pure: false)
numpy_dep = dependency('numpy', modules: ['numpy'], required: true)
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

# Generate the Python extension module
speedy_driver_module = custom_target(
    'speedy_driver_module',
    input: speedy_sources,
    output: 'speedy_driver.cpython-311-x86_64-linux-gnu.so',
    command: [
        f2py,
        '-c', '@INPUT@',
        '-m', 'speedy_driver',
        '--f90flags=-fopenmp -O3',
        '-L' + meson.current_source_dir(), '-lspeedy',
        '-lnetcdf', '-lnetcdff', '-lgomp'
    ],
    depends: [speedy_lib],
    install: true,
    install_dir: py.get_install_dir() / 'pyspeedy'
)

# --------------------------------------------------
# Step 3: Install Python modules
# --------------------------------------------------
install_subdir('pyspeedy',
    install_dir: py.get_install_dir(),
)
