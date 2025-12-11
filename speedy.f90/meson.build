# speedy.f90/meson.build
project_fortran = true

# list your Fortran sources (the same files you used in setup.py)
f90_sources = [
  'types.f90',
  'params.f90',
  'speedy_driver.f90',
]

# Build the Fortran library
speedy_lib = static_library(
  'speedy_core',
  f90_sources,
  install : false,
  interruptible : true,
)

# You may prefer shared_library if you want dynamic linking:
# speedy_lib = shared_library('speedy_core', f90_sources, install: false)

# Compose flags for f2py; include numpy include dir at build time
# meson can get numpy include dir by invoking python -c "import numpy; print(numpy.get_include())"
py = import('python')
py3 = py.find_installation(pure: false)
numpy_include_cmd = [py3.executable(), '-c', "import numpy; print(numpy.get_include())"]

# Create a custom target to compute numpy include dir at configure/build time
numpy_inc = run_command(numpy_include_cmd).stdout().strip()

# F2PY wrapper generation command (produce a .pyf or C file + build .so)
# We will ask f2py to compile directly into an extension module named pyspeedy.speedy_driver
f2py_cmd = [
  py3.executable(), '-m', 'numpy.f2py',
  '-m', 'pyspeedy.speedy_driver',
  '-c',
] + [
  # source files to f2py (fortran)
] + [
  # additional f2py args
]

# Add the fortran sources (relative to this dir)
foreach s : f90_sources
  f2py_cmd += s
endforeach

# Pass numpy include and any other include dirs
f2py_cmd += ['--include-paths=' + numpy_inc]

# Optionally: pass extra link args / library dirs so f2py links to speedy_core (if needed)
# But f2py normally compiles Fortran sources into a python module itself. Because we
# already built a speedy_lib it might be preferable to link to it. For simplicity,
# we'll let f2py build the extension directly (useful if your Makefile previously built "speedy" lib).
#
# Create a custom target that runs f2py and produces the extension .so as an output
f2py_target = custom_target(
  'f2py_wrap_speedy',
  command : f2py_cmd,
  output : 'pyspeedy_speedy_driver.*',   # Meson will match the produced .so name, be permissive
  capture : true
)

# Install the produced extension into the pyspeedy package directory so wheel includes it
install_data(f2py_target, install_dir: join_paths(py3.get_install_dir(), 'pyspeedy'))
