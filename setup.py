###############################################################################
# From `numpy/setup.py`
# We need to import setuptools here in order for it to persist in sys.modules.
# Its presence/absence is used in subclassing setup in numpy/distutils/core.py.
# However, we need to run the distutils version of sdist, so import that first
# so that it is in sys.modules
import numpy.distutils.command.sdist  # noqa
import setuptools  # noqa
from numpy.distutils.command.build_ext import build_ext
from numpy.distutils.core import Extension
from pathlib import Path
import os
import sys

###############################################################################

PROJECT_ROOT_DIR = Path(__file__).parent
SPEEDY_F90_SOURCE_DIR = PROJECT_ROOT_DIR / "speedy.f90"
F2CMAP = SPEEDY_F90_SOURCE_DIR.resolve() / ".f2py_f2cmap"

# Use user defined NETCDF library is the "NETCDF" environment variable is set.
NETCDF_DIR = os.environ.get("NETCDF", "/usr")
NETCDF_INCLUDE = os.path.join(NETCDF_DIR, "include")
NETCDF_LIB = os.path.join(NETCDF_DIR, "lib")

SPEEDY_TARGET = os.environ.get("SPEEDY_TARGET", "default")
VALID_TARGETS = ("default", "profile", "debug")

if SPEEDY_TARGET not in VALID_TARGETS:
    print("\nInvalid Makefile target.")
    print(" Supported values: " + ",".join(VALID_TARGETS) + "\n")
    print("Exiting installation.")
    sys.exit(1)

pyspeedy_extension = Extension(
    name="pyspeedy.speedy_driver",
    sources=[
        "speedy.f90/types.f90",
        "speedy.f90/params.f90",
        "speedy.f90/speedy_driver.f90",
    ],
    extra_link_args=[
        "-fopenmp",
        "-L./speedy.f90",
        f"-L{NETCDF_LIB}",
        "-lspeedy",
        "-lnetcdf",
        "-lnetcdff",
    ],
    include_dirs=[numpy.get_include(), NETCDF_INCLUDE],
    f2py_options=["--f2cmap", str(F2CMAP)],
)


class specialized_build_ext(build_ext):
    """
    Specialized builder for the speedy model that uses the Makefile.
    """

    special_extension = pyspeedy_extension.name

    def build_extension(self, ext):
        import subprocess
        from distutils.errors import DistutilsSetupError

        if ext.name == self.special_extension:
            # First compile the special extensions using Make
            make_process = subprocess.Popen(
                ["make", "-C", "speedy.f90", SPEEDY_TARGET], cwd=PROJECT_ROOT_DIR
            )
            stdout, stderr = make_process.communicate()
            print(stdout)
            print(stderr)
            print("-----")
            if make_process.returncode != 0:
                print(PROJECT_ROOT_DIR)
                import sys
                import glob

                print(glob.glob(str(PROJECT_ROOT_DIR / "*")))

                raise DistutilsSetupError(
                    "An error occurred while building the speedy.so"
                )
        # After making the library build the c library's python interface with the
        # parent build_extension method.
        super().build_extension(ext)


if __name__ == "__main__":
    from numpy.distutils.core import setup
    import sys

    sys.path.insert(0, str(PROJECT_ROOT_DIR / "registry"))

    from model_state_def import export_model_state_json, build_fortran_sources  # noqa

    build_fortran_sources()
    export_model_state_json()

    setup(
        ext_modules=[pyspeedy_extension],
        cmdclass={"build_ext": specialized_build_ext},
    )
