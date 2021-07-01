# -*- coding: utf-8 -*-
import os
import subprocess
from distutils.errors import DistutilsSetupError

import numpy
from numpy.distutils.command.build_ext import build_ext
from numpy.distutils.core import Extension
from pathlib import Path

PROJECT_ROOT_DIR = Path(__file__).parent
SPEEDY_SOURCE_DIR = PROJECT_ROOT_DIR / "source"
F2CMAP = SPEEDY_SOURCE_DIR.resolve() / "f2py_f2cmap"

pyspeedy_extension = Extension(
    name="pyspeedy._speedy",
    sources=["source/types.f90", "source/params.f90", "source/pyspeedy.f90"],
    extra_compile_args=[
        "-O2",
        "-ffast-math",
        "-L./source" "-lspeedy",
        "-g",
        "-fbacktrace",
        "-fcheck=bounds",
    ],
    extra_link_args=["-fopenmp", "-L./source", "-lspeedy", "-lnetcdf", "-lnetcdff"],
    include_dirs=[numpy.get_include()],
    f2py_options=["--f2cmap", str(F2CMAP)],
)


class specialized_build_ext(build_ext):
    """
    Specialized builder for the speedy model that uses the Makefile.
    """

    special_extension = pyspeedy_extension.name

    def build_extension(self, ext):
        if ext.name != self.special_extension:
            # Handle unspecial extensions with the parent class' method
            super(specialized_build_ext, self).build_extension(ext)
        else:
            make_process = subprocess.Popen("make", cwd=PROJECT_ROOT_DIR)
            stdout, stderr = make_process.communicate()
            print(stdout)
            print(stderr)
            if make_process.returncode != 0:
                raise DistutilsSetupError(
                    "An error occurred while building the speedy.so"
                )

            # After making the library build the c library's python interface with the
            # parent build_extension method.
            super(specialized_build_ext, self).build_extension(ext)


if __name__ == "__main__":
    from numpy.distutils.core import setup

    setup(
        name="pyspeedy",
        ext_modules=[pyspeedy_extension],
        packages=["pyspeedy"],
        cmdclass={"build_ext": specialized_build_ext},
        package_data={
            "pyspeedy": ["data/*.nc"],
        },
    )
