# -*- coding: utf-8 -*-
import os
import subprocess
from distutils.errors import DistutilsSetupError

import numpy
from numpy.distutils.command.build_ext import build_ext
from numpy.distutils.core import Extension

SPEEDY_SOURCE_DIR = os.path.join(os.path.dirname(__file__), "source")
speedy_gcm = Extension(name='speedy_f90._speedy',
                       sources=['source/speedy.f90'],
                       extra_compile_args=[
                           "-O2", 
                           "-ffast-math", 
                           "-L./source" "-lspeedy",
                           "-g", "-fbacktrace", "-fcheck=bounds"
                       ],
                       extra_link_args=["-fopenmp", "-L./source", "-lspeedy",
                                        "-lnetcdf", "-lnetcdff"],
                       include_dirs=[numpy.get_include()],
                       )


class specialized_build_ext(build_ext):
    """
    Specialized builder for the speedy model.
    """
    special_extension = speedy_gcm.name

    def build_extension(self, ext):
        if ext.name != self.special_extension:
            # Handle unspecial extensions with the parent class' method
            super(specialized_build_ext, self).build_extension(ext)
        else:
            make_process = subprocess.Popen("make", cwd=SPEEDY_SOURCE_DIR)
            stdout, stderr = make_process.communicate()
            print(stdout)
            print(stderr)
            if make_process.returncode != 0:
                raise DistutilsSetupError(
                    "An error occurred while building the speedy.so"
                )

            # After making the library build the c library's python interface with the parent build_extension method
            super(specialized_build_ext, self).build_extension(ext)


if __name__ == "__main__":
    from numpy.distutils.core import setup

    setup(name='speedy-f90',
          ext_modules=[speedy_gcm],
          packages=['speedy_f90'],
          cmdclass={'build_ext': specialized_build_ext},
          )
