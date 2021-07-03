"""
PySPEEDY module.
"""
from pathlib import Path

from pyspeedy.speedy_driver import speedy_driver as _speedy


def example_bc_file():
    """Returns the Path to the example bc file"""
    package_root = Path(__file__).parent
    return str(package_root / "data/example_bc.nc")


class __ModuleInitializer:
    def __init__(self):
        _speedy.initialize()

    def __del__(self):
        _speedy.close()


# When this object is deleted, it will deinitialize the module.
__module_init = __ModuleInitializer()
