"""
PySPEEDY module.
"""
from pathlib import Path

from pyspeedy.speedy_driver import speedy_driver as _speedy

PACKAGE_DATA_DIR = Path(__file__).parent / "data"


def example_bc_file():
    """Returns the Path to the example bc file."""
    return str(PACKAGE_DATA_DIR / "example_bc.nc")


def example_sst_anomaly_file():
    """Returns the Path to the example SST anomaly file."""
    return str(PACKAGE_DATA_DIR / "sst_anomaly.nc")


##############
# Housekeeping


class __ModuleInitializer:
    def __init__(self):
        """Initialize constant variables the Speedy submodules."""
        _speedy.initialize_module()

    def __del__(self):
        """Deallocate all the constant variables in the Speedy submodules."""
        _speedy.close_module()


# When this object is deleted, it will deinitialize the module.
__module_init = __ModuleInitializer()
