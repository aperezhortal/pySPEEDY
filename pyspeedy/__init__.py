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
