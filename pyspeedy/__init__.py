"""
================
pySpeedy package
================

.. autosummary::
    :toctree: ./generated/

    Speedy
    example_bc_file
    example_sst_anomaly_file
"""
from pathlib import Path
from .speedy_driver import speedy_driver as _speedy  # noqa

PACKAGE_DATA_DIR = Path(__file__).parent / "data"


def example_bc_file():
    """Returns the Path to the example bc file."""
    return str(PACKAGE_DATA_DIR / "example_bc.nc")


def example_sst_anomaly_file():
    """Returns the Path to the example SST anomaly file."""
    return str(PACKAGE_DATA_DIR / "sst_anomaly.nc")


from .speedy import Speedy, MODEL_STATE_DEF  # noqa
