"""
pySpeedy main module
====================

.. autosummary::
    :toctree: ../generated/

    example_bc_file
    example_sst_anomaly_file
    Speedy
    SpeedyEns
"""
from pathlib import Path
from .speedy_driver import speedy_driver as _speedy  # noqa

PACKAGE_DATA_DIR = Path(__file__).parent / "data"

DEFAULT_OUTPUT_VARS = (
    "u_grid",
    "v_grid",
    "t_grid",
    "q_grid",
    "phi_grid",
    "ps_grid",
)


def example_bc_file():
    """Returns the Path to the example bc file."""
    return str(PACKAGE_DATA_DIR / "example_bc.nc")


def example_sst_anomaly_file():
    """Returns the Path to the example SST anomaly file."""
    return str(PACKAGE_DATA_DIR / "sst_anomaly.nc")


from .speedy import Speedy, SpeedyEns, MODEL_STATE_DEF  # noqa
