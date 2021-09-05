import os
import tempfile
from datetime import datetime

import pytest
import xarray as xr
from pyspeedy.speedy import Speedy

start_dates = (
    # Run twice the same date to check if the globals variables in the library are modified.
    datetime(1982, 1, 1),
    # datetime(1982, 1, 1),
)


@pytest.mark.parametrize("start_date", start_dates)
def test_speedy_run(start_date):
    """Run speeedy for 1 month and compare the output with a reference."""
    reference_file = os.path.join(os.path.dirname(__file__), "fixtures/198201020000.nc")
    reference_ds = xr.open_dataset(reference_file)
    with tempfile.TemporaryDirectory() as tmp_work_dir:
        os.chdir(tmp_work_dir)
        model = Speedy(output_dir=tmp_work_dir, start_date=start_date)
        model.set_sst_anomalies()
        model.default_init()
        model.run()

        model_file = "./198201020000.nc"
        model_ds = xr.open_dataset(model_file)

        xr.testing.assert_allclose(
            model_ds, reference_ds, rtol=1e-06, atol=0
        )
