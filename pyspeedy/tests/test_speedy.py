import os
import tempfile
from datetime import datetime

import pytest
import xarray as xr
from pyspeedy.speedy import Speedy

start_dates = (
    # Run twice the same date to check if the globals variables in the library are modified.
    (datetime(1982, 1, 1), datetime(1982, 1, 2)),
    (datetime(1982, 1, 1), datetime(1982, 1, 2)),
    # Run for several days
    (datetime(1982, 1, 1), datetime(1982, 1, 4)),
)


@pytest.mark.parametrize("start_date, end_date", start_dates)
def test_speedy_run(start_date, end_date):
    """Run speeedy for 1 month and compare the output with a reference."""

    file_name = end_date.strftime("%Y%m%d%H%M.nc")

    reference_file = os.path.join(os.path.dirname(__file__), "fixtures", file_name)
    reference_ds = xr.open_dataset(reference_file)
    with tempfile.TemporaryDirectory() as tmp_work_dir:
        os.chdir(tmp_work_dir)
        model = Speedy(
            output_dir=tmp_work_dir, start_date=start_date, end_date=end_date
        )
        model.set_sst_anomalies()
        model.default_init()
        model.run()

        model_file = f"./{file_name}"
        model_ds = xr.open_dataset(model_file)

        xr.testing.assert_allclose(model_ds, reference_ds, rtol=1e-06, atol=0)
