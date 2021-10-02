import os
import tempfile
from datetime import datetime, timedelta

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
        model = Speedy(
            output_dir=tmp_work_dir, start_date=start_date, end_date=end_date
        )
        model.set_bc()
        model.run()

        model_file = os.path.join(tmp_work_dir, file_name)
        model_ds = xr.open_dataset(model_file)

        xr.testing.assert_allclose(model_ds, reference_ds, rtol=1e-06, atol=0)


def test_speedy_concurrent():
    """Run serially 2 speedy models for 4 days, alternating each model run every one day of simulation."""
    start_date = datetime(1982, 1, 1)
    end_date = datetime(1982, 1, 4)
    ndays = 3
    file_name = end_date.strftime("%Y%m%d%H%M.nc")

    reference_file = os.path.join(os.path.dirname(__file__), "fixtures", file_name)
    reference_ds = xr.open_dataset(reference_file)
    with tempfile.TemporaryDirectory() as tmp_work_dir:
        tmp_work_dir1 = os.path.join(tmp_work_dir, "run1")
        tmp_work_dir2 = os.path.join(tmp_work_dir, "run2")

        model = Speedy(
            output_dir=tmp_work_dir1, start_date=start_date, end_date=end_date
        )
        model.set_bc()

        # Create another speedy instance
        model2 = Speedy(
            output_dir=tmp_work_dir2, start_date=start_date, end_date=end_date
        )
        model2.set_bc()
        model2._set_sst_anomalies()

        for day in range(ndays):
            model.start_date = start_date + timedelta(days=day)
            model.end_date = start_date + timedelta(days=day + 1)
            model.run()

            model2.start_date = start_date + timedelta(days=day)
            model2.end_date = start_date + timedelta(days=day + 1)
            model2.run()

        model_file = os.path.join(tmp_work_dir1, file_name)
        model_ds = xr.open_dataset(model_file)
        xr.testing.assert_allclose(model_ds, reference_ds, rtol=1e-06, atol=0)

        model_file = os.path.join(tmp_work_dir2, file_name)
        model_ds = xr.open_dataset(model_file)
        xr.testing.assert_allclose(model_ds, reference_ds, rtol=1e-06, atol=0)
