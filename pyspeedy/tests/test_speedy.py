import os
import tempfile
from datetime import datetime, timedelta

import pytest
import xarray as xr

from pyspeedy.callbacks import XarrayExporter
from pyspeedy.speedy import Speedy, SpeedyEns

start_dates = (
    # Run twice the same date to check if the globals variables in the library are modified.
    (datetime(1982, 1, 1), datetime(1982, 1, 2)),
    (datetime(1982, 1, 1), datetime(1982, 1, 2)),
    # Run for several days
    (datetime(1982, 1, 1), datetime(1982, 1, 4)),
)

export_variables = (
    ["u_grid", "v_grid"],
    ["t_grid", "q_grid"],
    ["phi_grid", "ps_grid"],
    ["precnv", "precls"],
)


@pytest.mark.parametrize("start_date, end_date", start_dates)
def test_speedy_run(start_date, end_date):
    """Run speedy and compare the output with a reference."""

    file_name = end_date.strftime("%Y-%m-%d_%H%M.nc")

    reference_file = os.path.join(os.path.dirname(__file__), "fixtures", file_name)
    reference_ds = xr.open_dataset(reference_file)
    with tempfile.TemporaryDirectory() as tmp_work_dir:
        model = Speedy(start_date=start_date, end_date=end_date)
        model.set_bc()
        model.run(callbacks=[XarrayExporter(output_dir=tmp_work_dir)])

        model_file = os.path.join(tmp_work_dir, file_name)
        model_ds = xr.open_dataset(model_file)

        # Let's compare using different relative tolerance.
        # By doing so, if the fields differ, the we can identify how far away they are.
        # ALL THESE TESTS SHOULD PASS.
        xr.testing.assert_allclose(model_ds, reference_ds, rtol=1e-01, atol=0)
        xr.testing.assert_allclose(model_ds, reference_ds, rtol=1e-02, atol=0)
        xr.testing.assert_allclose(model_ds, reference_ds, rtol=1e-03, atol=0)
        xr.testing.assert_allclose(model_ds, reference_ds, rtol=1e-04, atol=0)
        xr.testing.assert_allclose(model_ds, reference_ds, rtol=1e-06, atol=0)


def test_speedy_concurrent():
    """Run serially 2 speedy models for 4 days, alternating each model run every one day of simulation."""
    start_date = datetime(1982, 1, 1)
    end_date = datetime(1982, 1, 4)
    ndays = 3
    file_name = end_date.strftime("%Y-%m-%d_%H%M.nc")

    reference_file = os.path.join(os.path.dirname(__file__), "fixtures", file_name)
    reference_ds = xr.open_dataset(reference_file)
    with tempfile.TemporaryDirectory() as tmp_work_dir:
        tmp_work_dir1 = os.path.join(tmp_work_dir, "run1")
        tmp_work_dir2 = os.path.join(tmp_work_dir, "run2")

        model = Speedy(start_date=start_date, end_date=end_date)
        model.set_bc()

        # Create another speedy instance
        model2 = Speedy(start_date=start_date, end_date=end_date)
        model2.set_bc()

        for day in range(ndays):
            model.start_date = start_date + timedelta(days=day)
            model.end_date = start_date + timedelta(days=day + 1)
            model.run(callbacks=[XarrayExporter(output_dir=tmp_work_dir1)])

            model2.start_date = start_date + timedelta(days=day)
            model2.end_date = start_date + timedelta(days=day + 1)
            model2.run(callbacks=[XarrayExporter(output_dir=tmp_work_dir2)])

        model_file = os.path.join(tmp_work_dir1, file_name)
        model_ds = xr.open_dataset(model_file)
        xr.testing.assert_allclose(model_ds, reference_ds, rtol=1e-06, atol=0)

        model_file = os.path.join(tmp_work_dir2, file_name)
        model_ds = xr.open_dataset(model_file)
        xr.testing.assert_allclose(model_ds, reference_ds, rtol=1e-06, atol=0)


def test_ens_speedy():
    """Test the SpeedyEns class."""
    num_of_members = 3
    start_date = datetime(1982, 1, 1)
    end_date = datetime(1982, 1, 2)
    file_name = end_date.strftime("%Y-%m-%d_%H%M.nc")
    reference_file = os.path.join(os.path.dirname(__file__), "fixtures", file_name)
    reference_ds = xr.open_dataset(reference_file)
    model_ens = SpeedyEns(num_of_members, start_date=start_date, end_date=end_date)
    for member in model_ens:
        member.set_bc()
    with tempfile.TemporaryDirectory() as tmp_work_dir:
        model_ens.run(callbacks=[XarrayExporter(output_dir=tmp_work_dir)])

        model_ens_ds = xr.open_dataset(os.path.join(tmp_work_dir, file_name))
        for m, member in enumerate(model_ens):
            xr.testing.assert_allclose(
                member.to_dataframe().squeeze(dim="ens", drop=True),
                reference_ds,
                rtol=1e-06,
                atol=0,
            )
            member_ds = model_ens_ds.sel(ens=m).drop_vars("ens")
            xr.testing.assert_allclose(member_ds, reference_ds, rtol=1e-06, atol=0)


def test_exceptions():
    """Test that certain exceptions are raised."""
    model = Speedy(start_date=datetime(1982, 1, 1), end_date=datetime(1982, 1, 2))
    model.set_bc()
    model.run()

    # Force a failure in the diagnostic check
    t = model["t"]
    t[:] = 0
    model["t"] = t
    with pytest.raises(RuntimeError):
        model.check()


@pytest.mark.parametrize("variables", export_variables)
def test_speedy_variable_export(variables):
    """Run speedy and compare the output with a reference."""

    start_date = datetime(1982, 1, 1)
    end_date = datetime(1982, 1, 2)

    file_name = end_date.strftime("%Y-%m-%d_%H%M.nc")

    with tempfile.TemporaryDirectory() as tmp_work_dir:
        model = Speedy(start_date=start_date, end_date=end_date)
        model.set_bc()

        exporter = XarrayExporter(output_dir=tmp_work_dir, variables=variables)
        model.run(callbacks=[exporter])

        model_file = os.path.join(tmp_work_dir, file_name)
        model_ds = xr.open_dataset(model_file)

        assert set((v.replace("_grid", "") for v in variables)) == set(model_ds.keys())
