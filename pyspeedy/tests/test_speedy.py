import os
import tempfile
import xarray as xr
from pyspeedy.speedy import Speedy


def test_speedy_run():
    """Run speeedy for 1 month and compare the output with a reference."""
    reference_file = os.path.join(os.path.dirname(__file__), "fixtures/198201020000.nc")
    reference_ds = xr.open_dataset(reference_file)
    with tempfile.TemporaryDirectory() as tmp_work_dir:
        os.chdir(tmp_work_dir)
        model = Speedy(output_dir=tmp_work_dir)
        model.set_sst_anomalies()
        model.default_init()
        model.run()

        model_file = "./198201020000.nc"
        model_ds = xr.open_dataset(model_file)

        xr.testing.assert_allclose(
            model_ds, reference_ds.transpose(), rtol=1e-06, atol=0
        )
