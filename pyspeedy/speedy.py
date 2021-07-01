from _speedy import pyspeedy
import xarray as xr
import numpy as np
from datetime import datetime

_DEFAULT_PARAMS = dict(
    history_interval=1,
    diag_interval=180,
    start_date=datetime(1982, 1, 1),
    end_date=datetime(1982, 1, 2),
)

# Make this constant immutable.
_DEFAULT_PARAMS = tuple(_DEFAULT_PARAMS.items())


class Speedy:
    """
    Speedy model.
    """

    def __init__(self, **control_params):
        """
        Constructor. Initializes the model.
        """
        self._start_date = None
        self._end_date = None


        _control_params = dict(_DEFAULT_PARAMS)
        _control_params.update(control_params)

        for key, value in _control_params.items():
            setattr(self, key, value)

        self._state = pyspeedy.modelstate_init()
        self._control = pyspeedy.controlparams_init(
            self._start_date,
            self._end_date,
            self.history_interval,
            self.diag_interval,
        )

    @staticmethod
    def _dealloc_date(container):
        if container is not None:
            pyspeedy.close_datetime(container)

        return None

    def __getitem__(self, var_name):
        """
        Getter for state variables
        """
        _getter = getattr(pyspeedy, f"get_{var_name}", None)
        if _getter is None:
            raise AttributeError(f"The state variable '{var_name}' does not exist.")
        return _getter(self._state)

    def get_shape(self, var_name):
        """Get state variable shape."""
        _getter = getattr(pyspeedy, f"get_{var_name}_shape", None)
        if _getter is None:
            raise AttributeError(
                f"The 'get-shape' method for the state variable '"
                f"{var_name}' does not exist."
            )
        return tuple(_getter(self._state))

    def __setitem__(self, var_name, value):
        """Setter for state variables."""
        _setter = getattr(pyspeedy, f"set_{var_name}")
        if _setter is None:
            raise AttributeError(
                f"The setter for the state variable '{var_name}' does not exist."
            )
        if self.get_shape(var_name) != value.shape:
            raise ValueError("Array shape missmatch")
        value = np.asfortranarray(value)
        return _setter(self._state, value)

    def __del__(self):
        """Clean up."""
        pyspeedy.modelstate_close(self._state)
        pyspeedy.controlparams_close(self._control)

        self._control = None
        self._state = None

        self._start_date = self._dealloc_date(self._start_date)
        self._end_date = self._dealloc_date(self._end_date)

    @staticmethod
    def _get_fortran_date(container):
        """Get a datetime object from a fortran datetime"""
        return datetime(*pyspeedy.get_datetime(container))

    @staticmethod
    def _set_fortran_date(container, date_value):
        """Create a datetime object from a fortran datetime"""

        Speedy._dealloc_date(container)

        if isinstance(date_value, datetime):
            return pyspeedy.create_datetime(
                date_value.year,
                date_value.month,
                date_value.day,
                date_value.hour,
                date_value.minute,
            )
        else:
            raise TypeError("The input value is not a datetime object.")

    @property
    def start_date(self):
        return self._get_fortran_date(self._start_date)

    @start_date.setter
    def start_date(self, value):
        self._start_date = self._set_fortran_date(self._start_date, value)

    @property
    def end_date(self):
        return self._get_fortran_date(self._end_date)

    @end_date.setter
    def end_date(self, value):
        self._end_date = self._set_fortran_date(self._end_date, value)

    def default_init(self):
        # In the model state, the variables follow the lon/lat dimension ordering.
        ds = xr.load_dataset("surface.nc")
        # Surface geopotential (i.e. orography)
        self["orog"] = ds["orog"].values.swapaxes(0, 1)[:, ::-1]
        self["fmask_orig"] = ds["lsm"].values.swapaxes(0, 1)[:, ::-1]
        self["alb0"] = ds["alb"].values.swapaxes(0, 1)[:, ::-1]

        self["veg_high"] = ds["vegh"].transpose("lon", "lat").values[:, ::-1]
        self["veg_low"] = ds["vegl"].transpose("lon", "lat").values[:, ::-1]

        ds = xr.load_dataset("land.nc")
        self["stl12"] = ds["stl"].transpose("lon", "lat", "time").values[:, ::-1, :]

        ds = xr.load_dataset("snow.nc")
        self["snowd12"] = ds["snowd"].transpose("lon", "lat", "time").values[:, ::-1, :]

        ds = xr.load_dataset("soil.nc")
        self["soil_wc_l1"] = (
            ds["swl1"].transpose("lon", "lat", "time").values[:, ::-1, :]
        )
        self["soil_wc_l2"] = (
            ds["swl2"].transpose("lon", "lat", "time").values[:, ::-1, :]
        )
        self["soil_wc_l3"] = (
            ds["swl3"].transpose("lon", "lat", "time").values[:, ::-1, :]
        )

        ds = xr.load_dataset("sea_surface_temperature.nc")
        self["sst12"] = ds["sst"].transpose("lon", "lat", "time").values[:, ::-1, :]

        ds = xr.load_dataset("sea_ice.nc")
        self["sea_ice_frac12"] = (
            ds["icec"].transpose("lon", "lat", "time").values[:, ::-1, :]
        )

    def load_anomalies(self):
        # ds = xr.load_dataset("sea_surface_temperature_anomaly.nc")
        pass

    def run(self):
        """
        Run the model.
        """
        pyspeedy.run(self._state, self._control)


if __name__ == "__main__":

    model = Speedy()
    # model.start_date = datetime(2010, 1, 2, 4, 5)
    # print(model.start_date)
    # model.start_date = datetime(2010, 1, 2, 4, 6)
    # print(model.start_date)
    # print(model["vor"].shape)
    # print(model["phi0"].shape)
    model.default_init()
    # print(model.get_shape("phi0"))
    model.run()

    # from matplotlib import pyplot as plt
    # plt.pcolormesh(t[:,:,4])
    # plt.colorbar()
    # plt.show()
