import os
import xarray as xr
import numpy as np
from datetime import datetime

from pyspeedy import _speedy, example_bc_file

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

        For a complete list of the accepted initialization parameters, see:
        :py:meth:`set_params`.
        """
        self._start_date = None
        self._end_date = None

        self.set_params(**control_params)

    def set_params(self, **control_params):
        """
        Set the model's control parameters.

        Parameters
        ----------
        history_interval: int
            Interval, in time steps, for which the model variables are saved.
        diag_interval: int
            Interval, in time steps, for which the diagnostic variables are saved.
        start_date: datetime
            Model start date.
        end_date: datetime
            Model end date.
        """
        _control_params = dict(_DEFAULT_PARAMS)
        _control_params.update(control_params)

        for key, value in _control_params.items():
            setattr(self, key, value)

        self._state = _speedy.modelstate_init()
        self._control = _speedy.controlparams_init(
            self._start_date,
            self._end_date,
            self.history_interval,
            self.diag_interval,
        )

    @staticmethod
    def _dealloc_date(container):
        if container is not None:
            _speedy.close_datetime(container)

        return None

    def __getitem__(self, var_name):
        """
        Getter for state variables
        """
        _getter = getattr(_speedy, f"get_{var_name}", None)
        if _getter is None:
            raise AttributeError(f"The state variable '{var_name}' does not exist.")
        return _getter(self._state)

    def get_shape(self, var_name):
        """Get state variable shape."""
        _getter = getattr(_speedy, f"get_{var_name}_shape", None)
        if _getter is None:
            raise AttributeError(
                f"The 'get-shape' method for the state variable '"
                f"{var_name}' does not exist."
            )
        return tuple(_getter(self._state))

    def __setitem__(self, var_name, value):
        """Setter for state variables."""
        _setter = getattr(_speedy, f"set_{var_name}")
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
        _speedy.modelstate_close(self._state)
        _speedy.controlparams_close(self._control)

        self._control = None
        self._state = None

        self._start_date = self._dealloc_date(self._start_date)
        self._end_date = self._dealloc_date(self._end_date)

    @staticmethod
    def _get_fortran_date(container):
        """Get a datetime object from a fortran datetime"""
        return datetime(*_speedy.get_datetime(container))

    @staticmethod
    def _set_fortran_date(container, date_value):
        """Create a datetime object from a fortran datetime"""

        Speedy._dealloc_date(container)
        if isinstance(date_value, datetime):
            return _speedy.create_datetime(
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

    def default_init(self, bc_file=None):
        # In the model state, the variables follow the lon/lat dimension ordering.

        from pathlib import Path

        if bc_file is None:
            bc_file = example_bc_file()

        if not os.path.isfile(bc_file):
            raise RuntimeError(
                "The boundary conditions file does not exist.\n" f"File: {bc_file}"
            )

        ds = xr.load_dataset(example_bc_file(), engine="netcdf4")

        self["orog"] = ds["orog"].values
        self["fmask_orig"] = ds["lsm"].values
        self["alb0"] = ds["alb"].values

        self["veg_high"] = ds["vegh"].values
        self["veg_low"] = ds["vegl"].values

        self["stl12"] = ds["stl"].values
        self["snowd12"] = ds["snowd"].values

        self["soil_wc_l1"] = ds["swl1"].values

        self["soil_wc_l2"] = ds["swl2"].values
        self["soil_wc_l3"] = ds["swl3"].values

        self["sst12"] = ds["sst"].values

        self["sea_ice_frac12"] = ds["icec"].values

    def load_anomalies(self):
        # ds = xr.load_dataset("sea_surface_temperature_anomaly.nc")
        pass

    def run(self):
        """
        Run the model.
        """
        _speedy.run(self._state, self._control)


if __name__ == "__main__":

    model = Speedy()
    model.default_init()
    model.run()

    # from matplotlib import pyplot as plt
    # plt.pcolormesh(t[:,:,4])
    # plt.colorbar()
    # plt.show()
