"""
The Speedy model
================

.. autosummary::
    :toctree: ./generated/

    Speedy
    SpeedyEns
"""

import os

import xarray as xr
import numpy as np
from datetime import datetime, timedelta
from dateutil.relativedelta import relativedelta
import json

from pyspeedy import (
    _speedy,  # noqa
    example_bc_file,
    example_sst_anomaly_file,
    PACKAGE_DATA_DIR,
    DEFAULT_OUTPUT_VARS,
)
from pyspeedy.error_codes import ERROR_CODES

_DEFAULT_PARAMS = dict(
    start_date=datetime(1982, 1, 1),
    end_date=datetime(1982, 1, 2),
)

# Make this dict immutable.
_DEFAULT_PARAMS = tuple(_DEFAULT_PARAMS.items())
with open(PACKAGE_DATA_DIR / "model_state.json") as fp:
    MODEL_STATE_DEF = json.load(fp)


class Speedy:
    """
    Speedy model.
    """

    def __init__(
        self,
        start_date=datetime(1982, 1, 1),
        end_date=datetime(1982, 1, 2),
        member=None,
    ):
        """
        Constructor. Initializes the model.

        Parameters
        ----------
        start_date: datetime
            Model start date.
        end_date: datetime
            Model end date.
        member: int
            Member number. If None, the run is not considered part of an ensemble run.
            This value is used by the callback functions to determine how to treat the simulation
            (a single isolated run, or an ensemble member).
        """
        self._start_date = None
        self._end_date = None
        self.member_id = member  # member number
        self.is_ensemble_member = self.member_id is not None

        # Allocate the model state
        self._state_cnt = _speedy.modelstate_init()
        self.set_params(start_date=start_date, end_date=end_date)

        self._initialized_bc = False
        self._initialized_ssta = False
        self.current_date = self.start_date

    def __del__(self):
        """Clean up."""
        _speedy.modelstate_close(self._state_cnt)
        _speedy.controlparams_close(self._control_cnt)

        self._dealloc_date(self._start_date)
        self._dealloc_date(self._end_date)

    def set_params(
        self,
        start_date=datetime(1982, 1, 1),
        end_date=datetime(1982, 1, 2),
    ):
        """
        Set the model's control parameters.

        Parameters
        ----------
        start_date: datetime
            Model start date.
        end_date: datetime
            Model end date.
        """
        self.start_date = start_date
        self.end_date = end_date

        if self.start_date > self.end_date:
            raise ValueError("The start date should be lower than the en date.")

        self._control_cnt = _speedy.controlparams_init(self._start_date, self._end_date)

        self._model_date = None

        self.current_date = start_date

        self.n_months = (
            (self.end_date.year - self.start_date.year) * 12
            + (self.end_date.month - self.start_date.month)
            + 1
        )

    @staticmethod
    def _dealloc_date(container):
        """Deallocate a datetime fortran object."""
        if container is not None:
            _speedy.close_datetime(container)

    def __getitem__(self, var_name):
        """Getter for state variables."""
        _getter = getattr(_speedy, f"get_{var_name}", None)
        if _getter is None:
            raise AttributeError(f"The state variable '{var_name}' does not exist.")

        time_dim = MODEL_STATE_DEF[var_name]["time_dim"]
        if time_dim:
            return _getter(self._state_cnt, getattr(self, time_dim))

        return _getter(self._state_cnt)

    def get_shape(self, var_name):
        """Get the shape of an state variable."""
        _getter = getattr(_speedy, f"get_{var_name}_shape", None)
        if _getter is None:
            raise AttributeError(
                f"The 'get-shape' method for the state variable "
                f"{var_name}' does not exist."
            )
        return tuple(_getter(self._state_cnt))

    def __setitem__(self, var_name, value):
        """Setter for state variables."""
        _setter = getattr(_speedy, f"set_{var_name}")
        if _setter is None:
            raise AttributeError(
                f"The setter for the state variable '{var_name}' does not exist."
            )

        is_array_func = getattr(_speedy, f"is_array_{var_name}")
        if is_array_func():
            if self.get_shape(var_name) != value.shape:
                raise ValueError("Array shape missmatch")
            value = np.asfortranarray(value)

            time_dim = MODEL_STATE_DEF[var_name]["time_dim"]
            if time_dim:
                return _setter(self._state_cnt, value, getattr(self, time_dim))

            return _setter(self._state_cnt, value)

        return _setter(self._state_cnt, value)

    @staticmethod
    def _get_fortran_date(container):
        """Get a datetime object from a fortran datetime"""
        return datetime(*_speedy.get_datetime(container))

    @staticmethod
    def _set_fortran_date(container, date_value):
        """Create a python datetime object from a fortran datetime"""
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

    def get_current_step(self):
        """Return the current step in the simulation."""
        return self["current_step"]

    @property
    def start_date(self):
        return self._get_fortran_date(self._start_date)

    @start_date.setter
    def start_date(self, value):
        self._start_date = self._set_fortran_date(self._start_date, value)

    @property
    def current_date(self):
        return self._get_fortran_date(self._model_date)

    @current_date.setter
    def current_date(self, value):
        self._model_date = self._set_fortran_date(self._model_date, value)

    @property
    def end_date(self):
        return self._get_fortran_date(self._end_date)

    @end_date.setter
    def end_date(self, value):
        self._end_date = self._set_fortran_date(self._end_date, value)

    def set_bc(self, bc_file=None, sst_anomaly=None):
        """
        Set the model boundary conditions from a Netcdf file.
        If no file is provided, the default boundary conditions from the original SPEEDY model are used.

        The boundary conditions file (`bc_file`) should contain the following fields:

        - Time invariant (lon, lat):
            - orog: Orographic height [m]
            - lsm: Land sea mask fraction. Values between 0 and 1.
            - vegl: Low vegetation cover (fraction). Values between 0 and 1.
            - vegh: High vegetation cover (fraction). Values between 0 and 1.
            - alb: Annual-mean albedo (fraction). Values between 0 and 1.
        - Climatological values for each month of the year (lon, lat, month):
            - stl: Land surface temp (top-layer) [degK].
            - snowd: Snow depth [kg/m2]
            - swl1: Soil wetness (layer 1) [vol. fraction, 0-1]
            - swl2: Soil wetness (layer 2) [vol. fraction, 0-1]
            - swl3: Soil wetness (layer 3) [vol. fraction, 0-1]
            - icec: Sea-ice concentration (fraction). Values between 0 and 1.
            - sst: Sea surface temperature [degK].

        In addition to the climatological fields, the SPEEDY model requires the
        sea surface temperature anomalies with respect to the the climatologies (lon, lat, day):
        These anomalies are loaded from the file specified in the `sst_anomaly` keyword, and it should contain the
        following field:

        - Anomalies fields (lon, lat, day):
            - ssta: Sea surface temperature anomaly [degK].

        By default, the anomalies available in the original SPEEDY model are used.
        Note that the anomaly fields should cover the simulation period.

        Notes
        -----
        The exact shapes for the invariant fields are:
        (lon, lat, month) = (96, 48, 12)

        The SPEEDY boundary conditions are included in the pySPEEDY package.
        This climatology was derived from the ERA interim re-analysis using the 1979-2008 period.
        Also, the example data provided in the pySPEEDY package include the monthly SST anomalies
        from 1979-01-01 to 2013-12-01 (Y/m/d).
        """

        if self._initialized_bc:
            raise RuntimeError(
                "The model was already initialized. Create a new instance if you need different boundary conditions."
            )

        self._set_sst_anomalies(sst_anomaly=sst_anomaly)

        # In the model state, the variables follow the lon/lat dimension ordering.
        if bc_file is None:
            bc_file = example_bc_file()

        if not os.path.isfile(bc_file):
            raise RuntimeError(
                "The boundary conditions file does not exist.\n" f"File: {bc_file}"
            )

        ds = xr.load_dataset(bc_file, engine="netcdf4")

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

        _speedy.init(self._state_cnt, self._control_cnt)

        self.spectral2grid()  # Convert some spectral variables to the grid space.
        self._initialized_bc = True

    def _set_sst_anomalies(self, sst_anomaly=None):
        """
        Load SST anomalies from netcdf file.

        **Important**: The SST anomalies need to be set before setting the ICs!

        Only the times between the simulation's start and end date are loaded.

        See the :meth:`set_bc` documentation for additional details on the expected fields.
        """

        if self._initialized_ssta:
            raise RuntimeError(
                "The SST anomaly was already initialized."
                " Create a new instance if you need different boundary conditions."
            )
        if sst_anomaly is None:
            sst_anomaly = example_sst_anomaly_file()

        if isinstance(sst_anomaly, str):
            if not os.path.isfile(sst_anomaly):
                raise RuntimeError(
                    "The SST anomaly file does not exist.\n" f"File: {sst_anomaly}"
                )
            ds = xr.load_dataset(sst_anomaly)
        elif isinstance(sst_anomaly, xr.Dataset):
            ds = sst_anomaly
        else:
            raise TypeError(f"Unsupported sst_anomaly input: {type(sst_anomaly)}")

        #########################################################################
        # Select only the times in the dataset between the start and the end date

        # At each timestep, Speedy uses a 3-month window for the computations.
        # Correct the start/end dates to account for those dates.
        start_date = self.start_date.replace(
            day=1, hour=0, minute=0, microsecond=0
        ) - relativedelta(months=1)

        end_date = self.end_date.replace(
            day=1, hour=0, minute=0, microsecond=0
        ) + relativedelta(months=1, days=1)

        ds = ds.loc[
            dict(
                lon=slice(None),
                lat=slice(None),
                time=slice(start_date, end_date),
            )
        ]

        expected_months = (
            (end_date.year - start_date.year) * 12
            + (end_date.month - start_date.month)
            + 1
        )

        missing_months = expected_months - len(ds["time"])

        if missing_months > 0:
            raise RuntimeError(
                f"{missing_months} months are missing in the SST anomalies file for the period: "
                + start_date.strftime("%Y/%m/%d")
                + " , "
                + end_date.strftime("%Y/%m/%d")
                + ".\n "
            )

        _speedy.modelstate_init_sst_anom(self._state_cnt, expected_months - 2)
        self["sst_anom"] = ds["ssta"]
        self._initialized_ssta = True

    def run(self, callbacks=None):
        """
        Run the model between the start and the end date (defined in the instance `start_date` and
        `end_date` attributes).

        Before running the model, the boundary conditions need to be set (see the :meth:`set_bc` method).

        Parameters
        ----------
        callbacks: iterable of callback functions
            Sequence of callback functions to be call every `interval` time steps. The interval is specified in each
            callback instance.
        """
        if callbacks is None:
            callbacks = list()
        if not self._initialized_bc:
            raise RuntimeError(
                "The SPEEDY model was not initialized. Call the `set_bc` method to initialize the model."
            )

        self.current_date = self.start_date
        dt_step = timedelta(seconds=3600 * 24 / 36)

        while self.current_date < self.end_date:
            error_code = _speedy.step(self._state_cnt, self._control_cnt)
            if error_code < 0:
                raise RuntimeError(ERROR_CODES[error_code])
            self.current_date += dt_step

            for callback in callbacks:
                callback(self)

    def grid2spectral(self):
        """Transform the grid u, v, t, qv, ps, and phi fields to the spectral domain."""
        _speedy.transform_grid2spectral(self._state_cnt)  # noqa

    def spectral2grid(self):
        """Transform the spectral u, v, t, qv, ps, and phi fields to the grid domain."""
        _speedy.transform_spectral2grid(self._state_cnt)  # noqa

    def to_dataframe(self, variables=None):
        """
        Return an xarray DataFrame with the current model state.
        """
        if variables is None:
            variables = DEFAULT_OUTPUT_VARS

        self.spectral2grid()
        data_vars = dict()

        for var in variables:
            # Append time dimension
            dims = MODEL_STATE_DEF[var]["nc_dims"] + ["time"]
            var_data = self[var][..., None].astype("float32")
            if self.is_ensemble_member:
                # Append ensemble dimension if needed.
                dims = dims + ["ens"]
                var_data = var_data[..., None]

            data_vars[MODEL_STATE_DEF[var]["alt_name"]] = (dims, var_data)

        coords = dict(
            lon=self["lon"],
            lat=self["lat"],
            lev=self["lev"],
            time=[self.current_date],
        )
        if self.is_ensemble_member:
            coords["ens"] = [self.member_id]

        output_ds = xr.Dataset(
            data_vars=data_vars,
            coords=coords,
        )

        encoding = dict()
        for var in list(variables) + list(coords.keys()):
            if var in ("time", "ens"):
                encoding[var] = {"dtype": "int32"}
                continue
            alt_name = MODEL_STATE_DEF[var]["alt_name"]
            if MODEL_STATE_DEF[var]["units"] is not None:
                output_ds[alt_name].attrs["units"] = MODEL_STATE_DEF[var]["units"]
            output_ds[alt_name].attrs["long_name"] = MODEL_STATE_DEF[var]["desc"]
            output_ds[alt_name].attrs["standard_name"] = MODEL_STATE_DEF[var][
                "std_name"
            ]
            encoding[alt_name] = {"dtype": "float32", "zlib": True}

        output_ds["lat"].attrs["axis"] = "Y"
        output_ds["lon"].attrs["axis"] = "X"
        output_ds["time"].attrs["axis"] = "T"
        output_ds["time"].attrs["standard_name"] = "time"

        # Reorder the dimensions to follow NWP output standards.
        # - dimensions: ("time", ["ensemble"], "lev", "lat", "lon").
        # - vertical levels increasing with height.
        if self.is_ensemble_member:
            sorted_dims = ("time", "ens", "lev", "lat", "lon")
        else:
            sorted_dims = ("time", "lev", "lat", "lon")
        output_ds = output_ds.reindex(lev=output_ds.lev[::-1]).transpose(*sorted_dims)
        return output_ds

    def check(self):
        """Run a diagnostic check for the model state."""
        error_code = _speedy.check(self._state_cnt)
        if error_code < 0:
            raise RuntimeError(ERROR_CODES[error_code])


class SpeedyEns:
    """
    Ensemble of Speedy model instances.
    """

    def __init__(
        self,
        num_of_members,
        start_date=datetime(1982, 1, 1),
        end_date=datetime(1982, 1, 2),
    ):
        """
        Constructor. Initializes the ensemble of Speedy instances.

        Parameters
        ----------
        num_of_members: int
            Ensemble size.
        start_date: datetime
            Model start date.
        end_date: datetime
            Model end date.
        """
        self.n_members = num_of_members
        self.members = [
            Speedy(start_date=start_date, end_date=end_date, member=mem_num)
            for mem_num in range(num_of_members)
        ]
        self.current_date = self.members[
            0
        ].current_date  # Current date for the ensemble run.

    def __iter__(self):
        """Iterate over the ensemble members"""
        return iter(self.members)

    def __len__(self):
        return self.n_members

    def set_params(
        self, start_date=datetime(1982, 1, 1), end_date=datetime(1982, 1, 2)
    ):
        """
        Set the model's control parameters.

        See :meth:`~Speedy.set_params` for additional details.
        """
        for member in self:
            member.set_params(start_date=start_date, end_date=end_date)

        self.current_date = start_date

    def to_dataframe(self, variables=None):
        """
        Return an xarray DataFrame with the current model state.
        """
        member_dfs = []
        for member in self:
            member_dfs.append(member.to_dataframe(variables=variables))
        return xr.merge(member_dfs)

    def run(self, callbacks=None):
        """
        Run each ensemble member between the start and the end dates

        Before running the model, the boundary conditions need to be set for each members
        (see the :meth:`set_bc` method).

        To initialize all the member with the default parameters, run:

        >>> speedy_ensemble = SpeedyEns()
        >>> for member in speedy_ensemble:
        >>>     member.set_bc()

        Parameters
        ----------
        callbacks: iterable of callback functions
            Sequence of callback functions to be call every `interval` time steps. The interval is specified in each
            callback instance.
        """
        if callbacks is None:
            callbacks = []

        end_date = self.members[0].end_date
        dt_step = timedelta(seconds=3600 * 24 / 36)

        state_cnts = np.zeros(self.n_members, dtype=np.int64)
        control_cnts = np.zeros(self.n_members, dtype=np.int64)
        for m, member in enumerate(self):
            state_cnts[m] = member._state_cnt  # noqa
            control_cnts[m] = member._control_cnt  # noqa

        while self.current_date < end_date:
            error_codes = _speedy.parallel_step(state_cnts, control_cnts)
            self.current_date += dt_step

            if (error_codes < 0).any():
                msg = ""
                for n, code in enumerate(error_codes):
                    msg += f"Member{n}: {ERROR_CODES[code]}\n"
                raise RuntimeError(msg)

            # Update current date in all members
            for member in self:
                member.current_date = self.current_date

            for callback in callbacks:
                callback(self)

    def get_current_step(self):
        """Return the current step in the simulation."""
        return self.members[0]["current_step"]
