"""
Callbacks module
================

A callback is an object can perform a particular task at the end of the model time step.
The following callbacks are available in pySPEEDY:

.. autosummary::
    :toctree: ../generated/

    BaseCallback
    DiagnosticCheck
    ModelCheckpoint
    XarrayExporter
"""
import copy

from datetime import datetime

import os

import xarray as xr
from pyspeedy import (
    _speedy,  # noqa
    DEFAULT_OUTPUT_VARS,
)
from pyspeedy.speedy import SpeedyEns, Speedy


class BaseCallback:
    """
    Base callback class.
    """

    def __init__(self, *args, **kwargs):
        """
        Constructor.

        Parameters
        ----------
        interval: int
            Interval, in time steps, for which the callback should be applied.
        verbose: bool
            If true, print debug and progress messages.
        spinup_date: datetime or None:
            End date of the spinup period. During the spinup the callback is ignored.
        """
        self.verbose = kwargs.pop("verbose", False)
        self.interval = kwargs.pop("interval", 1)
        self.spinup_date = kwargs.pop("spinup_date", None)

    def skip_flag(self, model_instance):
        """
        Return True when the callback execution is skipped for this time step if
        - model_date < spinup_date
        - current_step % interval !=0
        """
        if self.spinup_date is not None:
            if model_instance.current_date < self.spinup_date:
                # Do not save during spinup time
                return True

        return model_instance.get_current_step() % self.interval != 0

    def print_msg(self, msg):
        """Print debug message if `verbose` was set to True."""
        if self.verbose:
            print(msg)

    def copy(self):
        """Create a copy of the instance."""
        return copy.deepcopy(self)

    def __call__(self, model_instance):
        """Object call."""
        pass


class DiagnosticCheck(BaseCallback):
    """
    Callback used to check on that the prognostic variables are inside reasonable ranges.
    """

    def __init__(self, interval=36):
        """
        Constructor.

        Parameters
        ----------
        interval: int
            Interval, in time steps, for which the diagnostic checkes are run.
        """
        super().__init__(interval=interval)

    def __call__(self, model_instance):
        """
        Object call.

        Run diagnostic check if needed.
        """
        if self.skip_flag(model_instance):
            # Only run tests every `interval` steps.
            return

        if isinstance(model_instance, Speedy):
            # If it is a single run, we convert it to a list to make iterable.
            model_instance = [model_instance]

        for _member in model_instance:
            # If the test fails, it will raise a runtime exception.
            _member.check()


class ModelCheckpoint(BaseCallback):
    """
    Callback used save selected variables of the model state at the current time step.
    Note that this is not the full model state, only a small subset
    of it. We will refer to this saved state as "Checkpoint".

    Each instance of this callback will keep internally a time series of the checkpoints saved during the the model run
    in an xarray DataFrame following CF conventions.

    Notes
    -----
    The variables are saved in the lat/lon grid space (not the spectral domain).
    Spectral variables are not supported by this callback.
    """

    def __init__(
        self,
        interval=36,
        verbose=False,
        spinup_date=None,
        variables=None,
        output_dir="./",
    ):
        """
        Parameters
        ----------
        interval: int
            Interval, in time steps, for which the model variables are saved.
        verbose: bool
            If true, print debug and progress messages.
        spinup_date: datetime or None:
            End date of the spinup period. During the spinup the output files are not saved.
        variables: list, tuple
            List of variables to save
        output_dir: str
            Path to folder where the output files are stored.
        interval: int
            History interval in timesteps every which the output files are saved.
        spinup_date: datetime or None
            Model spinup date. From `start_date` to `spinup_date` the callbacks functions are not called.
        """
        if variables is None:
            variables = DEFAULT_OUTPUT_VARS
        self.variables = variables
        self.output_dir = output_dir
        self.history_interval = interval
        super().__init__(verbose=verbose, interval=interval, spinup_date=spinup_date)
        self.dataframe = None

    def __call__(self, model_instance):
        """
        Object call.

        Export the model data to file using xarray.
        """
        if self.skip_flag(model_instance):
            # Only save files at every history_interval steps.
            return

        model_df = model_instance.to_dataframe(variables=self.variables)
        if self.dataframe is None:
            self.dataframe = model_df
        else:
            self.dataframe = xr.merge((self.dataframe, model_df))


class XarrayExporter(BaseCallback):
    """
    Callback used to create an xarray dataset with selected variables a the current model time step.

    The variables are saved in the lat/lon grid space (not the spectral domain).
    Spectral variables are not supported by this callback.

    For ensemble runs, each output file is saved in a different subdirectory (named "member###").
    This ensure that concurrent calls to the callback from different ensemble members do not interfere with each other.
    """

    def __init__(
        self,
        interval=36,
        verbose=False,
        spinup_date=None,
        variables=None,
        output_dir="./",
        filename_fmt="%Y-%m-%d_%H%M.nc",
    ):
        """
        Parameters
        ----------
        interval: int
            Interval, in time steps, for which the model variables are saved.
        verbose: bool
            If true, print debug and progress messages.
        spinup_date: datetime or None:
            End date of the spinup period. During the spinup the model checkpoints are not saved.
        variables: list, tuple
            List of variables to save
        output_dir: str
            Path to folder where the output files are stored.
        filename_fmt: str
            Format string used to generate the output filename. The format string is passed to the `strftime` method
            for the current model datetime.
            For ensemble runs, each member output is saved in a different subfolder (named "member###").
        interval: int
            History interval in timesteps every which the output files are saved.
        spinup_date: datetime or None
            Model spinup date. From `start_date` to `spinup_date` the callbacks functions are not called.
        """
        if variables is None:
            variables = DEFAULT_OUTPUT_VARS
        self.variables = variables
        self.output_dir = output_dir
        self.filename_fmt = filename_fmt
        self.history_interval = interval
        super().__init__(verbose=verbose, interval=interval, spinup_date=spinup_date)

    def __call__(self, model_instance):
        """
        Object call.

        Export the model data to file using xarray.
        """
        if self.skip_flag(model_instance):
            # Only save files at every history_interval steps.
            return

        model_df = model_instance.to_dataframe(variables=self.variables)

        vars_and_coords = list(model_df.variables.keys()) + list(model_df.coords.keys())
        encoding = dict()
        for var in vars_and_coords:
            if var in ("time", "ens"):
                continue

        file_name = model_instance.current_date.strftime(self.filename_fmt)
        os.makedirs(self.output_dir, exist_ok=True)
        output_file_path = os.path.join(self.output_dir, file_name)
        self.print_msg(f"Saving model output at: {output_file_path}.")
        model_df.to_netcdf(output_file_path, encoding=encoding)
