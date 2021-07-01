from _speedy import pyspeedy
import xarray as xr
import numpy as np


class Speedy:
    """
    Speedy model.
    """

    def __init__(self):
        """
        Constructor. Initializes the model.
        """
        self._state = pyspeedy.initialize()

    def default_init(self):

        # In the model state, the variables follow the lon/lat dimension ordering.

        ds = xr.load_dataset("surface.nc")
        # Surface geopotential (i.e. orography)
        self["orog"] = ds["orog"].values.swapaxes(0, 1)[:, ::-1]
        self["fmask_orig"] = ds["lsm"].values.swapaxes(0, 1)[:, ::-1]
        self["alb0"] = ds["alb"].values.swapaxes(0, 1)[:, ::-1]
        

        # ds = xr.load_dataset("land.nc")
        # self["stl12"] = ds["stl"].values.swapaxes(0, 1)[:, :, ::-1]

        # float stl(time, lat, lon)        


    def run(self):
        """
        Run the model.
        """
        pyspeedy.run(self._state)

    def __getitem__(self, var_name):
        """
        Itemgetter
        
        """
        _getter = getattr(pyspeedy, f"get_{var_name}", None)
        if _getter is None:
            raise AttributeError(f"The state variable '{var_name}' does not exist.")
        return _getter(self._state)

    def get_shape(self, var_name):
        _getter = getattr(pyspeedy, f"get_{var_name}_shape", None)
        if _getter is None:
            raise AttributeError(f"The 'get-shape' method for the state variable '"
                                 f"{var_name}' does not exist.")
        return tuple(_getter(self._state))

    def __setitem__(self, var_name, value):
        _setter = getattr(pyspeedy, f"set_{var_name}")
        if _setter is None:
            raise AttributeError(
                f"The setter for the state variable '{var_name}' does not exist."
            )

        if self.get_shape(var_name) != value.shape:
            raise ValueError("Array shape missmatch")
        value = np.asfortranarray(value)
        return _setter(self._state, value)


if __name__ == "__main__":
    model = Speedy()
    print(model["vor"].shape)
    print(model["phi0"].shape)
    model.default_init()
    print(model.get_shape("phi0"))
    model.run()

    # from matplotlib import pyplot as plt
    # plt.pcolormesh(t[:,:,4])
    # plt.colorbar()
    # plt.show()
