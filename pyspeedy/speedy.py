from _speedy import pyspeedy


class Speedy:
    """
    Speedy model.
    """

    def __init__(self):
        """
        Constructor. Initializes the model.
        """
        self._state = pyspeedy.initialize()

    def run(self):
        """
        Run the model.
        """
        pyspeedy.run(self._state)

    def __getattr__(self, var_name):
        _getter = getattr(pyspeedy, f"get_{var_name}", None)
        if _getter is None:
            raise AttributeError(f"The state variable '{var_name}' does not exist.")
        return _getter(self._state)


if __name__ == "__main__":

    model = Speedy()
    # model.run()
    print(model.vor.shape)
    print(model.hfluxn.shape)
    print(model.hfluxnsss.shape)

    # from matplotlib import pyplot as plt
    # plt.pcolormesh(t[:,:,4])
    # plt.colorbar()
    # plt.show()
