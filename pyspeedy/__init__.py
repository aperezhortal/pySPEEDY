from _speedy import pyspeedy


class __ModuleInitializer:
    def __init__(self):
        pyspeedy.initialize()

    def __del__(self):
        pyspeedy.close()


# When this object is deleted, it will deinitialize the module.
__module_init = __ModuleInitializer()
