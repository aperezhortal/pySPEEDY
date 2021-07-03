from collections import namedtuple
from jinja2 import FileSystemLoader, Environment
from pathlib import Path
import json

THIS_FILE_DIR = Path(__file__).parent
SOURCES_DIR = (THIS_FILE_DIR / "../source").resolve()
PYSPEEDY_DATA_DIR = (THIS_FILE_DIR / "../pyspeedy/data").resolve()

class VarDef:
    def __init__(self, name, dtype, dims, desc, time_dim=None):
        """
        If has_time_dim is True, the variable is not allocated during the
        initialization since it depends on the duration of the simulation.
        """
        self.name = name
        self.dtype = dtype
        self.dims = dims
        self.desc = desc
        self.time_dim = time_dim

    @property
    def dimension(self):
        dimension = ", ".join(":" * self.ndim)
        return f"dimension({dimension})"

    @property
    def dimension_args(self):
        return self.dims.replace("(", "").replace(")", "")

    @property
    def dimension_args_declaration(self):
        return f"integer, intent(in) :: {self.dimension_args}"

    @property
    def ndim(self):
        return len(self.dims.split(","))


model_state = [
    ########################################
    # Prognostic variables (spectral domain)
    ########################################
    VarDef("vor", "complex", "(mx, nx, kx, t_levs)", "Vorticity"),
    VarDef("div", "complex", "(mx, nx, kx, t_levs)", "Divergence"),
    VarDef("t", "complex", "(mx, nx, kx, t_levs)", "Temperature [K]"),
    VarDef(
        "ps",
        "complex",
        "(mx, nx, t_levs)",
        "Log of (normalised) surface pressure (p_s/p0)",
    ),
    VarDef(
        "tr",
        "complex",
        "(mx, nx, kx, t_levs,ntr)",
        "Tracers (tr(1): specific humidity in g/kg)",
    ),
    VarDef("phi", "complex", "(mx, nx, kx)", "Atmospheric geopotential"),
    VarDef("phis", "complex", "(mx, nx)", "Surface geopotential"),
    ################################################
    # Auxiliary variables used by the physic schemes
    ################################################
    VarDef(
        "precnv", "real", "(ix, il)", "Convective precipitation  [g/(m^2 s)], total"
    ),
    VarDef(
        "precls", "real", "(ix, il)", "Large-scale precipitation [g/(m^2 s)], total"
    ),
    VarDef(
        "snowcv", "real", "(ix, il)", "Convective precipitation  [g/(m^2 s)], snow only"
    ),
    VarDef(
        "snowls", "real", "(ix, il)", "Large-scale precipitation [g/(m^2 s)], snow only"
    ),
    VarDef("cbmf", "real", "(ix, il)", "Cloud-base mass flux"),
    VarDef(
        "tsr", "real", "(ix, il)", "Top-of-atmosphere shortwave radiation (downward)"
    ),
    VarDef("ssrd", "real", "(ix, il)", "Surface shortwave radiation (downward-only)"),
    VarDef("ssr", "real", "(ix, il)", "Surface shortwave radiation (net downward)"),
    VarDef("slrd", "real", "(ix, il)", "Surface longwave radiation (downward-only)"),
    VarDef("slr", "real", "(ix, il)", " Surface longwave radiation (net upward)"),
    VarDef("olr", "real", "(ix, il)", "Outgoing longwave radiation (upward)"),
    # Third dimension -> 1:land, 2:sea, 3: weighted average
    VarDef("slru", "real", "(ix, il,aux_dim)", "Surface longwave emission (upward)"),
    VarDef("ustr", "real", "(ix, il,aux_dim)", "U-stress"),
    VarDef("vstr", "real", "(ix, il,aux_dim)", "Vstress"),
    VarDef("shf", "real", "(ix, il,aux_dim)", "Sensible heat flux"),
    VarDef("evap", "real", "(ix, il,aux_dim)", "Evaporation [g/(m^2 s)]"),
    VarDef("hfluxn", "real", "(ix, il,aux_dim)", "Net heat flux into surface"),
    ###########################
    # Boundary module variables
    ###########################
    VarDef("fmask_orig", "real", "(ix, il)", "Original (fractional) land-sea mask"),
    VarDef("phi0", "real", "(ix, il)", "Unfiltered surface geopotential"),
    VarDef("orog", "real", "(ix, il)", "Orography [m]"),
    VarDef("phis0", "real", "(ix, il)", "Spectrally-filtered surface geopotential"),
    VarDef("alb0", "real", "(ix, il)", "Bare-land annual-mean albedo"),
    ###############################
    # Geopotential module variables
    ###############################
    VarDef("xgeop1", "real", "(kx)", "Constant 1 for hydrostatic equation"),
    VarDef("xgeop2", "real", "(kx)", "Constant 2 for hydrostatic equation"),
    ##########################
    # Bounday module variables
    ##########################
    VarDef(
        "stl12",
        "real",
        "(ix, il, 12)",
        "Land surface temperature monthly-mean climatology",
    ),
    VarDef(
        "snowd12",
        "real",
        "(ix, il, 12)",
        "Snow depth (water equivalent) monthly-mean climatology",
    ),
    VarDef(
        "soilw12",
        "real",
        "(ix, il, 12)",
        "Soil water availability monthly-mean climatology",
    ),
    VarDef("veg_low", "real", "(ix, il)", "Low vegetation fraction"),
    VarDef("veg_high", "real", "(ix, il)", "High vegetation fraction"),
    VarDef("soil_wc_l1", "real", "(ix, il, 12)", "Soil water content: Layer 1"),
    VarDef("soil_wc_l2", "real", "(ix, il, 12)", "Soil water content: Layer 2"),
    VarDef("soil_wc_l3", "real", "(ix, il, 12)", "Soil water content: Layer 3"),
    ##########################
    # Bounday module variables
    ##########################
    VarDef("sst12", "real", "(ix, il, 12)", "Sea/ice surface temperature [K]"),
    VarDef("sea_ice_frac12", "real", "(ix, il, 12)", "Sea ice fraction"),
    ############################
    # Sea model module variables
    ############################
    VarDef(
        "sst_anom",
        "real",
        "(ix, il, 0:n_months+1)",
        "Observed SST anomaly (input).",
        time_dim="n_months",
    ),
]

file_loader = FileSystemLoader(THIS_FILE_DIR / "templates")
env = Environment(loader=file_loader, trim_blocks=True, lstrip_blocks=True)
template = env.get_template("model_state.f90.j2")
output = template.stream(model_state=model_state).dump(
    str(SOURCES_DIR / "model_state.f90")
)

template = env.get_template("speedy_driver.f90.j2")
output = template.stream(model_state=model_state).dump(
    str(SOURCES_DIR / "speedy_driver.f90")
)


model_state = {
    var.name: dict(dtype=var.dtype, dims=var.dims, desc=var.desc, time_dim=var.time_dim)
    for var in model_state
}

with open(PYSPEEDY_DATA_DIR/"model_state.json", 'w') as outfile:
    json.dump(model_state, outfile, indent=4)

