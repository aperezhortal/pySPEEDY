from collections import namedtuple
from jinja2 import FileSystemLoader, Environment

_VarDef = namedtuple("VariableDefinition", ["name", "dtype", "dims", "desc"])


class VarDef(_VarDef):
    @property
    def dimension(self):
        ndims = len(self.dims.split(","))
        dimension = ", ".join(":" * ndims)
        return f"dimension({dimension})"

    @property
    def dimension_args(self):
        return self.dims.replace("(", "").replace(")", "")

    @property
    def dimension_args_declaration(self):
        return f"integer, intent(in) :: {self.dimension_args}"


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
    VarDef("phis0", "real", "(ix, il)", "Spectrally-filtered surface geopotential"),
    VarDef("alb0", "real", "(ix, il)", "Bare-land annual-mean albedo"),
    ###########################
    # Geometry module variables
    ###########################
    #
    ## Geometry: Vertical level parameters
    #     VarDef("hsg", "real", "(kx+1)", "Half sigma levels"),
    #     VarDef("dhs", "real", "(kx)", "Sigma level thicknesses"),
    #     VarDef("fsg", "real", "(kx)", "Full sigma levels"),
    #     VarDef("dhsr", "real", "(kx)", "1/(2*sigma level thicknesses)"),
    #     VarDef("fsgr", "real", "(kx)", "akap/(2*full sigma levels)"),
    #     ## Geometry: vars as a function of latitude and longitude
    #     VarDef("radang", "real", "(il)", "Latitudes in radians"),
    #     VarDef("coriol", "real", "(il)", "Coriolis parameter as a function of latitude"),
    #     VarDef("sia", "real", "(il)", "sine(latitude)"),
    #     VarDef("coa", "real", "(il)", "cosine(latitude)"),
    #     VarDef("sia_half", "real", "(iy)", "sine(latitude) over one hemisphere only"),
    #     VarDef("coa_half", "real", "(il)", "cosine(latitude) over one hemisphere only"),
    #     VarDef("cosg", "real", "(il)", "Same as coa (TODO: remove)"),
    #     VarDef("cosgr", "real", "(il)", "1/coa"),
    #     VarDef("cosgr2", "real", "(il)", "1/coa^2"),
]

file_loader = FileSystemLoader("templates")
env = Environment(loader=file_loader, trim_blocks=True, lstrip_blocks=True)
template = env.get_template("model_state.f90.j2")
output = template.stream(model_state=model_state).dump("../source/model_state.f90")

template = env.get_template("pyspeedy.f90.j2")
output = template.stream(model_state=model_state).dump("../source/pyspeedy.f90")
