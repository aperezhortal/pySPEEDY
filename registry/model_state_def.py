from collections import defaultdict

from jinja2 import FileSystemLoader, Environment
from pathlib import Path
import json
import re

try:
    # Used to export the model state vars into an excel file
    import xlsxwriter

    XLSXWRITER_IMPORTED = True
except ImportError:
    XLSXWRITER_IMPORTED = False

try:
    # Used to export the model state vars into an excel file
    import pandas as pd

    PANDAS_IMPORTED = True
except ImportError:
    PANDAS_IMPORTED = False


THIS_FILE_DIR = Path(__file__).parent
SOURCES_DIR = (THIS_FILE_DIR / "../source").resolve()
DOCS_DIR = (THIS_FILE_DIR / "../docs/source").resolve()
PYSPEEDY_DATA_DIR = (THIS_FILE_DIR / "../pyspeedy/data").resolve()

NC_DIMS_LUT = {"ix": "lon", "il": "lat", "kx": "lev"}


class VarDef:
    def __init__(
        self,
        name,
        dtype,
        dims,
        desc,
        units=None,
        time_dim=None,
        alt_name=None,
        value=None,
    ):
        """
        If time_dim is not None, the variable is not allocated during the
        initialization since it depends on the duration of the simulation.
        """
        self.name = name
        self.dtype = dtype
        self.dims = dims
        self.nc_dims = None

        if dims is not None:
            for dim, nc_dim in NC_DIMS_LUT.items():
                dims = re.sub(r"\b%s\b" % dim, nc_dim, dims)
            self.nc_dims = dims.replace(" ", "").replace("(", "").replace(")", "")
            self.nc_dims = [s for s in self.nc_dims.split(",")]

        self.desc = desc
        self.time_dim = time_dim
        self.units = units
        self.alt_name = name
        if alt_name is not None:
            self.alt_name = alt_name
        self.is_module_instance = "class" in dtype.lower()
        self.value = value

    @property
    def dimension(self):
        if self.dims:
            dimension = ", ".join(":" * self.ndim)
            return f"dimension({dimension})"

    @property
    def dimension_args(self):
        if self.dims:
            return self.dims.replace("(", "").replace(")", "")

    @property
    def dimension_args_declaration(self):
        if self.dims:
            return f"integer, intent(in) :: {self.dimension_args}"

    @property
    def ndim(self):
        if self.dims:
            return len(self.dims.split(","))

    def __repr__(self):
        return str({k: v for k, v in self.__dict__.items() if not k.startswith("_")})


model_state = [
    ########################################
    # Model integration control variables
    ########################################
    VarDef("current_step", "integer", None, "Current model step."),
    ########################################
    # Prognostic variables (spectral domain)
    ########################################
    VarDef("vor", "complex(8)", "(mx, nx, kx, t_levs)", "Vorticity"),
    VarDef("div", "complex(8)", "(mx, nx, kx, t_levs)", "Divergence"),
    VarDef("t", "complex(8)", "(mx, nx, kx, t_levs)", "Temperature", "K"),
    VarDef(
        "ps",
        "complex(8)",
        "(mx, nx, t_levs)",
        "Log of (normalised) surface pressure",
        "(p_s/p0)",
    ),
    VarDef(
        "tr",
        "complex(8)",
        "(mx, nx, kx, t_levs,ntr)",
        "Tracers (tr(1): specific humidity in g/kg)",
    ),
    VarDef("phi", "complex(8)", "(mx, nx, kx)", "Atmospheric geopotential"),
    VarDef("phis", "complex(8)", "(mx, nx)", "Surface geopotential"),
    ####################################
    # Prognostic variables (grid domain)
    ####################################
    VarDef("u_grid", "real(8)", "(ix, il, kx)", "eastward_wind", "m/s", alt_name="u"),
    VarDef("v_grid", "real(8)", "(ix, il, kx)", "northward_wind", "m/s", alt_name="v"),
    VarDef("t_grid", "real(8)", "(ix, il, kx)", "air_temperature", "K", alt_name="t"),
    VarDef(
        "q_grid", "real(8)", "(ix, il, kx)", "specific_humidity", "Kg/Kg", alt_name="q"
    ),
    VarDef(
        "phi_grid",
        "real(8)",
        "(ix, il, kx)",
        "geopotential_height",
        "m",
        alt_name="phi",
    ),
    VarDef(
        "ps_grid", "real(8)", "(ix, il)", "surface_air_pressure", "Pa", alt_name="ps"
    ),
    ################################################
    # Auxiliary variables used by the physic schemes
    ################################################
    VarDef(
        "precnv", "real(8)", "(ix, il)", "Convective precipitation, total", "g/(m^2 s)"
    ),
    VarDef(
        "precls", "real(8)", "(ix, il)", "Large-scale precipitation, total", "g/(m^2 s)"
    ),
    VarDef(
        "snowcv",
        "real(8)",
        "(ix, il)",
        "Convective precipitation, snow only",
        "g/(m^2 s)",
    ),
    VarDef(
        "snowls",
        "real(8)",
        "(ix, il)",
        "Large-scale precipitation, snow only",
        "g/(m^2 s)",
    ),
    VarDef("cbmf", "real(8)", "(ix, il)", "Cloud-base mass flux"),
    VarDef(
        "tsr", "real(8)", "(ix, il)", "Top-of-atmosphere shortwave radiation (downward)"
    ),
    VarDef(
        "ssrd", "real(8)", "(ix, il)", "Surface shortwave radiation (downward-only)"
    ),
    VarDef("ssr", "real(8)", "(ix, il)", "Surface shortwave radiation (net downward)"),
    VarDef("slrd", "real(8)", "(ix, il)", "Surface longwave radiation (downward-only)"),
    VarDef("slr", "real(8)", "(ix, il)", "Surface longwave radiation (net upward)"),
    VarDef("olr", "real(8)", "(ix, il)", "Outgoing longwave radiation (upward)"),
    # Third dimension -> 1:land, 2:sea, 3: weighted average
    VarDef("slru", "real(8)", "(ix, il,aux_dim)", "Surface longwave emission (upward)"),
    VarDef("ustr", "real(8)", "(ix, il,aux_dim)", "U-stress"),
    VarDef("vstr", "real(8)", "(ix, il,aux_dim)", "Vstress"),
    VarDef("shf", "real(8)", "(ix, il,aux_dim)", "Sensible heat flux"),
    VarDef("evap", "real(8)", "(ix, il,aux_dim)", "Evaporation", "g/(m^2 s)"),
    VarDef("hfluxn", "real(8)", "(ix, il,aux_dim)", "Net heat flux into surface"),
    #
    # Saved computations
    VarDef(
        "tt_rsw",
        "real(8)",
        "(ix, il,kx)",
        "Flux of short-wave radiation absorbed in each atmospheric layer",
    ),
    ###########################
    # Boundary module variables
    ###########################
    VarDef("phi0", "real(8)", "(ix, il)", "Unfiltered surface geopotential"),
    VarDef("orog", "real(8)", "(ix, il)", "Orography", "m"),
    VarDef("phis0", "real(8)", "(ix, il)", "Spectrally-filtered surface geopotential"),
    VarDef("alb0", "real(8)", "(ix, il)", "Bare-land annual-mean albedo"),
    VarDef("forog", "real(8)", "(ix, il)", "Orographic factor for land surface drag"),
    #################################
    # Surface fluxes module variables
    #################################
    VarDef("fmask_orig", "real(8)", "(ix, il)", "Original (fractional) land-sea mask"),
    ###############################
    # Geopotential module variables
    ###############################
    VarDef("xgeop1", "real(8)", "(kx)", "Constant 1 for hydrostatic equation"),
    VarDef("xgeop2", "real(8)", "(kx)", "Constant 2 for hydrostatic equation"),
    #############################
    # Land model module variables
    #############################
    VarDef(
        "stl12",
        "real(8)",
        "(ix, il, 12)",
        "Land surface temperature monthly-mean climatology",
    ),
    VarDef(
        "snowd12",
        "real(8)",
        "(ix, il, 12)",
        "Snow depth (water equivalent) monthly-mean climatology",
    ),
    VarDef(
        "soilw12",
        "real(8)",
        "(ix, il, 12)",
        "Soil water availability monthly-mean climatology",
    ),
    VarDef("veg_low", "real(8)", "(ix, il)", "Low vegetation fraction"),
    VarDef("veg_high", "real(8)", "(ix, il)", "High vegetation fraction"),
    VarDef("soil_wc_l1", "real(8)", "(ix, il, 12)", "Soil water content: Layer 1"),
    VarDef("soil_wc_l2", "real(8)", "(ix, il, 12)", "Soil water content: Layer 2"),
    VarDef("soil_wc_l3", "real(8)", "(ix, il, 12)", "Soil water content: Layer 3"),
    ############################
    # Sea model module variables
    ############################
    VarDef("sst12", "real(8)", "(ix, il, 12)", "Sea/ice surface temperature", "K"),
    VarDef("sea_ice_frac12", "real(8)", "(ix, il, 12)", "Sea ice fraction"),
    VarDef(
        "sst_anom",
        "real(8)",
        "(ix, il, 0:n_months+1)",
        "Observed SST anomaly (input).",
        time_dim="n_months",
    ),
    ############################
    # Shortware radiation module
    ############################
    VarDef(
        "increase_co2",
        "logical",
        None,
        " Flag for CO2 optical thickness increase",
        value=".false.",
    ),
    VarDef(
        "compute_shortwave",
        "logical",
        None,
        "Flag for shortwave radiation routine (turned on and off in main loop depending on the value of nstrad)",
        value=".true.",
    ),
    VarDef(
        "air_absortivity_co2",
        "real(8)",
        None,
        "Absorptivity of air in CO2 band",
        value="6.0",
    ),
    VarDef("flux_solar_in", "real(8)", "(ix, il)", "Flux of incoming solar radiation"),
    VarDef(
        "flux_ozone_lower",
        "real(8)",
        "(ix, il)",
        "Flux absorbed by ozone (lower stratosphere)",
    ),
    VarDef(
        "flux_ozone_upper",
        "real(8)",
        "(ix, il)",
        "Flux absorbed by ozone (upper stratosphere)",
    ),
    VarDef(
        "zenit_correction",
        "real(8)",
        "(ix, il)",
        "Zenith angle correction to (downward) absorptivity",
    ),
    VarDef(
        "stratospheric_correction",
        "real(8)",
        "(ix, il)",
        "Stratospheric correction for polar night",
    ),
    VarDef(
        "qcloud_equiv", "real(8)", "(ix, il)", " Equivalent specific humidity of clouds"
    ),
    ###################
    # Land model module
    ###################
    VarDef("rhcapl", "real(8)", "(ix, il)", "1/heat capacity (land)"),
    VarDef("cdland", "real(8)", "(ix, il)", " 1/dissipation time (land)"),
    VarDef(
        "stlcl_obs", "real(8)", "(ix, il)", "Climatological land surface temperature"
    ),
    VarDef(
        "snowdcl_obs",
        "real(8)",
        "(ix, il)",
        "Climatological snow depth (water equivalent)",
    ),
    VarDef(
        "soilwcl_obs", "real(8)", "(ix, il)", "Climatological soil water availability"
    ),
    VarDef("land_temp", "real(8)", "(ix, il)", "Land surface temperature"),
    VarDef("snow_depth", "real(8)", "(ix, il)", "Snow depth (water equivalent)"),
    VarDef("soil_avail_water", "real(8)", "(ix, il)", "Soil water availability"),
    VarDef("stl_lm", "real(8)", "(ix, il)", "Land-model surface temperature"),
    VarDef("fmask_land", "real(8)", "(ix, il)", "Fraction of land"),
    VarDef("bmask_land", "real(8)", "(ix, il)", " Binary land mask"),
    VarDef(
        "land_coupling_flag",
        "logical",
        None,
        "Flag for land-coupling (0: off, 1: on)",
        value=".true.",
    ),
    ###
    # Sea model
    ####
    VarDef("rhcaps", "real(p)", "(ix, il)", "1./heat_capacity (sea)"),
    VarDef("rhcapi", "real(p)", "(ix, il)", "1./heat_capacity (ice)"),
    VarDef("cdsea", "real(p)", "(ix, il)", "1./dissip_time (sea)"),
    VarDef("cdice", "real(p)", "(ix, il)", "1./dissip_time (ice)"),
    VarDef("fmask_sea", "real(p)", "(ix, il)", "Fraction of sea"),
    VarDef("bmask_sea", "real(p)", "(ix, il)", "Binary sea mask"),
    VarDef("deglat_s", "real(p)", "(il)", "Grid latitudes"),
    VarDef("hfseacl", "real(p)", "(ix, il)", "Annual-mean heat flux into sea sfc."),
    VarDef("sstom12", "real(p)", "(ix, il, 12)", "Ocean model SST climatology"),
    VarDef("sstcl_ob", "real(p)", "(ix, il)", "Observed clim. SST"),
    VarDef("sicecl_ob", "real(p)", "(ix, il)", "Clim. sea ice fraction"),
    VarDef("ticecl_ob", "real(p)", "(ix, il)", "Clim. sea ice temperature"),
    VarDef("sstan_ob", "real(p)", "(ix, il)", "Daily observed SST anomaly"),
    VarDef("sstcl_om", "real(p)", "(ix, il)", "Ocean model clim. SST"),
    VarDef("sst_am", "real(p)", "(ix, il)", "SST (full-field)"),
    VarDef("sstan_am", "real(p)", "(ix, il)", "SST anomaly"),
    VarDef("sice_am", "real(p)", "(ix, il)", "Sea ice fraction"),
    VarDef("tice_am", "real(p)", "(ix, il)", "Sea ice temperature"),
    VarDef("sst_om", "real(p)", "(ix, il)", "Ocean model SST"),
    VarDef("sice_om", "real(p)", "(ix, il)", "Model sea ice fraction"),
    VarDef("tice_om", "real(p)", "(ix, il)", "Model sea ice temperature"),
    VarDef("ssti_om", "real(p)", "(ix, il)", "Model SST + sea ice temp."),
    VarDef(
        "wsst_ob", "real(p)", "(ix, il)", "Weight for obs. SST anomaly in coupled runs"
    ),
    VarDef(
        "sst_anomaly_coupling_flag",
        "logical",
        None,
        "Weight for obs. SST anomaly in coupled runs",
        value=".true.",
    ),
    ###################
    # mod_radcon module
    ###################
    VarDef(
        "ablco2_ref", "real(8)", None, "Initial absorptivity of air in CO2 band (t=t0)"
    ),
    VarDef(
        "fband",
        "real(8)",
        "(100:400,4)",
        "Energy fraction emitted in each LW band = f(T)",
    ),
    VarDef(
        "alb_land",
        "real(8)",
        "(ix,il)",
        "Daily-mean albedo over land (bare-land + snow)",
    ),
    VarDef(
        "alb_sea",
        "real(8)",
        "(ix,il)",
        "Daily-mean albedo over sea  (open sea + sea ice)",
    ),
    VarDef("alb_surface", "real(8)", "(ix,il)", "Combined surface albedo (land + sea)"),
    VarDef("snowc", "real(8)", "(ix,il)", "Effective snow cover (fraction)"),
    VarDef(
        "rad_flux", "real(8)", "(ix,il,4)", "Radiative flux in different spectral bands"
    ),
    VarDef(
        "rad_tau2", "real(8)", "(ix,il,kx,4)", "Transmissivity of atmospheric layers"
    ),
    VarDef(
        "rad_st4a",
        "real(8)",
        "(ix,il,kx,2)",
        "Blackbody emission from full and half atmospheric levels",
    ),
    VarDef("rad_strat_corr", "real(8)", "(ix,il,2)", "Stratospheric correction term"),
    #############
    # Coordinates
    #############
    VarDef("lon", "real", "(ix)", "longitude", "[degrees]"),
    VarDef("lat", "real", "(il)", "latitude", "[degrees]"),
    VarDef("lev", "real", "(kx)", "atmosphere_sigma_coordinate", "[]"),
    #
    # Module instances
    #
    VarDef(
        "mod_geometry",
        "class(ModGeometry_t)",
        None,
        "Geometry module instance",
    ),
    VarDef(
        "mod_spectral",
        "class(ModSpectral_t)",
        None,
        "Spectral module instance",
    ),
    VarDef(
        "mod_implicit",
        "class(ModImplicit_t)",
        None,
        "Implicit module instance",
    ),
]


def build_fortran_sources():
    """Create the sources for the Python interface and the model state."""
    state_arrays = [
        var for var in model_state if var.dims and not var.is_module_instance
    ]
    state_scalars = [
        var for var in model_state if var.dims is None and not var.is_module_instance
    ]
    state_modules = [var for var in model_state if var.is_module_instance]

    file_loader = FileSystemLoader(THIS_FILE_DIR / "templates")
    env = Environment(loader=file_loader, trim_blocks=True, lstrip_blocks=True)
    template = env.get_template("model_state.f90.j2")
    output_file = str(SOURCES_DIR / "model_state.f90")
    template.stream(
        state_arrays=state_arrays,
        state_scalars=state_scalars,
        state_modules=state_modules,
    ).dump(output_file)
    print(f"Saved source: {output_file} file.")

    template = env.get_template("speedy_driver.f90.j2")
    output_file = str(SOURCES_DIR / "speedy_driver.f90")
    template.stream(state_arrays=state_arrays, state_scalars=state_scalars).dump(
        str(SOURCES_DIR / "speedy_driver.f90")
    )
    print(f"Saved source: {output_file} file.")


def export_model_state_html():
    """
    Export state variables description to HTML for docs.
    """
    file_loader = FileSystemLoader(THIS_FILE_DIR / "templates")
    env = Environment(loader=file_loader, trim_blocks=True, lstrip_blocks=True)
    template = env.get_template("model_state_def.html")
    file_path = str(DOCS_DIR / "model_state_def.html")
    template.stream(model_state=model_state).dump(file_path)
    return file_path


def export_model_state_json():
    """Export state variables description in JSON format."""
    data2json = {
        var.name: dict(
            dtype=var.dtype,
            dims=var.dims,
            desc=var.desc,
            time_dim=var.time_dim,
            units=var.units,
            nc_dims=var.nc_dims,
            alt_name=var.alt_name,
        )
        for var in model_state
    }

    file_path = PYSPEEDY_DATA_DIR / "model_state.json"
    with open(file_path, "w") as outfile:
        json.dump(data2json, outfile, indent=4)

    print(f"Saved state definition: {file_path} file.")
    return file_path


def export_model_state_excel():
    """Export state variables description in Excel format."""
    if XLSXWRITER_IMPORTED and PANDAS_IMPORTED:
        _data = defaultdict(list)
        for var in model_state:
            _data["name"].append(var.name)
            _data["dtype"].append(var.dtype)
            _data["dims"].append(var.dims)
            _data["nc_dims"].append(var.dims)
            _data["desc"].append(var.desc)
            _data["units"].append(var.units)
            _data["time_dim"].append(var.time_dim)
            _data["alt_name"].append(var.alt_name)

        my_dataframe = pd.DataFrame(data=_data)

        file_path = PYSPEEDY_DATA_DIR / "model_state.xlsx"

        writer = pd.ExcelWriter(file_path, engine="xlsxwriter")
        sheetname = "state_variables"
        my_dataframe.to_excel(writer, sheet_name=sheetname, index=False)
        # Adjust the columns size
        worksheet = writer.sheets[sheetname]  # pull worksheet object
        for idx, col in enumerate(my_dataframe):  # loop through all columns
            series = my_dataframe[col]
            max_len = (
                max(
                    (
                        series.astype(str).map(len).max(),  # len of largest item
                        len(str(series.name)),  # len of column name/header
                    )
                )
                + 1
            )  # adding a little extra space
            worksheet.set_column(idx, idx, max_len)  # set column width
        writer.save()
        return file_path


if __name__ == "__main__":
    build_fortran_sources()
    export_model_state_json()
    export_model_state_excel()
