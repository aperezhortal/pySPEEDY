import os
import configparser
import sys

sys.path.insert(1, os.path.abspath("."))
sys.path.insert(1, os.path.abspath("../../examples"))
if "READTHEDOCS" not in os.environ:
    sys.path.insert(1, os.path.abspath("../../"))

# -- Project information -----------------------------------------------------

project = "pySPEEDY"
copyright = "2021, Andrés Pérez Hortal, Sam Hatfield, Fred Kucharski, Franco Molteni"
author = "Andrés Pérez Hortal, Sam Hatfield, Fred Kucharski, Franco Molteni"

# Get the full version from the setup.cfg file.
setup_cfg_path = os.path.join(os.path.dirname(__file__), "../../setup.cfg")

config = configparser.ConfigParser()
config.read(setup_cfg_path)
release = config["metadata"]["version"]

# If extensions (or modules to document with autodoc) are in another directory,
# add these directories to sys.path here. If the directory is relative to the
# documentation root, use os.path.abspath to make it absolute, like shown here.
#
import os
import sys

if "READTHEDOCS" not in os.environ:
    sys.path.insert(1, os.path.abspath("../../"))

# -- General configuration ---------------------------------------------------

extensions = [
    "numpydoc",
    "sphinx.ext.autodoc",
    "sphinx.ext.autosummary",
    "nbsphinx",
    "nbsphinx_link",
    "sphinx_gallery.load_style",
]

exclude_patterns = ["_build", "**.ipynb_checkpoints"]

templates_path = ["_templates"]

html_theme = "sphinx_rtd_theme"

# Add any paths that contain custom static files (such as style sheets) here,
# relative to this directory. They are copied after the builtin static files,
# so a file named "default.css" will overwrite the builtin "default.css".
html_static_path = ["_static"]

html_domain_indices = True

# Autosummary
autosummary_generate = True
numpydoc_show_class_members = False
autosummary_imported_members = True

# Build the Model State table.
sys.path.insert(1, os.path.abspath("../../registry"))
from model_state_def import export_model_state_html  # noqa

export_model_state_html()
