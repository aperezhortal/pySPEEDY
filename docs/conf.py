import os
from os.path import abspath
import configparser
import sys
import subprocess
from pathlib import Path

DOCS_FOLDER = Path(__file__).parent
PACKAGE_FOLDER = DOCS_FOLDER / ".."
REGISTRY_FOLDER = PACKAGE_FOLDER / "registry"


def run_cmd(cmd, cwd=None):
    print(f'Running: "{cmd}"')
    cmd = cmd.split(" ")
    result = subprocess.run(cmd, capture_output=True, cwd=cwd)
    print(result.stdout.decode("utf-8"))
    if result.returncode != 0:
        raise RuntimeError('Error running "{cmd}"')


sys.path.insert(1, abspath(str(PACKAGE_FOLDER / "examples")))
sys.path.insert(1, abspath(str(PACKAGE_FOLDER)))

if "READTHEDOCS" in os.environ:
    # In read the docs, lets create first the fortran documentation.
    ford_cfg = str(DOCS_FOLDER / "speedy_f90_ford_project_file.md")
    run_cmd(f"pwd")  # Additional debugging info
    run_cmd(f"ford {ford_cfg}", cwd=str(DOCS_FOLDER))
    run_cmd(f"ls", cwd=str(DOCS_FOLDER))  # Additional debugging info
    run_cmd(f"ls _build/html", cwd=str(DOCS_FOLDER))  # Additional debugging info

# -- Project information -----------------------------------------------------

project = "pySPEEDY"
copyright = "2021, Andrés Pérez Hortal, Sam Hatfield, Fred Kucharski, Franco Molteni"
author = "Andrés Pérez Hortal, Sam Hatfield, Fred Kucharski, Franco Molteni"

# Get the full version from the setup.cfg file.
setup_cfg_path = str(PACKAGE_FOLDER / "setup.cfg")

config = configparser.ConfigParser()
config.read(setup_cfg_path)
release = config["metadata"]["version"]

# If extensions (or modules to document with autodoc) are in another directory,
# add these directories to sys.path here. If the directory is relative to the
# documentation root, use os.path.abspath to make it absolute, like shown here.
#
import sys

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
sys.path.insert(1, abspath(str(REGISTRY_FOLDER)))
from model_state_def import export_model_state_html  # noqa

export_model_state_html(str(DOCS_FOLDER / "_build/html/model_state_def.html"))

if "READTHEDOCS" in os.environ:
    # In read the docs, lets print additional debugging info
    run_cmd(f"ls _build/html", cwd=str(DOCS_FOLDER))  # Additional debugging info
    run_cmd(f"ls -R _build/html", cwd=str(DOCS_FOLDER))  # Additional debugging info
    run_cmd(f"ls -1 ..", cwd=str(DOCS_FOLDER))  # Additional debugging info
    run_cmd(f"ls -1 ../../", cwd=str(DOCS_FOLDER))  # Additional debugging info
