"""
This scripts is used to regenerate the fixtures.
It should only be used when some parts of the model change and the reference simulations
needs to be recomputed.
"""

from datetime import datetime
from pyspeedy import Speedy
from pyspeedy.callbacks import XarrayExporter

model = Speedy(
    start_date=datetime(1982, 1, 1),
    end_date=datetime(1982, 1, 4),
    diag_interval=180,
)
model.set_bc()

callbacks = [
    XarrayExporter(interval=36, spinup_time=None, output_dir="./", verbose=True)
]

model.run(callbacks=callbacks)
