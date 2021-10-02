"""
This scripts is used to regenerate the fixtures.
It should only be used when some parts of the model change and the reference simulations
needs to be recomputed.
"""

from datetime import datetime
from pyspeedy import Speedy

model = Speedy(
    output_dir="./",
    start_date=datetime(1982, 1, 1),
    end_date=datetime(1982, 1, 4),
    history_interval=36,
    diag_interval=180,
    output_vars=None,
)

model.set_bc()
model.run()
