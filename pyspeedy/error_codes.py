

from collections import defaultdict

ERROR_CODES = defaultdict(lambda x: f"Unexpected error: {x}")
ERROR_CODES[-1] = (
    "The model state was not initialized. "
    "Please inialize it before running the model."
)