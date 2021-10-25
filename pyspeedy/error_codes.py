from collections import defaultdict

ERROR_CODES = defaultdict(lambda x: f"Unexpected error: {x}")
ERROR_CODES[0] = "Run successful."
ERROR_CODES[
    -1
] = "The model state was not initialized. Please initialize it before running the model."
ERROR_CODES[-2] = "Model variables out of accepted range."
