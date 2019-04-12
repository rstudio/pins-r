"""
The ``pins`` module provides an API for tracking, discovering and sharing datasets.
"""

import os
import yaml

def find_pin():
    """
    Find pins in the active board.
    """
    config_path = os.path.expanduser("~/pins/arrow/config.yml")
    with open(config_path, "r") as stream:
        try:
            print(yaml.safe_load(stream))
        except yaml.YAMLError as exc:
            print(exc)

    return []
