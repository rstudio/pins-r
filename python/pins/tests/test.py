import pytest

import pandas as pd
import pins

def test_pin_get():
  assert len(pins.pin_get("hpiR/seattle_sales")) == 43313

def test_pin_find():
  assert len(pins.pin_find()) > 0

def test_pin():
  data = {
    'apples':  [1, 2, 0, 2], 
    'bananas': [0, 1, 2, 2]
  }
  
  data = pd.DataFrame(data)
  data = pins.pin(data, "fruits")
  
  assert len(data) == 4
