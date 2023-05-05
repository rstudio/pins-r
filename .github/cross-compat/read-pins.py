import sys

from pins.data import mtcars
from pins import board_folder

board = board_folder(sys.argv[1])
res = board.pin_read("mtcars-r")
assert res.equals(mtcars)
