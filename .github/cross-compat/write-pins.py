import sys

from pins.data import mtcars
from pins import board_folder

board = board_folder(sys.argv[1])
board.pin_write(mtcars, "mtcars-py", type = "csv")
