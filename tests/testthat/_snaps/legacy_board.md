# good errors when use modern board with legacy api

    Code
      board <- board_temp()
      pin(mtcars, "mtcars", board = board)
    Condition
      Warning:
      `pin()` was deprecated in pins 1.4.0.
      i Please use `pin_write()` instead.
      Error in `board_pin_create()`:
      ! Use `pin_write()` with this board, not `pin()`
    Code
      pin_get("mtcars", board = board)
    Condition
      Warning:
      `pin_get()` was deprecated in pins 1.4.0.
      i Please use `pin_read()` instead.
      Error in `board_pin_get()`:
      ! Use `pin_read()` with this board, not `pin_get()`
    Code
      pin_find("mtcars", board = board)
    Condition
      Warning:
      `pin_find()` was deprecated in pins 1.4.0.
      i Please use `pin_search()` instead.
      Error in `board_pin_find()`:
      ! Use `pin_search()` with this board, not `pin_find()`
    Code
      pin_versions("mtcars", board)
    Condition
      Error in `pin_versions()`:
      ! Please supply `board` then `name` when working with modern boards
    Code
      board_browse(board)
    Condition
      Error in `board_browse()`:
      ! Use `pin_browse()` with this board, not `board_browse()`

