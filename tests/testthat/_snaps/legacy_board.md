# good errors when use modern board with legacy api

    Code
      board <- board_temp()
      pin(mtcars, "mtcars", board = board)
    Condition
      Error in `this_not_that()`:
      ! Use `pin_write()` with this board, not `pin()`
    Code
      pin_get("mtcars", board = board)
    Condition
      Error in `this_not_that()`:
      ! Use `pin_read()` with this board, not `pin_get()`
    Code
      pin_find("mtcars", board = board)
    Condition
      Error in `this_not_that()`:
      ! Use `pin_search()` with this board, not `pin_find()`
    Code
      pin_versions("mtcars", board)
    Condition
      Error in `pin_versions()`:
      ! Please supply `board` then `name` when working with modern boards
    Code
      board_browse(board)
    Condition
      Error in `this_not_that()`:
      ! Use `pin_browse()` with this board, not `board_browse()`

