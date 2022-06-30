# raw pins can only be downloaded

    Pin does not declare file type so can't be automatically read
    i Retrieve uploaded paths with `pin_download()`

# useful errors for unsupported methods

    Code
      board %>% pin_write(1:5, "x")
    Condition
      Error in `abort_board_read_only()`:
      ! board_url() is read only
    Code
      board %>% pin_delete("x")
    Condition
      Error in `abort_board_read_only()`:
      ! board_url() is read only
    Code
      board %>% pin_meta("froofy", version = "x")
    Condition
      Error in `abort_pin_missing()`:
      ! Can't find pin called 'froofy'
      i Use `pin_list()` to see all available pins in this board
    Code
      board %>% pin_meta("x", version = "x")
    Condition
      Error in `pin_meta()`:
      ! board_url() doesn't support versions
    Code
      board %>% pin_versions("x")
    Condition
      Error in `pin_versions_modern()`:
      ! This board doesn't support versions
    Code
      board %>% board_deparse()
    Condition
      Error in `board_deparse()`:
      ! This board doesn't support deparsing
    Code
      pin(1:5, name = "x", board = board)
    Condition
      Error in `this_not_that()`:
      ! Use `pin_write()` with this board, not `pin()`
    Code
      pin_get(name = "x", board = board)
    Condition
      Error in `this_not_that()`:
      ! Use `pin_read()` with this board, not `pin_get()`

