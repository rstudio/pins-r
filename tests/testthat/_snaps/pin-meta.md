# informative error when used with legacy board

    Code
      board <- legacy_temp()
      pin_meta(board, "x")
    Condition
      Error in `this_not_that()`:
      ! Use `pin_info()` with this board, not `pin_meta()`

