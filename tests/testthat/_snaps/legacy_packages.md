# bad pin names give useful errors

    Code
      pin_get(1, board = board)
    Condition
      Error in `board_pin_get()`:
      ! A package pin must be a string
    Code
      pin_get("a", board = board)
    Condition
      Error in `board_pin_get()`:
      ! A package pin must have structure 'package/dataset'
    Code
      pin_get("a/b/c", board = board)
    Condition
      Error in `board_pin_get()`:
      ! A package pin must have structure 'package/dataset'
    Code
      pin_get("datasets/BJsales", board = board)
    Condition
      Error in `board_pin_get()`:
      ! 'datasets/BJsales' isn't a single dataset
    Code
      pin_get("packagethatdoesntexist/x", board = board)
    Condition
      Error in `board_pin_get()`:
      ! The package "packagethatdoesntexist" is required.

