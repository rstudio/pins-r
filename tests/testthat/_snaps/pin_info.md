# gives useful errors

    Code
      pin_info("mtcars2")
    Condition
      Error:
      ! `pin_info()` was deprecated in pins 1.4.0 and is now defunct.
      i Please use `pin_meta()` instead.
    Code
      pin(mtcars[1:2], "mtcars2", board = "test1")
    Condition
      Error:
      ! `pin()` was deprecated in pins 1.4.0 and is now defunct.
      i Please use `pin_write()` instead.
    Code
      pin(mtcars[1:2], "mtcars2", board = "test2")
    Condition
      Error:
      ! `pin()` was deprecated in pins 1.4.0 and is now defunct.
      i Please use `pin_write()` instead.
    Code
      pin_info("mtcars2")
    Condition
      Error:
      ! `pin_info()` was deprecated in pins 1.4.0 and is now defunct.
      i Please use `pin_meta()` instead.

# gives informative error if used with modern board

    Code
      board <- board_temp()
      pin_info("mtcars", board = board)
    Condition
      Error:
      ! `pin_info()` was deprecated in pins 1.4.0 and is now defunct.
      i Please use `pin_meta()` instead.

