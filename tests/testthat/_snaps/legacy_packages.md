# bad pin names give useful errors

    Code
      pin_get(1, board = board)
    Condition
      Error:
      ! `pin_get()` was deprecated in pins 1.4.0 and is now defunct.
      i Please use `pin_read()` instead.
    Code
      pin_get("a", board = board)
    Condition
      Error:
      ! `pin_get()` was deprecated in pins 1.4.0 and is now defunct.
      i Please use `pin_read()` instead.
    Code
      pin_get("a/b/c", board = board)
    Condition
      Error:
      ! `pin_get()` was deprecated in pins 1.4.0 and is now defunct.
      i Please use `pin_read()` instead.
    Code
      pin_get("datasets/BJsales", board = board)
    Condition
      Error:
      ! `pin_get()` was deprecated in pins 1.4.0 and is now defunct.
      i Please use `pin_read()` instead.
    Code
      pin_get("packagethatdoesntexist/x", board = board)
    Condition
      Error:
      ! `pin_get()` was deprecated in pins 1.4.0 and is now defunct.
      i Please use `pin_read()` instead.

