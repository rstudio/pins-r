# bad pin names give useful errors

    Code
      pin_get(1, board = board)
    Error <rlang_error>
      A package pin must be a string
    Code
      pin_get("a", board = board)
    Error <rlang_error>
      A package pin must have structure 'package/dataset'
    Code
      pin_get("a/b/c", board = board)
    Error <rlang_error>
      A package pin must have structure 'package/dataset'
    Code
      pin_get("datasets/BJsales", board = board)
    Error <rlang_error>
      'datasets/BJsales' isn't a single dataset
    Code
      pin_get("packagethatdoesntexist/x", board = board)
    Error <rlang_error>
      The `packagethatdoesntexist` package is required.

