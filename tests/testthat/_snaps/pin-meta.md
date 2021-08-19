# informative error when used with legacy board

    Code
      board <- legacy_temp()
      pin_meta(board, "x")
    Error <rlang_error>
      Use `pin_info()` with this board, not `pin_meta()`

