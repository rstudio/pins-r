# good errors when use modern board with legacy api

    Code
      board <- board_temp()
      pin(mtcars, "mtcars", board = board)
    Error <rlang_error>
      Use `pin_write()` with this board, not `pin()`
    Code
      pin_get("mtcars", board = board)
    Error <rlang_error>
      Use `pin_read()` with this board, not `pin_get()`
    Code
      pin_find("mtcars", board = board)
    Error <rlang_error>
      Use `pin_search()` with this board, not `pin_find()`
    Code
      pin_versions("mtcars", board)
    Error <rlang_error>
      Please supply `board` then `name` when working with modern boards
    Code
      board_browse(board)
    Error <rlang_error>
      Use `pin_browse()` with this board, not `board_browse()`

