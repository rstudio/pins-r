# raw pins can only be downloaded

    Pin created with `pin_upload()`
    i Retrieve uploaded paths with `pin_download()`

# useful errors for unsupported methods

    Code
      board %>% pin_write(1:5, "x")
    Error <rlang_error>
      board_url() is read only
    Code
      board %>% pin_delete("x")
    Error <rlang_error>
      board_url() is read only
    Code
      board %>% pin_meta("froofy", version = "x")
    Error <rlang_error>
      Can't find pin called froofy
    Code
      board %>% pin_meta("x", version = "x")
    Error <rlang_error>
      board_url() doesn't support versions
    Code
      board %>% pin_versions("x")
    Error <rlang_error>
      board_url() doesn't support versions
    Code
      pin(1:5, name = "x", board = board)
    Error <rlang_error>
      Use `pin_write()` with this board, not `pin()`
    Code
      pin_get(name = "x", board = board)
    Error <rlang_error>
      Use `pin_read()` with this board, not `pin_get()`

