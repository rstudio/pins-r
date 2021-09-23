# can't pin_read() file that was pin_uploaded()

    Code
      pin_read(board, "test")
    Error <rlang_error>
      Pin does not declare file type so can't be automatically read
      i Retrieve uploaded paths with `pin_download()`

# useful errors on bad inputs

    Code
      pin_write(mtcars)
    Error <rlang_error>
      `board` must be a pin board
    Code
      pin_write(board, mtcars, name = 1:10)
    Error <rlang_error>
      `name` must be a string
    Code
      pin_write(board, mtcars, name = "mtcars", type = "froopy-loops")
    Error <rlang_error>
      `type` must be one of "rds", "json", "arrow", "pickle", "csv", or "qs".
    Code
      pin_write(board, mtcars, name = "mtcars", metadata = 1)
    Error <rlang_error>
      `metadata` must be a list

# pin_write() noisily generates name and type

    Code
      b <- board_temp()
      pin_write(b, mtcars)
    Message <message>
      Using `name = 'mtcars'`
      Guessing `type = 'rds'`
      Creating new version '20120304T050607Z-dfa6c'
      Writing to pin 'mtcars'
    Code
      pin_write(b, data.frame(x = 1))
    Error <rlang_error>
      Must supply `name` when `x` is an expression

# can request specific hash

    Code
      b <- board_temp()
      pin_write(b, mtcars, name = "mtcars", type = "rds")
    Message <message>
      Creating new version '20120304T050607Z-dfa6c'
      Writing to pin 'mtcars'
    Code
      pin_read(b, "mtcars", hash = "ABCD")
    Error <rlang_error>
      Specified hash 'ABCD' doesn't match pin hash 'dfa6c1c109362781'

# informative error for legacy boards

    Code
      board <- legacy_temp()
      board %>% pin_write(1:10, "x")
    Error <rlang_error>
      Use `pin()` with this board, not `pin_write()`
    Code
      board %>% pin_read("x")
    Error <rlang_error>
      Use `pin_get()` with this board, not `pin_read()`

