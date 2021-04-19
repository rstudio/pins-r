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
      `type` must be one of "rds", "json", "arrow", "pickle", or "csv".

# pin_write() noisily generates name and type

    Code
      b <- board_temp()
      pin_write(b, mtcars)
    Message <message>
      Guessing `name = 'mtcars'`
      Guessing `type = 'rds'`
      Creating new version '5f21220925ff5df0'

# can request specific hash

    Code
      b <- board_temp()
      pin_write(b, mtcars, name = "mtcars", type = "rds")
    Message <message>
      Creating new version '5f21220925ff5df0'
    Code
      pin_read(b, "mtcars", hash = "ABCD")
    Error <rlang_error>
      Specified hash 'ABCD' doesn't match pin hash '5f21220925ff5df0'

