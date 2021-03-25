# useful errors on bad inputs

    Code
      pin_write(mtcars)
    Error <rlang_error>
      `board` must be a pin board
    Code
      pin_write(board, mtcars, name = 1:10)
    Message <message>
      Guessing `type = 'rds'`
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
      Guessing `type = 'rds'`
      Guessing `name = 'mtcars'`
      Created new pin

# can request specific hash

    Code
      b <- board_temp()
      pin_write(b, mtcars, name = "mtcars", type = "rds")
    Message <message>
      Created new pin
    Code
      pin_read(b, "mtcars", hash = "ABCD")
    Error <rlang_error>
      Specified hash 'ABCD' doesn't match pin hash 'd2a1679fa44c4fc3'

