# has useful print method

    Code
      board_folder("/tmp", name = "test")
    Output
      Pin board <pins_board_local>
      Path: '/tmp'
      With 0 pins: ''

# can get versions

    Code
      pin_write(b, 1:5, "x", type = "rds", versioned = TRUE)
    Message <message>
      Created version 3a40601dc0088965

---

    Code
      pin_write(b, 1:6, "x", type = "rds", versioned = TRUE)
    Message <message>
      Created version 16270b863f1f3278

---

    Code
      pin_read(b, "x", version = "xxx")
    Error <rlang_error>
      Can't find version xxx

# generates useful messages

    Code
      pin_write(b, 1:5, "x", type = "rds")
    Message <message>
      Created new pin
    Code
      pin_write(b, 1:5, "x", type = "rds")
    Message <message>
      Existing pin unchanged
    Code
      pin_write(b, 1:6, "x", type = "rds")
    Message <message>
      Replaced existing pin

