# has useful print method

    Code
      board_folder("/tmp", name = "test")
    Output
      Pin board <pins_board_local>
      Path: '/tmp'
      With no pins.

# can get versions

    Code
      pin_write(b, 1:5, "x", type = "rds", versioned = TRUE)
    Message <message>
      Creating new version '4cbcc49c50fda258'

---

    Code
      pin_write(b, 1:6, "x", type = "rds", versioned = TRUE)
    Message <message>
      Creating new version '5b1d0a90cd79c9d1'

---

    Code
      pin_read(b, "x", version = "xxx")
    Error <rlang_error>
      Can't find version 'xxx'

# can't unversion an unversioned pin

    Code
      b <- board_temp(versions = TRUE)
      pin_write(b, 1:5, "x", type = "rds")
    Message <message>
      Creating new version '4cbcc49c50fda258'
    Code
      pin_write(b, 1:5, "x", type = "rds")
    Message <message>
      Creating new version '4cbcc49c50fda258'
    Code
      pin_write(b, 1:5, "x", type = "rds", versioned = FALSE)
    Error <rlang_error>
      Pin is versioned, but you have requested a write without versions
      i To un-version a pin, you must delete it

# generates useful messages

    Code
      pin_read(b, "x")
    Error <rlang_error>
      Can't find pin 'x'
    Code
      pin_write(b, 1:5, "x", type = "rds")
    Message <message>
      Creating new version '4cbcc49c50fda258'
    Code
      pin_write(b, 1:5, "x", type = "rds")
    Message <message>
      Replacing version '4cbcc49c50fda258' with '4cbcc49c50fda258'
    Code
      pin_write(b, 1:6, "x", type = "rds")
    Message <message>
      Replacing version '4cbcc49c50fda258' with '5b1d0a90cd79c9d1'

