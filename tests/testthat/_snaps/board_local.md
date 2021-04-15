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
      Creating new version '3a40601dc0088965'

---

    Code
      pin_write(b, 1:6, "x", type = "rds", versioned = TRUE)
    Message <message>
      Creating new version '16270b863f1f3278'

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
      Creating new version '3a40601dc0088965'
    Code
      pin_write(b, 1:5, "x", type = "rds")
    Message <message>
      Creating new version '3a40601dc0088965'
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
      Creating new version '3a40601dc0088965'
    Code
      pin_write(b, 1:5, "x", type = "rds")
    Message <message>
      Replacing version '3a40601dc0088965' with '3a40601dc0088965'
    Code
      pin_write(b, 1:6, "x", type = "rds")
    Message <message>
      Replacing version '3a40601dc0088965' with '16270b863f1f3278'

