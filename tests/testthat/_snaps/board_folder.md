# has useful print method

    Code
      board_folder("/tmp/test", name = "test")
    Output
      Pin board <pins_board_folder>
      Path: '/tmp/test'
      With no pins.

# can get versions

    Code
      pin_write(b, 1:5, "x", type = "rds", versioned = TRUE)
    Message <message>
      Creating new version 'ab444e71e875d63b'

---

    Code
      pin_write(b, 1:6, "x", type = "rds", versioned = TRUE)
    Message <message>
      Creating new version 'ab6b56a248de3a44'

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
      Creating new version 'ab444e71e875d63b'
    Code
      pin_write(b, 1:5, "x", type = "rds")
    Message <message>
      Creating new version 'ab444e71e875d63b'
    Code
      pin_write(b, 1:5, "x", type = "rds", versioned = FALSE)
    Error <rlang_error>
      Pin is versioned, but you have requested a write without versions
      i To un-version a pin, you must delete it

# can browse

    Code
      b %>% pin_browse("x")
    Message <cliMessage>
      i Pin at </tmp/test/x/afd4b2f6506fce6d>

---

    Code
      b %>% pin_browse("x", cache = TRUE)
    Error <rlang_error>
      board_local() does not have a cache

# generates useful messages

    Code
      pin_read(b, "x")
    Error <rlang_error>
      Can't find pin 'x'
    Code
      pin_write(b, 1:5, "x", type = "rds")
    Message <message>
      Creating new version 'ab444e71e875d63b'
    Code
      pin_write(b, 1:5, "x", type = "rds")
    Message <message>
      Replacing version 'ab444e71e875d63b' with 'ab444e71e875d63b'
    Code
      pin_write(b, 1:6, "x", type = "rds")
    Message <message>
      Replacing version 'ab444e71e875d63b' with 'ab6b56a248de3a44'

