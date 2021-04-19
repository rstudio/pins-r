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
      Creating new version '3d563e5fb04b216f'

---

    Code
      pin_write(b, 1:6, "x", type = "rds", versioned = TRUE)
    Message <message>
      Creating new version '04815005671b2e74'

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
      Creating new version '3d563e5fb04b216f'
    Code
      pin_write(b, 1:5, "x", type = "rds")
    Message <message>
      Creating new version '3d563e5fb04b216f'
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
      Creating new version '3d563e5fb04b216f'
    Code
      pin_write(b, 1:5, "x", type = "rds")
    Message <message>
      Replacing version '3d563e5fb04b216f' with '3d563e5fb04b216f'
    Code
      pin_write(b, 1:6, "x", type = "rds")
    Message <message>
      Replacing version '3d563e5fb04b216f' with '04815005671b2e74'

