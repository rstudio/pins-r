# has useful print method

    Code
      board_folder("/tmp/test", name = "test")
    Output
      Pin board <pins_board_folder>
      Path: '/tmp/test'
      Cache size: 0

# can get versions

    Code
      pin_write(b, 1:5, "x", type = "rds")
    Message <message>
      Creating new version '20120304T050607Z-ab444'

---

    Code
      pin_write(b, 1:6, "x", type = "rds")
    Message <message>
      Creating new version '20120304T050607Z-ab6b5'

---

    Code
      pin_read(b, "x", version = "xxx")
    Error <rlang_error>
      Can't find version 'xxx'

# can't unversion an unversioned pin

    Code
      b <- board_temp(versioned = TRUE)
      pin_write(b, 1, "x", type = "rds")
    Message <message>
      Creating new version '20120304T050607Z-3de1f'
    Code
      pin_write(b, 2, "x", type = "rds")
    Message <message>
      Creating new version '20120304T050607Z-04a4e'
    Code
      pin_write(b, 3, "x", type = "rds", versioned = FALSE)
    Error <rlang_error>
      Pin is versioned, but you have requested a write without versions
      i To un-version a pin, you must delete it

# can browse

    Code
      b %>% pin_browse("x")
    Message <cliMessage>
      i Pin at </tmp/test/x/20120304T050607Z-afd4b>

---

    Code
      b %>% pin_browse("x", cache = TRUE)
    Error <rlang_error>
      board_local() does not have a cache

# generates useful messages

    Code
      pin_read(b, "x")
    Error <pins_pin_absent>
      Can't find pin called 'x'
      i Use `pin_list()` to see all available pins in this board
    Code
      pin_write(b, 1:5, "x", type = "rds")
    Message <message>
      Creating new version '20120304T050607Z-ab444'
    Code
      pin_write(b, 1:5, "x", type = "rds")
    Message <message>
      Replacing version '20120304T050607Z-ab444' with '20120304T050607Z-ab444'
    Code
      pin_write(b, 1:6, "x", type = "rds")
    Message <message>
      Replacing version '20120304T050607Z-ab444' with '20120304T050607Z-ab6b5'

