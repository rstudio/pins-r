# has useful print method

    Code
      board_folder("/tmp/test", name = "test")
    Output
      Pin board <pins_board_folder>
      Path: '/tmp/test'
      Cache size: 0

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

