# has useful print method

    Code
      board_folder(path)
    Output
      Pin board <pins_board_folder>
      Path: '<redacted>'
      Cache size: 0

# can browse

    Code
      b %>% pin_browse("x")
    Error <rlang_error>
      pin doesn't have remote url
    Code
      b %>% pin_browse("x", local = TRUE)
    Message <cliMessage>
      i Pin at <redacted>

# can deparse

    Code
      board_deparse(b)
    Output
      board_folder(path = "<redacted>")

# generates useful messages

    Code
      pin_write(b, 1:5, "x", type = "rds")
    Message <message>
      Creating new version '20120304T050607Z-ab444'
      Writing to pin 'x'
    Code
      pin_write(b, 1:5, "x", type = "rds")
    Message <message>
      Replacing version '20120304T050607Z-ab444' with '20120304T050607Z-ab444'
      Writing to pin 'x'
    Code
      pin_write(b, 1:6, "x", type = "rds")
    Message <message>
      Replacing version '20120304T050607Z-ab444' with '20120304T050607Z-ab6b5'
      Writing to pin 'x'

