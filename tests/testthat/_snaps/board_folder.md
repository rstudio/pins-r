# can find board required pkgs

    Code
      required_pkgs(board)
    Output
      character(0)

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
    Condition
      Error in `pin_browse()`:
      ! pin doesn't have remote url
    Code
      b %>% pin_browse("x", local = TRUE)
    Message
      i Pin at <redacted>

# can deparse

    Code
      board_deparse(b)
    Output
      board_folder(path = "<redacted>")

# generates useful messages

    Code
      pin_write(b, 1:5, "x", type = "rds")
    Message
      Creating new version '20130104T050607Z-xxxxx'
      Writing to pin 'x'
    Code
      pin_write(b, 1:6, "x", type = "rds")
    Message
      Replacing version '20130104T050607Z-xxxxx' with '20130204T050607Z-yyyyy'
      Writing to pin 'x'
    Code
      pin_write(b, 1:7, "x", type = "rds")
    Message
      Replacing version '20130204T050607Z-yyyyy' with '20130304T050607Z-zzzzz'
      Writing to pin 'x'

