# can find board required pkgs

    Code
      required_pkgs(board)
    Output
      character(0)

# metadata checking functions give correct errors

    `tags` must be a character vector.

---

    `metadata` must be a list.

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
      Creating new version '20120304T050607Z-ab444'
      Writing to pin 'x'
    Code
      pin_write(b, 1:5, "x", type = "rds")
    Message
      Replacing version '20120304T050607Z-ab444' with '20120304T050607Z-ab444'
      Writing to pin 'x'
    Code
      pin_write(b, 1:6, "x", type = "rds")
    Message
      Replacing version '20120304T050607Z-ab444' with '20120304T050607Z-ab6b5'
      Writing to pin 'x'

