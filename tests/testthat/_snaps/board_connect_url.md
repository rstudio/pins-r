# can deparse

    Code
      board_deparse(board)
    Output
      board_connect_url(c(x = "https://connect.example.com/sizable_tigerbeetle/", 
          y = "https://connect.example.com/avaricious_indianspinyloach/"), 
          auth = "envvar")

# useful errors for unsupported methods

    Code
      board %>% pin_write(1:5, "x")
    Condition
      Error in `abort_board_read_only()`:
      ! pins_board_connect_url() is read only
    Code
      board %>% pin_delete("x")
    Condition
      Error in `abort_board_read_only()`:
      ! pins_board_connect_url() is read only
    Code
      board %>% pin_meta("x")
    Condition
      Error in `pin_meta()`:
      ! Malformed vanity URL(s):
      x <foo>
      i Check the vanity URL for your pin
    Code
      board %>% pin_versions("x")
    Condition
      Error in `pin_versions_modern()`:
      ! This board doesn't support versions
    Code
      board %>% pin_version_delete("x")
    Condition
      Error in `pin_version_delete()`:
      ! This board doesn't support versions

