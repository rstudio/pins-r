# useful errors for unsupported methods

    Code
      board %>% pin_write(1:5, "x")
    Condition
      Error in `pin_store()`:
      ! `board_url()` is read only
    Code
      board %>% pin_delete("x")
    Condition
      Error in `pin_delete()`:
      ! `board_url()` is read only
    Code
      board %>% pin_versions("x")
    Condition
      Error in `pin_versions_modern()`:
      ! This `board_url()` is not versioned
    Code
      board %>% pin_version_delete("x")
    Condition
      Error in `pin_version_delete()`:
      ! `board_url()` is read only

