# useful errors for unsupported methods

    Code
      pin_write(board, 1:5, "x")
    Condition
      Error in `pin_store()`:
      ! `board_url()` is read only
    Code
      pin_delete(board, "x")
    Condition
      Error in `pin_delete()`:
      ! `board_url()` is read only
    Code
      pin_versions(board, "x")
    Condition
      Error in `pin_versions_modern()`:
      ! This `board_url()` is not versioned
    Code
      pin_version_delete(board, "x")
    Condition
      Error in `pin_version_delete()`:
      ! `board_url()` is read only

