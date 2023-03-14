# can find board required pkgs

    Code
      required_pkgs(board)
    Output
      [1] "rsconnect"

# metadata checking functions give correct errors

    `tags` must be a character vector or `NULL`, not a list.

---

    `metadata` must be a list or `NULL`, not a character vector.

# get useful error for rebranding

    `board_rsconnect()` was deprecated in pins 1.1.0.
    i Please use `board_connect()` instead.

# can deparse

    Code
      board_deparse(board)
    Output
      board_connect(auth = "envvar")

# can find content by full/partial name

    Code
      rsc_content_find(board, "marjory/test-partial")
    Condition
      Error in `rsc_content_find_live()`:
      ! Can't find pin named 'test-partial' with owner 'marjory'

# can create and delete content

    Code
      rsc_content_create(board, "test-1", list())
    Condition
      Error in `rsc_check_status()`:
      ! Posit Connect API failed [409]
      * An object with that name already exists.

---

    Code
      rsc_content_delete(board, "test-1")
    Condition
      Error in `rsc_content_find_live()`:
      ! Can't find pin called "test-1"
      i Use `pin_list()` to see all available pins in this board

