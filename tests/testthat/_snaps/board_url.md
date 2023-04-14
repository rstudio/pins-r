# raw pins can only be downloaded

    Pin does not declare file type so can't be automatically read
    i Retrieve uploaded paths with `pin_download()`

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
      board %>% pin_meta("froofy", version = "x")
    Condition
      Error in `pin_meta()`:
      ! Can't find pin called "froofy"
      i Use `pin_list()` to see all available pins in this board
    Code
      board %>% pin_meta("x", version = "x")
    Condition
      Error in `pin_meta()`:
      ! This `board_url()` is not versioned
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
    Code
      board %>% board_deparse()
    Condition
      Error in `board_deparse()`:
      ! This board doesn't support deparsing
    Code
      pin(1:5, name = "x", board = board)
    Condition
      Error in `board_pin_create()`:
      ! Use `pin_write()` with this board, not `pin()`
    Code
      pin_get(name = "x", board = board)
    Condition
      Error in `board_pin_get()`:
      ! Use `pin_read()` with this board, not `pin_get()`

# useful errors for specifying board

    Code
      board_url(c("foo", "bar"))
    Condition
      Error in `get_url_format()`:
      ! `urls` must resolve to either:
      * an unnamed character scalar, i.e. a single URL
      * a named character vector
      * a named list, where all elements are character scalars or vectors
    Code
      board_url(list("a", 1:2))
    Condition
      Error in `get_url_format()`:
      ! `urls` must resolve to either:
      * an unnamed character scalar, i.e. a single URL
      * a named character vector
      * a named list, where all elements are character scalars or vectors
    Code
      board_url(1:10)
    Condition
      Error in `get_url_format()`:
      ! `urls` must resolve to either:
      * an unnamed character scalar, i.e. a single URL
      * a named character vector
      * a named list, where all elements are character scalars or vectors
    Code
      board_url(c(x = "foo"), headers = list(auth = "x"))
    Condition
      Error in `board_url()`:
      ! `headers` must be a named character vector or `NULL`, not a list.
    Code
      board_url(c(x = "foo"), headers = "my_api_key")
    Condition
      Error in `board_url()`:
      ! `headers` must be a named character vector or `NULL`, not the string "my_api_key".

