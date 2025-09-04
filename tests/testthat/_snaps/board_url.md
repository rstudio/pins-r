# provides key methods

    Code
      board_deparse(board)
    Output
      board_url(c(rds = "https://raw.githubusercontent.com/rstudio/pins-r/master/tests/testthat/pin-rds/"))

# raw pins can only be downloaded

    Cannot automatically read pin:
    * Is your pin specified as a full path? Retrieve it with `pin_download()`
    * Is your pin specified via a URL that is not a full path, such as a Posit Connect vanity URL? Remember to include a trailing slash `/`

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
      pin_meta(board, "froofy", version = "x")
    Condition
      Error in `pin_meta()`:
      ! Can't find pin called "froofy"
      i Use `pin_list()` to see all available pins in this board
    Code
      pin_meta(board, "x", version = "x")
    Condition
      Error in `pin_meta()`:
      ! This `board_url()` is not versioned
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
    Code
      pin(1:5, name = "x", board = board)
    Condition
      Error:
      ! `pin()` was deprecated in pins 1.4.0 and is now defunct.
      i Please use `pin_write()` instead.
    Code
      pin_get(name = "x", board = board)
    Condition
      Error:
      ! `pin_get()` was deprecated in pins 1.4.0 and is now defunct.
      i Please use `pin_read()` instead.

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

