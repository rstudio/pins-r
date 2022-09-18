# raw pins can only be downloaded

    Pin does not declare file type so can't be automatically read
    i Retrieve uploaded paths with `pin_download()`

# useful errors for unsupported methods

    Code
      board %>% pin_write(1:5, "x")
    Condition
      Error in `abort_board_read_only()`:
      ! board_url() is read only
    Code
      board %>% pin_delete("x")
    Condition
      Error in `abort_board_read_only()`:
      ! board_url() is read only
    Code
      board %>% pin_meta("froofy", version = "x")
    Condition
      Error in `abort_pin_missing()`:
      ! Can't find pin called 'froofy'
      i Use `pin_list()` to see all available pins in this board
    Code
      board %>% pin_meta("x", version = "x")
    Condition
      Error in `pin_meta()`:
      ! This board_url() is not versioned
    Code
      board %>% pin_versions("x")
    Condition
      Error in `pin_versions_modern()`:
      ! This board_url() is not versioned
    Code
      board %>% board_deparse()
    Condition
      Error in `board_deparse()`:
      ! This board doesn't support deparsing
    Code
      pin(1:5, name = "x", board = board)
    Condition
      Error in `this_not_that()`:
      ! Use `pin_write()` with this board, not `pin()`
    Code
      pin_get(name = "x", board = board)
    Condition
      Error in `this_not_that()`:
      ! Use `pin_read()` with this board, not `pin_get()`

# useful errors for manifest problems

    Code
      board_url("https://not_real_url.posit.co")
    Condition
      Error in `board_url()`:
      ! Error requesting manifest-file from URL <https://not_real_url.posit.co>:
        Received HTTP code 502 from proxy after CONNECT
    Code
      board_url(github_raw("rstudio/pins-r/master/tests/testthat/pin-rds/"))
    Condition
      Error in `board_url()`:
      ! Error requesting manifest-file from URL <https://raw.githubusercontent.com/rstudio/pins-r/master/tests/testthat/pin-rds/pins.txt>:
        Not Found (HTTP 404).
    Code
      board_url(github_raw(
        "ijlyttle/pinsManifest/main/tests/testthat/pins/mtcars-csv/20220811T155805Z-48c73/mtcars-csv.csv"))
    Condition
      Error in `board_url()`:
      ! Error parsing manifest-file at URL <https://raw.githubusercontent.com/ijlyttle/pinsManifest/main/tests/testthat/pins/mtcars-csv/20220811T155805Z-48c73/mtcars-csv.csv>:
        Parser error: did not find expected <document start> at line 1, column 6
      i Manifest file must be text and parsable as YAML.
    Code
      board_url(3)
    Condition
      Error in `board_url()`:
      ! `urls` must resolve to either:
      * unnamed character scalar, i.e. a URL
      * named character vector
      * named list, where all elements are character scalars or vectors
      i `urls` is a "numeric".
    Code
      board_url(list("a", "b"))
    Condition
      Error in `board_url()`:
      ! `urls` must resolve to either:
      * unnamed character scalar, i.e. a URL
      * named character vector
      * named list, where all elements are character scalars or vectors
      i `urls` resolves to a list:
      i - named: FALSE
      i - all values character: TRUE
    Code
      board_url(list(a = "a", b = 2))
    Condition
      Error in `board_url()`:
      ! `urls` must resolve to either:
      * unnamed character scalar, i.e. a URL
      * named character vector
      * named list, where all elements are character scalars or vectors
      i `urls` resolves to a list:
      i - named: TRUE
      i - all values character: FALSE
    Code
      board_url(c("a", "b"))
    Condition
      Error in `board_url()`:
      ! `urls` must resolve to either:
      * unnamed character scalar, i.e. a URL
      * named character vector
      * named list, where all elements are character scalars or vectors
      i `urls` resolves to a character vector, but is unnamed.

