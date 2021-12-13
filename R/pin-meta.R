#' Retrieve metadata for a pin
#'
#' @description
#' Pin metadata comes from three sources:
#'
#' * Standard metadata added by `pin_upload()`/`pin_write()`. This includes:
#'   * `$name` - the pin's name.
#'   * `$file` - names of files stored in the pin.
#'   * `$file_size` - size of each file.
#'   * `$pin_hash` - hash of pin contents.
#'   * `$type` - type of pin, "rds", "csv", etc
#'   * `$title` - pin title
#'   * `$description` - pin description
#'   * `$created` - date this (version of the pin) was created
#'   * `$api_version` - API version used by pin
#'
#' * Metadata supplied by the user, stored in `$user`. This is untouched
#'   from what is supplied in [pin_write()]/[pin_upload()] except for being
#'   converted to and from to YAML.
#'
#' * Local metadata generated when caching the pin, stored in `$local`.
#'   This includes information like the version of the pin, and the path
#'   its local cache.
#'
#' @inheritParams pin_read
#' @returns A list.
#' @export
#' @examples
#' b <- board_temp()
#' b %>% pin_write(head(mtcars), "mtcars", metadata = list("Hadley" = TRUE))
#'
#' # Get the pin
#' b %>% pin_read("mtcars")
#' # Get its metadata
#' b %>% pin_meta("mtcars")
#' # Get path to underlying data
#' b %>% pin_download("mtcars")
pin_meta <- function(board, name, version = NULL, ...) {
  check_board(board, "pin_meta()", "pin_info()")
  UseMethod("pin_meta")
}

multi_meta <- function(board, names) {
  meta <- map(names, pin_meta, board = board)

  if (length(names) == 0) {
    tibble::tibble(
      name = character(),
      type = character(),
      title = character(),
      created = .POSIXct(double()),
      file_size = fs::fs_bytes(),
      meta = list()
    )
  } else {
    # Need defaults here because can be applied to pins metadata created with
    # api version 0.
    tibble::tibble(
      name = names,
      type = map_chr(meta, ~ .x$type %||% NA_character_),
      title = map_chr(meta, ~ .x$title %||% NA_character_),
      created = .POSIXct(map_dbl(meta, ~ .x$created %||% NA_real_)),
      file_size = fs::as_fs_bytes(map_dbl(meta, ~ sum(.x$file_size) %||% NA_real_)),
      meta = meta
    )
  }
}

# All pin_meta() methods should use `local_meta()` to ensure that results
# are stored in a consistent way
#
#' @noRd
#' @param dir Path to local cache directory
#' @param url Remote url to pin; used `pin_browser()`
local_meta <- function(x, name, dir, url = NULL, version = NULL, ...) {
  x$name <- name
  x$local <- list(
    dir = dir,
    url = url,
    version = version,
    ...
  )
  structure(x, class = "pins_meta")
}

test_api_meta <- function(board) {
  testthat::test_that("can round-trip pin metadata", {
    name <- local_pin(board, 1, title = "title", description = "desc", metadata = list(a = "a"))
    meta <- pin_meta(board, name)
    testthat::expect_equal(meta$name, name)
    testthat::expect_equal(meta$title, "title")
    testthat::expect_equal(meta$description, "desc")
    testthat::expect_equal(meta$user$a, "a")
  })

  testthat::test_that("can update pin metadata", {
    # RSC requires at least 3 characters
    name <- local_pin(board, 1, title = "xxx-a1", description = "xxx-a2")
    pin_write(board, 1, name, title = "xxx-b1", description = "xxx-b2")

    meta <- pin_meta(board, name)
    testthat::expect_equal(meta$title, "xxx-b1")
    testthat::expect_equal(meta$description, "xxx-b2")
  })

  testthat::test_that("pin_meta fails cleanly if pin is missing", {
    testthat::expect_error(
      pin_read(board, "DOES-NOT-EXIST"),
      class = "pins_pin_missing"
    )
  })

  testthat::test_that("pin_meta() returns pins_meta object", {
    name <- local_pin(board, 1)

    meta <- pin_meta(board, name)
    testthat::expect_s3_class(meta, "pins_meta")

    testthat::expect_vector(meta$file, character())
    testthat::expect_s3_class(meta$file_size, "fs_bytes")
    testthat::expect_vector(meta$pin_hash, character(), 1)
    testthat::expect_true(meta$type %in% object_types)
    testthat::expect_vector(meta$title, character(), 1)
    testthat::expect_vector(meta$created, .POSIXct(double()), 1)
    testthat::expect_vector(meta$api_version, double(), 1)

    testthat::expect_vector(meta$user, list())
    testthat::expect_vector(meta$local, list())
  })
}

#' @export
print.pins_meta <- function(x, ...) {
  utils::str(unclass(x))
  invisible(x)
}

check_pin_version <- function(board, name, version) {
  if (is.null(version)) {
    last(pin_versions(board, name)$version) %||% abort("No versions found")
  } else if (is_string(version)) {
    # TODO: provide pin_version_exists() so this can return informative error
    version
  } else {
    abort("`version` must be a string or `NULL`")
  }
}
