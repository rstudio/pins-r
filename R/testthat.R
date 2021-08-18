local_pin <- function(board, value, ..., env = parent.frame()) {
  name <- pin_write(board, value, random_pin_name(), ...)
  withr::defer(pin_delete(board, name), env)

  name
}

random_pin_name <- function() {
  rand <- sample(c(letters, LETTERS, 0:9), 10, replace = TRUE)
  paste0("test-", paste(rand, collapse = ""))
}

# These functions are used to test families of invariants that apply to the
# behaviour or multiple generics. They are broken up into rough familes to
# guide the process of implementing a board, and making it a little easier
# to debug when things go wrong.
#
# These will eventually be exported so folks outside of pins can more easily
# test their own boards. But need to first fully think through consequences -
# what happens if I add new invariant that causes existing CRAN packages
# to fail R CMD check? How does the release process work?

test_api_basic <- function(board) {
  # First, ensure that enough of the API works that we can use local_pin
  # If this doesn't work, the code will error, and none of the tests will be
  # run
  name <- pin_write(board, 1, random_pin_name())
  pin_delete(board, name)

  testthat::test_that("pin_exists() returns TRUE for pin that exists and FALSE otherwise", {
    name <- local_pin(board, 1)
    testthat::expect_true(pin_exists(board, name))
    testthat::expect_false(pin_exists(board, "DOES-NOT-EXIST"))
  })

  if (!identical(pin_list(board), NA)) {
    testthat::test_that("pin_list() includes newly created pin", {
      name <- pin_write(board, 1, random_pin_name())
      testthat::expect_true(name %in% pin_list(board))

      pin_delete(board, name)
      testthat::expect_false(name %in% pin_list(board))
    })
  }

  testthat::test_that("can round-trip pin metadata", {
    name <- local_pin(board, 1, desc = "desc", metadata = list(a = "a"))
    meta <- pin_meta(board, name)
    expect_equal(meta$description, "desc")
    expect_equal(meta$user$a, "a")
  })

  testthat::test_that("can round-trip pin data", {
    name <- local_pin(board, 1)
    testthat::expect_equal(pin_read(board, name), 1)
  })

  testthat::test_that("pin_meta fails cleanly if pin is missing", {
    testthat::expect_error(
      pin_read(board, "DOES-NOT-EXIST"),
      class = "pins_pin_missing"
    )
  })
}

test_api_versioning <- function(board) {
  # assume that test_api_basic() has passed

  testthat::test_that("pin_versions() returns one row per version", {
    name <- local_pin(board, 1)
    testthat::expect_equal(nrow(pin_versions(board, name)), 1)
    pin_write(board, 2, name)
    testthat::expect_equal(nrow(pin_versions(board, name)), 2)
    pin_write(board, 3, name)
    testthat::expect_equal(nrow(pin_versions(board, name)), 3)
  })

  test_that("pin_read() returns latest version", {
    name <- local_pin(board, 1)
    pin_write(board, 2, name)
    pin_write(board, 3, name)

    testthat::expect_equal(pin_read(board, name), 3)
  })

  test_that("can retrieve data from previous version", {
    name <- local_pin(board, 1)
    v1 <- pin_versions(board, name)$version[[1]]

    pin_write(board, 2, name)
    pin_write(board, 3, name)
    testthat::expect_equal(pin_read(board, name, version = v1), 1)
  })

  test_that("clear error for missing version", {
    name <- local_pin(board, 1)
    testthat::expect_error(
      pin_read(board, name, version = "DOES-NOT-EXIST"),
      class = "pins_pin_version_missing"
    )
  })

  test_that("unversioned write overwrites single previous version", {
    name <- local_pin(board, 1)
    pin_write(board, 2, name, versioned = FALSE)

    testthat::expect_equal(nrow(pin_versions(board, name)), 1)
    testthat::expect_equal(pin_read(board, name), 2)
  })

  test_that("unversioned write errors if multiple versions", {
    name <- local_pin(board, 1)
    pin_write(board, 2, name)

    testthat::expect_error(
      pin_write(board, 3, name, versioned = FALSE),
      class = "pins_pin_versioned"
    )
  })

}

# errors live here for now since they're closely bound to the tests

abort_pin_missing <- function(name) {
  abort(c(
    glue("Can't find pin called '{name}'"),
    i = "Use `pin_list()` to see all available pins in this board"
  ), class = "pins_pin_missing")
}

abort_pin_version_missing <- function(version) {
  abort(glue("Can't find version '{version}'"), class = "pins_pin_version_missing")
}

abort_pin_versioned <- function() {
  abort(c(
    "Pin is versioned, but you have requested a write without versions",
    i = "To un-version a pin, you must delete it"
  ), class = "pins_pin_versioned")
}
