local_pin <- function(board, value, ..., env = parent.frame()) {
  name <- random_pin_name()

  pin_write(board, value, name, ...)
  withr::defer(pin_delete(board, name), env)

  name
}

random_pin_name <- function() {
  rand <- sample(c(letters, LETTERS, 0:9), 10, replace = TRUE)
  paste0("test-", paste(rand, collapse = ""))
}

test_board_api <- function(board) {
  # First, ensure that enough of the API works that we can use local_pin
  # If this doesn't work, the code will error, and none of the tests will be
  # run
  name <- random_pin_name()
  pin_write(board, 1, name)
  pin_delete(board, name)

  # These tests should be ordered in roughly the order you'd implement
  # when creating a new board - probably should live next to the generic
  # so that the function and it's interface are defined nearby.
  testthat::test_that("pin_exists() returns TRUE for pin that exists and FALSE otherwise", {
    name <- local_pin(board, 1)
    testthat::expect_true(pin_exists(board, name))
    testthat::expect_false(pin_exists(board, "DOES-NOT-EXIST"))
  })

  if (!identical(pin_list(board), NA)) {
    testthat::test_that("pin_list() includes newly created pin", {
      name <- random_pin_name()
      pin_write(board, 1, name)
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
}

test_version_api <- function(board) {
  # assume that test_board_api() has passed

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
pin_abort_version_missing <- function(version) {
  abort(glue("Can't find version '{version}'"), class = "pins_pin_version_missing")
}


