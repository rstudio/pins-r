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
