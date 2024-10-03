#' Test Board
#'
#' `r lifecycle::badge('deprecated')`
#'
#' Tests a particular board, useful only when creating new boards.
#'
#' @param board The name of the board to test.
#' @param exclude Names of tests to exclude form test.
#' @param suite The test suite to run, currently only `"versions"` or
#'   `default` are supported.
#'
#' @keywords internal
#' @export
board_test <- function(board,
                       exclude = list(),
                       suite = c("default", "versions")) {
  suite <- switch(arg_match(suite),
    default = board_test_default,
    versions = board_test_versions
  )
  suite(board, exclude)
}

board_test_default <- function(board, exclude) {
  name <- paste0("board_", board$name)

  pin_name <- random_pin_name()
  dataset_name <- random_pin_name()

  old_progress <- options(pins.progress = FALSE)
  on.exit(options(pins.progress = old_progress))

  testthat::test_that(paste("can pin() file to", name), {
    path <- testthat::test_path("files/hello.txt")
    cached_path <- pin(path, pin_name, board = board)

    testthat::expect_true(is.character(cached_path))
    testthat::expect_equal(readLines(cached_path), "hello world")
  })

  testthat::test_that(paste("can pin() data frame to", name), {
    dataset <- pin(datasets::iris, dataset_name, board = board)

    testthat::expect_true(is.data.frame(dataset))
  })

  testthat::test_that(paste("can pin_get() a pin from", name), {
    cached_path <- pin_get(pin_name, board = board)

    testthat::expect_true(is.character(cached_path))
    testthat::expect_equal(readLines(cached_path), "hello world")
  })

  testthat::test_that(paste("can pin_find() in", name), {
    results <- pin_find(pin_name, board = board)

    testthat::expect_true(any(grepl(pin_name, results$name)))
  })

  testthat::test_that(paste("can pin_info() in", name), {
    info <- pin_info(pin_name, board = board)
    testthat::expect_true(grepl(pin_name, info$name))
  })

  testthat::test_that(paste("can pin_remove() file from", name), {
    if ("remove" %in% exclude) testthat::succeed()

    result <- pin_remove(pin_name, board = board)
    testthat::expect_equal(result, NULL)

    results <- pin_find(name = pin_name, board = board)
    testthat::expect_equal(nrow(results), 0)
  })

  testthat::test_that(paste("can pin_remove() dataset from", name), {
    if ("remove" %in% exclude) testthat::succeed()

    result <- pin_remove(dataset_name, board = board)
    testthat::expect_equal(result, NULL)

    results <- pin_find(name = dataset_name, board = board)
    testthat::expect_equal(nrow(results), 0)
  })
}

board_test_versions <- function(board, exclude, name) {
  name <- paste0("board_", board$name)
  pin_name <- random_pin_name()

  old_progress <- options(pins.progress = FALSE)
  on.exit(options(pins.progress = old_progress))

  testthat::test_that(paste("can pin() and retrieve specific version", name), {
    version_a <- data.frame(nums = 1:3)
    version_b <- data.frame(nums = 11:13)

    pin(I(version_a), pin_name, board = board)
    pin(I(version_b), pin_name, board = board)

    versions <- pin_versions(pin_name, board = board)
    testthat::expect_gte(length(versions$version), 2)

    testthat::expect_equal(
      as.character(pin_get(pin_name, version = versions$version[length(versions$version)], board = board)),
      as.character(version_a)
    )

    testthat::expect_equal(
      as.character(pin_get(pin_name, version = versions$version[length(versions$version) - 1], board = board)),
      as.character(version_b)
    )
  })

  testthat::test_that(paste("can pin_remove() a pin with versions", name), {
    if ("remove" %in% exclude) testthat::skip("This test is in the excluded list")

    result <- pin_remove(pin_name, board = board)
    testthat::expect_equal(result, NULL)

    results <- pin_find(name = pin_name, board = board)
    if (nrow(results) > 0) {
      testthat::fail(paste0("Pin '", paste(results$name, collapse = ","), "' still exists after removal."))
    }
  })
}
