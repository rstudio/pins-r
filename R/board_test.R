#' Test Board
#'
#' Tests a particular board, useful only when creating new boards.
#'
#' @param name The name of the board to test.
#' @param exclude Names of tests to exclude form test.
#' @param suite The test suite to run, currently only `"versions"` or
#'   `default` are supported.
#' @param destination The text to describe where the board is.
#'
#' @keywords internal
#' @export
board_test <- function(board,
                       exclude = list(),
                       suite = "default",
                       destination = paste(board, "board")) {
  suites <- list(
    default = board_test_default,
    versions = board_test_versions
  )

  if (!suite %in% names(suites)) {
    stop("The test suite '", suite, "' is not available.")
  }

  suites[[suite]](board, exclude, destination)
}

board_test_default <- function(board, exclude, destination) {
  text_file <- dir(getwd(), recursive = TRUE, pattern = "hello.txt", full.names = TRUE)
  pin_name <- paste0("afile", round(stats::runif(1, 1, 1000)))
  dataset_name <- paste0("adataset", round(stats::runif(1, 1, 1000)))

  old_progress <- options(pins.progress = FALSE)
  on.exit(options(pins.progress = old_progress))

  testthat::test_that(paste("can pin() file to", destination), {
    cached_path <- pin(text_file, pin_name, board = board)

    testthat::expect_true(is.character(cached_path))

    testthat::expect_equal(readLines(cached_path), "hello world")
  })

  testthat::test_that(paste("can pin() data frame to", destination), {
    dataset <- pin(datasets::iris, dataset_name, board = board)

    testthat::expect_true(is.data.frame(dataset))
  })

  testthat::test_that(paste("can pin_get() a pin from", destination), {
    cached_path <- pin_get(pin_name, board = board)

    testthat::expect_true(is.character(cached_path))

    testthat::expect_equal(readLines(cached_path), "hello world")
  })

  testthat::test_that("can pin_find() the pin in any board", {
    testthat::expect_true(testthat::try_again(3, any(grepl(pin_name, pin_find(pin_name)$name))))
  })

  testthat::test_that(paste("can pin_find() in", destination), {
    results <- pin_find(pin_name, board = board)

    testthat::expect_true(grepl(pin_name, results$name))
  })

  testthat::test_that(paste("can pin_info() in", destination), {
    info <- pin_info(pin_name, board = board)

    testthat::expect_true(grepl(pin_name, info$name))
  })

  testthat::test_that(paste("can pin_remove() file from", destination), {
    if ("remove" %in% exclude) testthat::skip("This test is in the excluded list")

    result <- pin_remove(pin_name, board = board)
    testthat::expect_equal(result, NULL)

    results <- pin_find(name = pin_name, board = board)
    if (nrow(results) > 0)
      testthat::fail(paste0("Pin '", paste(results$name, collapse = ","), "' still exists after removal."))
  })

  testthat::test_that(paste("can pin_remove() dataset from", destination), {
    if ("remove" %in% exclude) testthat::skip("This test is in the excluded list")

    result <- pin_remove(dataset_name, board = board)
    testthat::expect_equal(result, NULL)

    results <- pin_find(name = dataset_name, board = board)
    if (nrow(results) > 0)
      testthat::fail(paste0("Pin '", paste(results$name, collapse = ","), "' still exists after removal."))
  })
}

board_test_versions <- function(board, exclude, destination) {
  pin_name <- paste0("aversion", round(stats::runif(1, 1, 1000)))

  old_progress <- options(pins.progress = FALSE)
  on.exit(options(pins.progress = old_progress))

  testthat::test_that(paste("can pin() and retrieve specific version", destination), {
    version_a <- data.frame(nums = 1:3)
    version_b <- data.frame(nums = 11:13)

    pin(I(version_a), pin_name, board = board)
    pin(I(version_b), pin_name, board = board)

    versions <- pin_versions(pin_name, board = board)
    testthat::expect_gte(length(versions$version), 2)

    testthat::expect_equal(
      as.character(pin_get(pin_name, version = versions$version[length(versions$version)], board = board)),
      as.character(version_a))

    testthat::expect_equal(
      as.character(pin_get(pin_name, version = versions$version[length(versions$version)-1], board = board)),
      as.character(version_b))
  })

  testthat::test_that(paste("can pin_remove() a pin with versions", destination), {
    if ("remove" %in% exclude) testthat::skip("This test is in the excluded list")

    result <- pin_remove(pin_name, board = board)
    testthat::expect_equal(result, NULL)

    results <- pin_find(name = pin_name, board = board)
    if (nrow(results) > 0)
      testthat::fail(paste0("Pin '", paste(results$name, collapse = ","), "' still exists after removal."))
  })
}
