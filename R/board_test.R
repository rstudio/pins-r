test_dependencies <- function() {
  if (!"testthat" %in% installed.packages()) stop("Package 'testthat' needs to be installed to test boards.")

  list(
    test_that = get("test_that", envir = asNamespace("testthat")),
    expect_true = get("expect_true", envir = asNamespace("testthat")),
    expect_equal = get("expect_equal", envir = asNamespace("testthat"))
  )
}

#' Test Board
#'
#' Tests a particular board, useful only when creating new boards.
#'
#' @param name The name of the board to test
#' @param exclude Names of tests to exclude form test
#'
#' @keywords internal
#' @export
board_test <- function(board, exclude = list()) {
  deps <- test_dependencies()

  text_file <- dir(getwd(), recursive = TRUE, pattern = "hello.txt", full.names = TRUE)

  deps$test_that(paste("can pin() file to", board, "board"), {
    cached_path <- pin(text_file, "afile", board = board)

    deps$expect_true(is.character(cached_path))

    deps$expect_equal(readLines(cached_path), "hello world")
  })

  deps$test_that(paste("can pin_get() a pin from", board, "board"), {
    pin(text_file, "afile")

    cached_path <- pin_get("afile", board = board)

    deps$expect_true(is.character(cached_path))

    deps$expect_equal(readLines(cached_path), "hello world")
  })

  deps$test_that("can pin_find() the pin in any board", {
    results <- pin_find("afile")

    deps$expect_true(any(grepl("afile$", results$name)))
  })

  deps$test_that(paste("can pin_find() in", board, "board"), {
    results <- pin_find("afile", board = board)

    deps$expect_true(grepl("afile$", results$name))
  })

  deps$test_that(paste("can pin_remove() from", board, "board"), {
    if ("remove" %in% exclude) skip("This test is in the excluded list")
    pin(text_file, "afile")

    result <- pin_remove("afile", board = board)

    deps$expect_equal(result, NULL)
  })
}
