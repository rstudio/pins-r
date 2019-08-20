test_dependencies <- function() {
  if (length(find.package("testthat", quiet = TRUE)) == 0)
    stop("Package 'testthat' needs to be installed to test boards.")

  list(
    test_that = get("test_that", envir = asNamespace("testthat")),
    expect_true = get("expect_true", envir = asNamespace("testthat")),
    expect_equal = get("expect_equal", envir = asNamespace("testthat")),
    skip = get("skip", envir = asNamespace("testthat"))
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
  pin_name <- paste0("afile", round(stats::runif(1, 1, 1000)))
  dataset_name <- paste0("adataset", round(stats::runif(1, 1, 1000)))

  old_progress <- options(pins.progress = FALSE)
  on.exit(options(pins.progress = old_progress))

  deps$test_that(paste("can pin() file to", board, "board"), {
    cached_path <- pin(text_file, pin_name, board = board)

    deps$expect_true(is.character(cached_path))

    deps$expect_equal(readLines(cached_path), "hello world")
  })

  deps$test_that(paste("can pin() data frame to", board, "board"), {
    iris <- get("iris", envir = asNamespace("datasets"))

    dataset <- pin(iris, dataset_name, board = board)

    deps$expect_true(is.data.frame(dataset))
  })

  deps$test_that(paste("can pin_get() a pin from", board, "board"), {
    pin(text_file, pin_name)

    cached_path <- pin_get(pin_name, board = board)

    deps$expect_true(is.character(cached_path))

    deps$expect_equal(readLines(cached_path), "hello world")
  })

  deps$test_that("can pin_find() the pin in any board", {
    results <- pin_find(pin_name)

    deps$expect_true(any(grepl(pin_name, results$name)))
  })

  deps$test_that(paste("can pin_find() in", board, "board"), {
    results <- pin_find(pin_name, board = board)

    deps$expect_true(grepl(pin_name, results$name))
  })

  deps$test_that(paste("can pin_remove() from", board, "board"), {
    if ("remove" %in% exclude) deps$skip("This test is in the excluded list")

    result <- pin_remove(pin_name, board = board)
    deps$expect_equal(result, NULL)

    result <- pin_remove(dataset_name, board = board)
    deps$expect_equal(result, NULL)
  })
}
