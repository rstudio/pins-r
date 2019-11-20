test_dependencies <- function() {
  if (length(find.package("testthat", quiet = TRUE)) == 0)
    stop("Package 'testthat' needs to be installed to test boards.")

  list(
    test_that = get("test_that", envir = asNamespace("testthat")),
    expect_true = get("expect_true", envir = asNamespace("testthat")),
    expect_equal = get("expect_equal", envir = asNamespace("testthat")),
    skip = get("skip", envir = asNamespace("testthat")),
    fail = get("fail", envir = asNamespace("testthat"))
  )
}

#' Test Board
#'
#' Tests a particular board, useful only when creating new boards.
#'
#' @param name The name of the board to test.
#' @param exclude Names of tests to exclude form test.
#' @param destination The text to describe where the board is.
#'
#' @keywords internal
#' @export
board_test <- function(board, exclude = list(), destination = paste(board, "board")) {
  deps <- test_dependencies()

  text_file <- dir(getwd(), recursive = TRUE, pattern = "hello.txt", full.names = TRUE)
  pin_name <- paste0("afile", round(stats::runif(1, 1, 1000)))
  dataset_name <- paste0("adataset", round(stats::runif(1, 1, 1000)))

  old_progress <- options(pins.progress = FALSE)
  on.exit(options(pins.progress = old_progress))

  deps$test_that(paste("can pin() file to", destination), {
    cached_path <- pin(text_file, pin_name, board = board)

    deps$expect_true(is.character(cached_path))

    deps$expect_equal(readLines(cached_path), "hello world")
  })

  deps$test_that(paste("can pin() data frame to", destination), {
    iris <- get("iris", envir = asNamespace("datasets"))

    dataset <- pin(iris, dataset_name, board = board)

    deps$expect_true(is.data.frame(dataset))
  })

  deps$test_that(paste("can pin_get() a pin from", destination), {
    cached_path <- pin_get(pin_name, board = board)

    deps$expect_true(is.character(cached_path))

    deps$expect_equal(readLines(cached_path), "hello world")
  })

  deps$test_that("can pin_find() the pin in any board", {
    results <- pin_find(pin_name)

    deps$expect_true(any(grepl(pin_name, results$name)))
  })

  deps$test_that(paste("can pin_find() in", destination), {
    results <- pin_find(pin_name, board = board)

    deps$expect_true(grepl(pin_name, results$name))
  })

  deps$test_that(paste("can pin_info() in", destination), {
    info <- pin_info(pin_name, board = board)

    deps$expect_true(grepl(pin_name, info$name))
  })

  deps$test_that(paste("can pin_remove() file from", destination), {
    if ("remove" %in% exclude) deps$skip("This test is in the excluded list")

    result <- pin_remove(pin_name, board = board)
    deps$expect_equal(result, NULL)

    results <- pin_find(name = pin_name, board = board)
    if (nrow(results) > 0)
      deps$fail(paste0("Pin '", paste(results$name, collapse = ","), "' still exists after removal."))
  })

  deps$test_that(paste("can pin_remove() dataset from", destination), {
    if ("remove" %in% exclude) deps$skip("This test is in the excluded list")

    result <- pin_remove(dataset_name, board = board)
    deps$expect_equal(result, NULL)

    results <- pin_find(name = dataset_name, board = board)
    if (nrow(results) > 0)
      deps$fail(paste0("Pin '", paste(results$name, collapse = ","), "' still exists after removal."))
  })
}
