context("pin aaa check")

test_that("tests start without local dirs", {
  skip_on_cran()

  pin_folders <- dir(board_cache_path())

  if (!identical(pin_folders, character(0)))
    stop("Found local folders: ", paste(pin_folders, collapse = ", "))

  succeed()
})
