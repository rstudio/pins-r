context("pin zzz check")

test_that("tests do not create local dirs", {
  skip_on_cran()

  pin_folders <- dir("~/.pins")

  if (!identical(pin_folders, character(0)))
    stop("Found local folders: ", paste(pin_folders, collapse = ", "))

  succeed()
})
