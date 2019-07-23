context("pin local")

test_that("can pin() remote URL", {
  google_page <- readLines(pin("https://www.google.com/"), warn = FALSE)

  expect_gt(length(google_page), 0)
})
