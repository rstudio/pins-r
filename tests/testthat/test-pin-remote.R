context("pin local")

test_that("can pin() remote URL", {
  rstudio_page <- readLines(pin("https://www.google.com/"), warn = FALSE)

  expect_gt(length(rstudio_page), 0)
})
