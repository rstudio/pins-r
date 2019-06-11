context("pin local")

test_that("can pin() remote URL", {
  rstudio_page <- readLines(pin("https://www.google.com/"))

  expect_gt(length(rstudio_page), 0)
})
