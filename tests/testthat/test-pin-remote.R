context("pin local")

test_that("can pin() remote URL", {
  google_page <- readLines(pin("https://www.google.com/"), warn = FALSE)

  expect_gt(length(google_page), 0)
})

test_that("can pin() remote CSV with URL", {
  retail_sales <- read.csv(pin("https://raw.githubusercontent.com/facebook/prophet/master/examples/example_retail_sales.csv"))

  expect_gt(nrow(retail_sales), 10)
})

test_that("can pin() remote CSV with URL and name", {
  retail_sales <- read.csv(pin("http://www.fhfa.gov/DataTools/Downloads/Documents/HPI/HPI_master.csv", name = "named_hpi_master"))

  expect_gt(nrow(retail_sales), 10)
})

