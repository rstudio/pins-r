context("pin local")

test_that("can pin() remote URL", {
  skip_on_cran()

  google_page <- readLines(pin("https://www.google.com/"), warn = FALSE)

  expect_gt(length(google_page), 0)
})

test_that("can pin() remote CSV with URL", {
  skip_on_cran()

  retail_sales <- read.csv(pin("https://raw.githubusercontent.com/facebook/prophet/master/examples/example_retail_sales.csv"))

  expect_gt(nrow(retail_sales), 10)
})

test_that("can pin() remote CSV with URL and name", {
  skip_on_cran()

  retail_sales <- read.csv(pin("http://www.fhfa.gov/DataTools/Downloads/Documents/HPI/HPI_master.csv", name = "named_hpi_master"))

  expect_gt(nrow(retail_sales), 10)
})

test_that("can pin() remote CSV with signature", {
  skip_on_cran()

  retail_sales <- read.csv(pin("http://www.fhfa.gov/DataTools/Downloads/Documents/HPI/HPI_master.csv", signature = "264ea461b5f73a50e5935ab0ba2fb062fbef642d"))

  expect_gt(nrow(retail_sales), 10)
})

test_that("fail to pin() remote CSV with bad signature", {
  skip_on_cran()

  expect_error({
    pin("http://www.fhfa.gov/DataTools/Downloads/Documents/HPI/HPI_master.csv", signature = "aaaaa461b5f73a50e5935ab0ba2fb062fbef642d")
  })
})

