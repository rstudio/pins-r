context("pin data table")

test_that("can maintain class when using pin(I())", {
  if (length(find.package("data.table", quiet = TRUE)) == 0) skip("data.table not installed")
  data_table <- get0("data.table", envir = asNamespace("data.table"))

  smalldat <- data_table(group1 = rep(1:2, each = 5),
                         group2 = rep(c('a','b'), times = 5),
                         x = rnorm(10))


  smalldat_roundtrip <- pin(smalldat)

  expect_true(identical(class(smalldat_roundtrip), class(smalldat)))
  expect_equal(smalldat_roundtrip, smalldat)
})
