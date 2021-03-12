test_that("can maintain class when using pin(I())", {
  skip_if_not_installed("data.table")

  smalldat <- data.table::data.table(
    group1 = rep(1:2, each = 5),
    group2 = rep(c('a','b'), times = 5),
    x = rnorm(10)
  )

  smalldat_roundtrip <- pin(smalldat)

  expect_true(identical(class(smalldat_roundtrip), class(smalldat)))
  expect_equal(smalldat_roundtrip, smalldat)
})
