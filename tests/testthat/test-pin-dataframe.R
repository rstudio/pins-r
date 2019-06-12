context("pin dataframe")

test_that("can pin() data frame", {
  roundtrip <- pin(iris, "iris")

  expect_equal(as.data.frame(roundtrip), iris)
  expect_equal(as.data.frame(pin_get("iris")), iris)
})
