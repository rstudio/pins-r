context("pin identity")

test_that("can maintain class when using pin(I())", {
  cars <- mtcars
  class(cars) <- c("foo", class(cars))

  cars_roundtrip <- pin(I(cars))

  expect_true(identical(cars, cars_roundtrip))
})
