context("pin dataframe")

test_that("can pin() data frame", {
  roundtrip <- pin(iris, "iris")

  expect_equal(as.data.frame(roundtrip), iris)
  expect_equal(as.data.frame(pin_get("iris")), iris)
})

test_that("can sanitize data frame names", {
  name <- "___sdf ds32___42342     dsf dsf dsfds____"
  expect_equal(
    pin_default_name(name, board_default()),
    "sdf-ds32-42342-dsf-dsf-dsfds"
  )
})
