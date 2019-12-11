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

test_that("can save data frame with different types", {
  test_df <- data.frame(raw =  charToRaw("asdas"),
                        posix = as.POSIXlt(Sys.time(), "EST"),
                        date = as.Date(35981, origin = "1899-12-30"),
                        integer = 1L,
                        numeric = 1,
                        booleaen = TRUE)

  pins_safe_csv(test_df, "test_df.csv")

  expect_true(file.exists("test_df.csv"))

  unlink("test_df.csv")
})

test_that("can pin complex data frame", {
  complex_df <- readRDS("files/complex-df.rds")

  pin(complex_df, "complex-df")

  succeed()
})
