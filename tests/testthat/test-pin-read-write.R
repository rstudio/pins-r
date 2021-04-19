test_that("can round trip all types", {
  withr::local_options(pins.quiet = TRUE)
  board <- board_temp()

  # Data frames
  df <- data.frame(x = 1:10)
  pin_write(board, df, "df-1", type = "rds")
  expect_equal(pin_read(board, "df-1"), df)

  pin_write(board, df, "df-2", type = "arrow")
  expect_equal(pin_read(board, "df-2"), df)

  pin_write(board, df, "df-3", type = "csv")
  expect_equal(pin_read(board, "df-3"), df)

  # List
  x <- list(a = 1:5, b = 1:10)
  pin_write(board, x, "x-1", type = "json")
  expect_equal(pin_read(board, "x-1"), x)
})

test_that("useful errors on bad inputs", {
  board <- board_temp()

  expect_snapshot(error = TRUE, {
    pin_write(mtcars)
    pin_write(board, mtcars, name = 1:10)
    pin_write(board, mtcars, name = "x/y")
    pin_write(board, mtcars, name = "mtcars", type = "froopy-loops")
  })
})

test_that("guess_type() works as expected", {
  expect_equal(guess_type(mtcars), "rds")
  expect_equal(guess_type(lm(mpg ~ disp, data = mtcars)), "rds")
  expect_equal(guess_type(list(x = 1)), "json")
})

test_that("pin_write() noisily generates name and type", {
  expect_snapshot({
    b <- board_temp()
    pin_write(b, mtcars)
  })
})

test_that("can request specific hash", {
  expect_snapshot(error = TRUE, {
    b <- board_temp()
    pin_write(b, mtcars, name = "mtcars", type = "rds")
    pin_read(b, "mtcars", hash = "ABCD")
  })
})
