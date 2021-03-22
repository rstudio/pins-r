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
