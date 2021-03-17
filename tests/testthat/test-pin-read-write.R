test_that("pin_write() noisily generates name", {
  expect_snapshot({
    b <- board_temp()
    pin_write(b, mtcars)
  })
})
