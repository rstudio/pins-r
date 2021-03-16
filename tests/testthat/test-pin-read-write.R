test_that("pin_write() noisily generates name", {
  expect_snapshot({
    b <- board_local(tempfile())
    pin_write(b, mtcars)
  })
})
