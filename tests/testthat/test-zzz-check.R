context("pin zzz check")

test_that("tests do not create local dirs", {
  expect_equal(dir("~/.pins"), character(0))
})
