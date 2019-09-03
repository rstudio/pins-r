context("pin zzz check")

test_that("tests do not create local dirs", {
  expect_true(!dir.exists("~/.pins"))
})
