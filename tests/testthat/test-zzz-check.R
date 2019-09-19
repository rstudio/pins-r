context("pin zzz check")

test_that("tests do not create local dirs", {
  skip_on_cran()
  test_local_files()
})
