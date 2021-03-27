test_that("non-existent file returns default data", {
  expect_equal(read_meta(tempfile()), list(api_version = 1))
})

test_that("newer version triggers error", {
  path <- withr::local_tempdir()

  write_meta(list(api_version = 2), path)
  expect_snapshot_error(read_meta(path))
})

test_that("older version adds missing pieces", {
  path <- withr::local_tempdir()

  write_meta(list(), path)
  expect_equal(read_meta(path), list(api_verison = 0, type = "files"))
})
