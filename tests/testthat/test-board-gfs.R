context("board gfs")

test_gfs_bucket <- Sys.getenv("TEST_GOOGLE_BUCKET", "")

test_that("board contains proper gfs headers", {
  headers <- names(gfs_headers(list(token = "abc"), "PUT", "x", "files/hello.txt")$headers)

  expect_true("Content-Type" %in% headers)
  expect_true("Authorization" %in% headers)
})

if (nchar(test_gfs_bucket) > 0) {
  if ("gfs" %in% board_list())
    board_deregister("gfs")

  board_register("gfs",
                 bucket = test_gfs_bucket,
                 cache = tempfile())
}

if (test_board_is_registered("gfs")) {
  board_test("gfs")
} else {
  test_that("can't register gfs board", {
    skip("failed to register gfs board")
  })
}
