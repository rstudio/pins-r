context("board s3")

test_s3_bucket <- Sys.getenv("TEST_AWS_BUCKET", "")
test_s3_key <- Sys.getenv("TEST_AWS_KEY", "")
test_s3_secret <- Sys.getenv("TEST_AWS_SECRET", "")

test_that("board contains proper s3 headers", {
  headers <- names(s3_headers(list(), "PUT", "x")$headers)

  expect_true("Host" %in% headers)
  expect_true("Date" %in% headers)
  expect_true("Content-Type" %in% headers)
  expect_true("Authorization" %in% headers)
})

if (nchar(test_s3_bucket) > 0) {
  if ("s3" %in% board_list())
    board_deregister("s3")

  board_register("s3",
                 bucket = test_s3_bucket,
                 key = test_s3_key,
                 secret = test_s3_secret,
                 cache = tempfile())
}

if (test_board_is_registered("s3")) {
  board_test("s3")
} else {
  test_that("can't register s3 board", {
    skip("failed to register s3 board")
  })
}
