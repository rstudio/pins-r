context("board s3")

test_s3_bucket <- Sys.getenv("TEST_AWS_BUCKET", "")
test_s3_key <- Sys.getenv("TEST_AWS_KEY", "")
test_s3_secret <- Sys.getenv("TEST_AWS_SECRET", "")

test_that("can pin large resources in github releases", {
  expect_equal(format(Sys.time(), "%a, %d %b %Y %X %z"), "")
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
