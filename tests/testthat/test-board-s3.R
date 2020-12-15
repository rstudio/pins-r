context("board s3")

test_s3_bucket <- Sys.getenv("TEST_AWS_BUCKET", "")
test_s3_key <- Sys.getenv("TEST_AWS_KEY", "")
test_s3_secret <- Sys.getenv("TEST_AWS_SECRET", "")
test_s3_region <- Sys.getenv("TEST_AWS_REGION", "")
if (identical(test_s3_region, "")) test_s3_region <- NULL

test_that("board contains proper s3 headers", {
  headers <- names(s3_headers(list(), "PUT", "x")$headers)

  expect_true("Host" %in% headers)
  expect_true("Date" %in% headers)
  expect_true("Content-Type" %in% headers)
  expect_true("Authorization" %in% headers)
})

test_s3_suite <- function(suite, versions = NULL) {
  if (nchar(test_s3_bucket) > 0) {
    if ("s3" %in% board_list())
      board_deregister("s3")

    board_register("s3",
                   bucket = test_s3_bucket,
                   key = test_s3_key,
                   secret = test_s3_secret,
                   versions = versions,
                   cache = tempfile(),
                   region = test_s3_region)
  }

  if (test_board_is_registered("s3")) {
    board_test("s3", suite = suite)
  } else {
    test_that("can't register s3 board", {
      skip("failed to register s3 board")
    })
  }
}

test_s3_suite("default")
test_s3_suite("versions", versions = TRUE)
