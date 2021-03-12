test_that("board contains proper s3 headers", {
  headers <- names(s3_headers(list(), "PUT", "x")$headers)

  expect_true("Host" %in% headers)
  expect_true("Date" %in% headers)
  expect_true("Content-Type" %in% headers)
  expect_true("Authorization" %in% headers)
})

# Live API ---------------------------------------------------------------------

if (!has_envvars(c("TEST_AWS_BUCKET", "TEST_AWS_KEY", "TEST_AWS_SECRET", "TEST_AWS_REGION "))) {
  skip("requires env vars TEST_AWS_BUCKET, TEST_AWS_KEY, TEST_AWS_SECRET, TEST_AWS_REGION")
}

board_register_s3(
  name = "test-s3-1",
  bucket = Sys.getenv("TEST_AWS_BUCKET"),
  key = Sys.getenv("TEST_AWS_KEY"),
  secret = Sys.getenv("TEST_AWS_SECRET"),
  cache = tempfile(),
  region = Sys.getenv("TEST_AWS_REGION")
)
withr::defer(board_deregister("test-s3-1"))
board_register_s3(
  name = "test-s3-2",
  bucket = Sys.getenv("TEST_AWS_BUCKET"),
  key = Sys.getenv("TEST_AWS_KEY"),
  secret = Sys.getenv("TEST_AWS_SECRET"),
  versions = TRUE,
  cache = tempfile(),
  region = Sys.getenv("TEST_AWS_REGION")
)
withr::defer(board_deregister("test-s3-2"))

board_test("test-rsconnect-1", suite = "default")
board_test("test-rsconnect-2", suite = "versions")
