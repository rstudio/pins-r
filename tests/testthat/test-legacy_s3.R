test_that("board contains proper s3 headers", {
  headers <- names(s3_headers(list(), "PUT", "x")$headers)

  expect_true("Host" %in% headers)
  expect_true("Date" %in% headers)
  expect_true("Content-Type" %in% headers)
  expect_true("Authorization" %in% headers)
})

# Live API ---------------------------------------------------------------------

skip_if_missing_envvars(
  tests = "legacy_s3()",
  envvars = c("TEST_AWS_BUCKET", "TEST_AWS_KEY", "TEST_AWS_SECRET", "TEST_AWS_REGION")
)

board <- legacy_s3(
  bucket = Sys.getenv("TEST_AWS_BUCKET"),
  key = Sys.getenv("TEST_AWS_KEY"),
  secret = Sys.getenv("TEST_AWS_SECRET"),
  cache = tempfile(),
  region = Sys.getenv("TEST_AWS_REGION")
)
board_test(board, suite = "default")

board <- legacy_s3(
  name = "test-s3-2",
  bucket = Sys.getenv("TEST_AWS_BUCKET"),
  key = Sys.getenv("TEST_AWS_KEY"),
  secret = Sys.getenv("TEST_AWS_SECRET"),
  versions = TRUE,
  cache = tempfile(),
  region = Sys.getenv("TEST_AWS_REGION")
)
board_test(board, suite = "versions")
