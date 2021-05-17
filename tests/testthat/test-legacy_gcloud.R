test_that("board contains proper gcloud headers", {
  headers <- names(gcloud_headers(list(token = "abc"), "PUT", "x", "files/hello.txt")$headers)

  expect_true("Content-Type" %in% headers)
  expect_true("Authorization" %in% headers)
})

# Live API ---------------------------------------------------------------------

if (!has_envvars("TEST_GOOGLE_BUCKET")) {
  skip("requires TEST_GOOGLE_BUCKET env var")
}

board <- legacy_gcloud(
  bucket = Sys.getenv("TEST_GOOGLE_BUCKET"),
  cache = tempfile()
)
board_test(board, suite = "default")

board <- legacy_gcloud(
  bucket = Sys.getenv("TEST_GOOGLE_BUCKET"),
  versions = TRUE,
  cache = tempfile()
)
board_test(board, suite = "versions")
