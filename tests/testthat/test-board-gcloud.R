context("board gcloud")

test_that("board contains proper gcloud headers", {
  headers <- names(gcloud_headers(list(token = "abc"), "PUT", "x", "files/hello.txt")$headers)

  expect_true("Content-Type" %in% headers)
  expect_true("Authorization" %in% headers)
})

# Live API ---------------------------------------------------------------------

if (!has_envvars("TEST_GOOGLE_BUCKET")) {
  skip("requires TEST_GOOGLE_BUCKET env var")
}

board_register_gcloud(
  name = "test-gcloud-1",
  bucket = Sys.getenv("TEST_GOOGLE_BUCKET"),
  cache = tempfile()
)
withr::defer(board_deregister("test-gcloud-1"))
board_register_gcloud(
  name = "test-gcloud-1",
  bucket = Sys.getenv("TEST_GOOGLE_BUCKET"),
  versions = TRUE,
  cache = tempfile()
)
withr::defer(board_deregister("test-gcloud-2"))

board_test("test-gcloud-1", suite = "default")
board_test("test-gcloud-2", suite = "versions")
