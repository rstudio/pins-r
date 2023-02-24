skip_if_not_installed("mime")
skip_if_not_installed("openssl")

test_that("board contains proper azure headers", {
  mock_board <- list(key = jsonlite::base64_enc(as.raw(1:3)), url = "https://foo.com")
  headers <- names(azure_headers(mock_board, "PUT", "x", "files/hello.txt")$headers)

  expect_true("x-ms-date" %in% headers)
  expect_true("x-ms-version" %in% headers)
  expect_true("x-ms-blob-type" %in% headers)
  expect_true("Authorization" %in% headers)
})

# Live API ---------------------------------------------------------------------

skip_if_missing_envvars(
  tests = "legacy_azure()",
  envvars = c("TEST_AZURE_CONTAINER", "TEST_AZURE_ACCOUNT", "TEST_AZURE_KEY")
)

board <- legacy_azure(
  container = Sys.getenv("TEST_AZURE_CONTAINER"),
  account = Sys.getenv("TEST_AZURE_ACCOUNT"),
  key = Sys.getenv("TEST_AZURE_KEY"),
  cache = tempfile()
)
board_test(board, suite = "default")

board <- legacy_azure(
  name = "test-azure-2",
  container = Sys.getenv("TEST_AZURE_CONTAINER"),
  account = Sys.getenv("TEST_AZURE_ACCOUNT"),
  key = Sys.getenv("TEST_AZURE_KEY"),
  versions = TRUE,
  cache = tempfile()
)
board_test(board, suite = "versions")
