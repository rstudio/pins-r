context("board azure")

test_azure_container <- Sys.getenv("TEST_AZURE_CONTAINER", "")
test_azure_account <- Sys.getenv("TEST_AZURE_ACCOUNT", "")
test_azure_key <- Sys.getenv("TEST_AZURE_KEY", "")

test_that("board contains proper azure headers", {
  mock_board <- list(key = base64enc::base64encode(as.raw(1:3)), url = "https://foo.com")
  headers <- names(azure_headers(mock_board, "PUT", "x", "files/hello.txt")$headers)

  expect_true("x-ms-date" %in% headers)
  expect_true("x-ms-version" %in% headers)
  expect_true("x-ms-blob-type" %in% headers)
  expect_true("Authorization" %in% headers)
})

if (nchar(test_azure_container) > 0) {
  if ("azure" %in% board_list())
    board_deregister("azure")

  board_register("azure",
                 container = test_azure_container,
                 account = test_azure_account,
                 key = test_azure_key,
                 cache = tempfile())
}

if (test_board_is_registered("azure")) {
  board_test("azure")
} else {
  test_that("can't register azure board", {
    skip("failed to register azure board")
  })
}
