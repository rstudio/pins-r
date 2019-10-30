context("board wasb")

test_wasb_container <- Sys.getenv("TEST_AZURE_CONTAINER", "")
test_wasb_account <- Sys.getenv("TEST_AZURE_ACCOUNT", "")
test_wasb_key <- Sys.getenv("TEST_AZURE_KEY", "")

test_that("board contains proper wasb headers", {
  mock_board <- list(key = base64enc::base64encode(as.raw(1:3)), url = "https://foo.com")
  headers <- names(wasb_headers(mock_board, "PUT", "x", "files/hello.txt")$headers)

  expect_true("x-ms-date" %in% headers)
  expect_true("x-ms-version" %in% headers)
  expect_true("x-ms-blob-type" %in% headers)
  expect_true("Authorization" %in% headers)
})

if (nchar(test_wasb_container) > 0) {
  if ("wasb" %in% board_list())
    board_deregister("wasb")

  board_register("wasb",
                 container = test_wasb_container,
                 account = test_wasb_account,
                 key = test_wasb_key,
                 cache = tempfile())
}

if (test_board_is_registered("wasb")) {
  board_test("wasb")
} else {
  test_that("can't register wasb board", {
    skip("failed to register wasb board")
  })
}
