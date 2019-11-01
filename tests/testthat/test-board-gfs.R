context("board gcloud")

test_gcloud_bucket <- Sys.getenv("TEST_GOOGLE_BUCKET", "")

test_that("board contains proper gcloud headers", {
  headers <- names(gcloud_headers(list(token = "abc"), "PUT", "x", "files/hello.txt")$headers)

  expect_true("Content-Type" %in% headers)
  expect_true("Authorization" %in% headers)
})

if (nchar(test_gcloud_bucket) > 0) {
  if ("gcloud" %in% board_list())
    board_deregister("gcloud")

  board_register("gcloud",
                 bucket = test_gcloud_bucket,
                 cache = tempfile())
}

if (test_board_is_registered("gcloud")) {
  board_test("gcloud")
} else {
  test_that("can't register gcloud board", {
    skip("failed to register gcloud board")
  })
}
