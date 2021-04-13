test_that("bundle contains expected files", {
  path <- withr::local_tempdir()

  df <- data.frame(x = 1:2, y = 2:1)
  board <- list(account = "TEST", server = "example.com")
  metadata <- list(path = "test.csv")


})

test_that("generates index files", {
  expect_snapshot({
    board <- list(account = "TEST", server = "example.com")
    df <- data.frame(x = 1:2, y = 2:1)
    metadata <- list(path = "test.csv")

    cat(rsc_bundle_preview_index(board, "test", df, metadata))
  })
})

test_that("generates preview data", {
  # for data frame
  expect_snapshot({
    df <- data.frame(x = 1:2, y = c("a", "b"), stringsAsFactors = FALSE)
    str(rsc_bundle_preview_data(df))
  })

  # for NULL
  expect_snapshot({
    str(rsc_bundle_preview_data(NULL))
  })
})
