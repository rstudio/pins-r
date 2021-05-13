test_that("bundle contains expected files", {
  path <- withr::local_tempdir()

  df <- data.frame(x = 1:2, y = 2:1)
  saveRDS(df, fs::path(path, "test.rds"))

  board <- list(account = "TEST", server = "example.com")
  metadata <- list(file = "test.rds")

  out <- rsc_bundle(board, "test", fs::path(path, "test.rds"), list(), df)

  files <- fs::dir_ls(out)
  files <- files[!fs::is_dir(files)]
  expect_setequal(
    fs::path_file(files),
    c("data.txt", "index.html", "manifest.json", "test.rds")
  )
})

test_that("generates index files", {
  expect_snapshot({
    board <- list(account = "TEST", server_name = "example.com")
    df <- data.frame(x = 1:2, y = 2:1)
    metadata <- list(file = "test.csv")

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
