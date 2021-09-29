test_that("bundle contains expected files", {
  path <- withr::local_tempdir()

  df <- data.frame(x = 1:2, y = 2:1)
  saveRDS(df, fs::path(path, "test.rds"))

  board <- list(account = "TEST", server_name = "example.com")
  metadata <- list(file = "test.rds")
  class(board) <- c("pins_board_rsconnect", "pins_board")

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
    class(board) <- c("pins_board_rsconnect", "pins_board")
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


# santise -----------------------------------------------------------------

test_that("rsc_bundle_preview_data() sanitisies columns", {
  df <- data.frame(x = as.difftime(1, units = "secs"))
  preview <- rsc_bundle_preview_data(df)
  expect_equal(preview$data$x, "1 secs")
})

test_that("simple types remain as is", {
  expect_equal(sanitise_col(TRUE), TRUE)
  expect_equal(sanitise_col(1.5), 1.5)
  expect_equal(sanitise_col("x"), "x")

  expect_equal(sanitise_col(factor("a")), factor("a"))
  expect_equal(sanitise_col(as.Date("2010-01-01")), as.Date("2010-01-01"))
  expect_equal(sanitise_col(as.POSIXct("2010-01-01")), as.POSIXct("2010-01-01"))
})

test_that("other base types convert with format", {
  expect_equal(sanitise_col(as.difftime(1, units = "secs")), "1 secs")
})

test_that("unless their format method isn't designed for columns", {
  df <- data.frame(x = 1:2)
  expect_equal(sanitise_col(df), rep("No preview available", 2))
  expect_equal(sanitise_col(mean), "No preview available")
})
