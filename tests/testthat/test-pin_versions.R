test_that("can use old pin_versions() api", {
  board <- legacy_local()
  pin(x = 1:5, "x", board = board)

  expect_snapshot({
    x <- pin_versions("x")
    x <- pin_versions("x", "local")
    x <- pin_versions("x", board)
  })
})

test_that("`full` is deprecated", {
  board <- board_temp()
  board %>% pin_write(x = 1:5, "x")

  expect_snapshot({
    x <- pin_versions(board, "x", full = TRUE)
  })
})

test_that("can parse versions from path", {
  date <- as.POSIXct("2020-01-03 04:05", tz = "")
  out <- version_from_path(paste0(as_8601_compact(date), "-", "hash"))
  expect_equal(out$created, date)
  expect_equal(out$hash, "hash")
})
