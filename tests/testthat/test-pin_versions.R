test_that("can use old pin_versions() api", {
  board <- legacy_local()
  pin(x = 1:5, "x", board = board)
  withr::defer(pin_remove("x", board))

  expect_snapshot({
    x <- pin_versions("x")
    x <- pin_versions("x", "local")
    x <- pin_versions("x", board)
  })
})

test_that("can't swap arguments or omit name with modern api", {
  board <- board_temp()
  name <- local_pin(board, 1)
  expect_snapshot(pin_versions(name, board), error = TRUE)
  expect_snapshot(pin_versions(board), error = TRUE)
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


# versions pruning --------------------------------------------------------

test_that("can prune old versions", {
  board <- board_temp(versioned = TRUE)

  pin_write(board, 1, "x")
  pin_write(board, 2, "x")
  pin_write(board, 3, "x")
  pin_write(board, 4, "x")
  expect_equal(nrow(pin_versions(board, "x")), 4)

  ui_loud()
  expect_snapshot({
    pin_versions_prune(board, "x", n = 1)
    pin_versions_prune(board, "x", n = 1)
  })

  expect_equal(nrow(pin_versions(board, "x")), 1)
})

test_that("can prune by days or number", {
  # from newest to oldest
  x <- Sys.time() - (0:3 * 2 * 86400)

  expect_equal(versions_keep(x, n = 2), c(TRUE, TRUE, FALSE, FALSE))
  expect_equal(versions_keep(x, n = 1), c(TRUE, FALSE, FALSE, FALSE))
  # always keeps latest
  expect_equal(versions_keep(x, n = 0), c(TRUE, FALSE, FALSE, FALSE))

  expect_equal(versions_keep(x, days = 5), c(TRUE, TRUE, TRUE, FALSE))
  expect_equal(versions_keep(x, days = 2), c(TRUE, FALSE, FALSE, FALSE))
  # always keeps latest
  expect_equal(versions_keep(x, days = 0), c(TRUE, FALSE, FALSE, FALSE))
})

test_that("versions_keep() gives useful errors", {
  expect_snapshot(error = TRUE, {
    versions_keep(NULL)
    versions_keep(Sys.time())
    versions_keep(Sys.time(), n = "a")
    versions_keep(Sys.time(), days = "a")
  })
})
