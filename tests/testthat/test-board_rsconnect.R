# user facing -------------------------------------------------------------

test_that("can round-trip a pin (v1)", {
  withr::local_options(pins.quiet = TRUE)
  board <- board_rsconnect(cache = tempfile())

  df1 <- data.frame(x = 1:5)
  pin_write(board, df1, "df1", type = "rds")
  withr::defer(board_pin_remove(board, "df1"))

  df2 <- pin_read(board, "df1")
  expect_equal(df1, df2)
})

test_that("can round-trip a pin (v0)", {
  withr::local_options(pins.quiet = TRUE)
  board <- board_rsconnect(cache = tempfile())

  df1 <- data.frame(x = 1:5)
  pin(df1, "df1", board = board)
  withr::defer(board_pin_remove(board, "df1"))

  df2 <- pin_get("df1", board = board)
  expect_equal(df1, df2)
})

test_that("can search pins", {
  withr::local_options(pins.quiet = TRUE)
  board <- board_rsconnect(cache = tempfile())

  df1 <- data.frame(x = 1:5)
  pin(df1, "xyzxyzxyzxyz-abc", board = board)
  withr::defer(board_pin_remove(board, "xyzxyzxyzxyz-abc"))

  expect_equal(nrow(board_pin_find(board, "xyzxyzxyzxyz")), 1)
})

# versioning --------------------------------------------------------------

test_that("versioned by default", {
  board <- board_rsconnect()
  withr::defer(board_pin_remove(board, "df1"))

  pin_write(board, data.frame(x = 1:3), "df1", type = "rds")
  pin_write(board, data.frame(x = 1:4), "df1", type = "rds")
  pin_write(board, data.frame(x = 1:5), "df1", type = "rds")

  versions <- board_pin_versions(board, "df1")
  expect_equal(nrow(versions), 3)

  df2 <- pin_read(board, "df1", version = versions$version[[2]])
  expect_equal(df2, data.frame(x = 1:4))
})

test_that("if unversioned, deletes last one", {
  withr::local_options(pins.quiet = TRUE)

  board <- board_rsconnect(versions = FALSE)
  withr::defer(board_pin_remove(board, "df1"))

  pin_write(board, data.frame(x = 1), "df1", type = "rds")
  pin_write(board, data.frame(x = 2), "df1", type = "rds")

  guid <- rsc_content_find(board, "df1")$guid
  expect_equal(nrow(rsc_content_versions(board, guid)), 1)

  df2 <- pin_read(board, "df1")
  expect_equal(df2$x, 2)
})

test_that("can't accidentally switch from versioned to unversioned", {
  board <- board_rsconnect()
  withr::defer(board_pin_remove(board, "df1"))

  df1 <- data.frame(x = 1:3)
  pin_write(board, df1, "df1", type = "rds")
  pin_write(board, df1, "df1", type = "rds")
  expect_snapshot(error = TRUE,
    pin_write(board, df1, "df1", type = "rds", versioned = FALSE)
  )
})

# content -----------------------------------------------------------------

test_that("can find content by full/partial name", {
  board <- board_rsconnect()

  json <- rsc_content_find(board, "sales-by-baths")
  expect_equal(json$name, "sales-by-baths")

  json <- rsc_content_find(board, "hadley/sales-by-baths")
  expect_equal(json$name, "sales-by-baths")

  expect_snapshot(rsc_content_find(board, "susan/sales-by-baths"), error = TRUE)
})

test_that("can create and delete content", {
  board <- board_rsconnect()

  rsc_content_create(board, "test-1", list())
  expect_snapshot(error = TRUE,
    rsc_content_create(board, "test-1", list())
  )

  rsc_content_delete(board, "test-1")
  expect_snapshot(error = TRUE,
    rsc_content_delete(board, "test-1")
  )
})

test_that("can parse user & pin name", {
  expect_equal(rsc_parse_name("x"), list(owner = NULL, name = "x"))
  expect_equal(rsc_parse_name("y/x"), list(owner = "y", name = "x"))
})

# bundle ------------------------------------------------------------------

test_that("can create a bundle", {
  board <- board_rsconnect()
  temp <- withr::local_tempfile(fileext = "rds")
  saveRDS(data.frame(x = 10), temp)

  out <- rsc_bundle(board, "test", temp, list())
  expect_setequal(
    dir(out),
    c("data.txt", "index.html", "manifest.json", fs::path_file(temp))
  )
})
