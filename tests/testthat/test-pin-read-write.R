test_that("can round trip all types", {
  skip_if_not_installed("qs")
  skip_if_not_installed("arrow")
  board <- board_temp()

  # Data frames
  df <- tibble::tibble(x = 1:10)
  pin_write(board, df, "df-1", type = "rds")
  expect_equal(pin_read(board, "df-1"), df)

  pin_write(board, df, "df-2", type = "parquet")
  expect_equal(pin_read(board, "df-2"), df)

  pin_write(board, df, "df-3", type = "arrow")
  expect_equal(pin_read(board, "df-3"), df)

  pin_write(board, df, "df-4", type = "csv")
  expect_equal(pin_read(board, "df-4"), as.data.frame(df))

  pin_write(board, df, "df-5", type = "qs")
  expect_equal(pin_read(board, "df-5"), df)

  # List
  x <- list(a = 1:5, b = 1:10)
  pin_write(board, x, "x-1", type = "json")
  expect_equal(pin_read(board, "x-1"), x)
})

test_that("can't pin_read() file that was pin_uploaded()", {
  path <- withr::local_tempfile()
  writeLines("Hi!", path)

  board <- board_temp()
  pin_upload(board, path, "test")
  expect_snapshot(error = TRUE, pin_read(board, "test"))
})

test_that("useful errors on bad inputs", {
  board <- board_temp()

  expect_snapshot(error = TRUE, {
    pin_write(mtcars)
    pin_write(board, mtcars, name = 1:10)
    pin_write(board, mtcars, name = "mtcars", "json")
    pin_write(board, mtcars, name = "mtcars", type = "froopy-loops")
    pin_write(board, mtcars, name = "mtcars", metadata = 1)
    pin_write(board, mtcars, title = "title", tags = list(a = "a"))
    pin_write(board, mtcars, title = "title", urls = list(a = "a"))
    pin_write(board, mtcars, title = "title", metadata = c("tag1", "tag2"))
  })
})

test_that("guess_type() works as expected", {
  expect_equal(guess_type(mtcars), "rds")
  expect_equal(guess_type(lm(mpg ~ disp, data = mtcars)), "rds")
  expect_equal(guess_type(list(x = 1)), "json")
})

test_that("pin_write() noisily generates name and type", {
  ui_loud()
  local_mocked_bindings(version_name = function(metadata) "20120304T050607Z-xxxxx")
  expect_snapshot(error = TRUE, {
    b <- board_temp()
    pin_write(b, mtcars)
    pin_write(b, data.frame(x = 1))
  })
})

test_that("user can supply metadata", {
  board <- board_temp()

  pin_write(board, 1:10, "x", metadata = list(name = "Susan"), description = "A vector")
  meta <- pin_meta(board, "x")
  expect_equal(meta$user, list(name = "Susan"))
  expect_equal(meta$description, "A vector")
})

test_that("can request specific hash", {
  local_mocked_bindings(
    version_name = function(metadata) "20120304T050607Z-xxxxx",
    write_rds = write_rds_test
    )
  ui_loud()
  expect_snapshot(error = TRUE, {
    b <- board_temp()
    pin_write(b, mtcars, name = "mtcars", type = "rds")
    pin_read(b, "mtcars", hash = "ABCD")
  })
})

test_that("informative error for legacy boards", {
  expect_snapshot(error = TRUE, {
    board <- legacy_temp()
    board %>% pin_write(1:10, "x")
    board %>% pin_read("x")
  })
})

