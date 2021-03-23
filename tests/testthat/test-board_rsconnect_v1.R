# user facing -------------------------------------------------------------

test_that("can round-trip a pin", {
  board <- board_rsconnect(cache = tempfile())

  df1 <- data.frame(x = 1:5)
  pin_write(board, df1, "df1", type = "rds")
  withr::defer(rsc_content_delete(board, "df1"))

  df2 <- pin_read(board, "df1")
  expect_equal(df1, df2)
})

test_that("can read a pin", {
  board <- board_rsconnect(cache = tempfile())

  out <- pin_read(board, "sales-by-baths")
  expect_s3_class(out, "tbl_df")
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
