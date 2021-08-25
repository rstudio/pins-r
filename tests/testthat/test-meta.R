test_that("non-existent file returns default data", {
  expect_equal(read_meta(tempfile()), list(api_version = 1))
})

test_that("standard metadata is useful", {
  df <- data.frame(x = 1:10)
  path <- withr::local_tempfile()
  write_rds(df, path)

  meta <- standard_meta(path, "arrow", df)
  meta$file <- "df.rds"
  meta$created <- "<TODAY>"
  expect_snapshot_output(str(meta))
})

test_that("newer version triggers error", {
  path <- withr::local_tempdir()

  write_meta(list(api_version = 2), path)
  expect_snapshot_error(read_meta(path))
})

test_that("produces reasonable default title", {
  expect_snapshot({
    default_title(NULL, c("data.csv"))
    default_title(NULL, c("data.csv", "foo.csv"))
    default_title(mtcars, "data.csv")
    default_title(1:10, "data.csv")
  })
})
