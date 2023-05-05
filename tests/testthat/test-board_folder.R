test_api_basic(board_temp())
test_api_versioning(board_temp(versioned = TRUE))
test_api_meta(board_temp())
test_api_manifest(board_temp())

test_that("has useful print method", {
  path <- withr::local_tempfile()
  withr::local_options(cli.width = 120)
  expect_snapshot(
    board_folder(path),
    transform = ~ gsub("Path: .*", "Path: '<redacted>'", .x)
  )
})

test_that("can upload/download multiple files", {
  path1 <- withr::local_tempfile()
  writeLines("a", path1)
  path2 <- withr::local_tempfile()
  writeLines("b", path2)

  board <- board_temp()
  suppressMessages(pin_upload(board, c(path1, path2), "test"))

  out <- pin_download(board, "test")
  expect_equal(length(out), 2)
  expect_equal(readLines(out[[1]]), "a")
  expect_equal(readLines(out[[2]]), "b")
})

test_that("can browse", {
  b <- board_folder(withr::local_tempfile())
  b %>% pin_write(1:10, "x")

  expect_snapshot({
    b %>% pin_browse("x")
    b %>% pin_browse("x", local = TRUE)
  }, error = TRUE, transform = ~ gsub("<.*>", "<redacted>", .x))
})

test_that("can deparse", {
  b <- board_folder(withr::local_tempfile())
  expect_snapshot(
    board_deparse(b),
    transform = ~ gsub('".*"', '"<redacted>"', .x)
  )
})

test_that("contents of manifest match", {
  b <- board_folder(withr::local_tempfile(), versioned = TRUE)
  pin_write(b, mtcars, "mtcars-csv", type = "csv")
  pin_write(b, head(mtcars), "mtcars-csv", type = "csv")
  pin_write(b, mtcars, "mtcars-json", type = "json")
  write_board_manifest(b)

  # names are correct
  manifest <- yaml::read_yaml(fs::path(b$path, manifest_pin_yaml_filename))
  expect_identical(names(manifest), pin_list(b))

  # numbers of versions are correct
  expect_identical(
    map(manifest, length),
    list(`mtcars-csv` = 2L, `mtcars-json` = 1L)
  )

  # values (relative paths to versions) are correct
  imap(
    manifest,
    ~ expect_identical(
      .x,
      end_with_slash(as.character(fs::path(.y, pin_versions(b, .y)$version)))
    )
  )
})


test_that("generates useful messages", {
  skip_if_not_installed("mockery")

  mock_version_name <- mockery::mock(
    "20130104T050607Z-xxxxx",
    "20130204T050607Z-yyyyy",
    "20130304T050607Z-zzzzz"
  )
 local_mocked_bindings(version_name = mock_version_name)

  ui_loud()
  b <- board_temp()
  expect_snapshot({
    pin_write(b, 1:5, "x", type = "rds")
    pin_write(b, 1:6, "x", type = "rds")
    pin_write(b, 1:7, "x", type = "rds")
  })
})
