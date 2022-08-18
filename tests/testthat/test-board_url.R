test_that("provides key methods", {
  board <- board_url_test(c(
    rds = github_raw("rstudio/pins-r/master/tests/testthat/pin-rds/")
  ))

  board %>%
    pin_list() %>%
    expect_equal("rds")

  board %>%
    pin_read("rds") %>%
    expect_equal(data.frame(x = 1:10))
})

test_that("absent pins handled consistently", {
  board <- board_url_test(c(
    x = github_raw("rstudio/pins-r/master/tests/testthat/pin-rds/")
  ))

  expect_equal(pin_list(board), "x")
  expect_equal(pin_exists(board, "x"), TRUE)
  expect_equal(pin_exists(board, "y"), FALSE)

  expect_error(pin_meta(board, "y"), class = "pins_pin_missing")
})

test_that("only downloads once", {
  board <- board_url_test(c(
    rds = github_raw("rstudio/pins-r/master/tests/testthat/pin-rds/")
  ))

  board %>%
    pin_read("rds") %>%
    expect_condition(class = "pins_cache_downloaded")

  board %>%
    pin_read("rds") %>%
    expect_condition(class = "pins_cache_cached")
})

test_that("raw pins can only be downloaded", {
  board <- board_url_test(c(
    raw = github_raw("rstudio/pins-r/master/tests/testthat/pin-files/first.txt")
  ))

  board %>%
    pin_read("raw") %>%
    expect_snapshot_error()

  board %>%
    pin_download("raw") %>%
    readLines() %>%
    expect_equal("abcdefg")
})

test_that("useful errors for unsupported methods", {
  board <- board_url(c("x" = "foo"))

  expect_snapshot(error = TRUE, {
    board %>% pin_write(1:5, "x")
    board %>% pin_delete("x")
    board %>% pin_meta("froofy", version = "x")
    board %>% pin_meta("x", version = "x")
    board %>% pin_versions("x")
    board %>% board_deparse()
    pin(1:5, name = "x", board = board)
    pin_get(name = "x", board = board)
  })
})

# http_download ----------------------------------------------------------

test_that("use cache with Last-Modified header", {
  skip_on_cran()
  path <- fs::dir_create(withr::local_tempdir())
  url <- "https://httpbin.org/cache"

  expect_condition(
    http_download(url, path, "test"),
    class = "pins_cache_downloaded"
  )

  cache <- read_cache(download_cache_path(path))
  expect_named(cache, url)

  expect_condition(
    http_download(url, path, "test"),
    class = "pins_cache_not_modified"
  )
})

test_that("use cache with etags header", {
  skip_on_cran()
  path <- fs::dir_create(withr::local_tempdir())
  url <- "https://httpbin.org/etag/xxx"

  expect_condition(
    http_download(url, path, "test"),
    class = "pins_cache_downloaded"
  )

  cache <- read_cache(download_cache_path(path))
  expect_equal(cache[["https://httpbin.org/etag/xxx"]]$etag, "xxx")

  expect_condition(
    http_download(url, path, "test"),
    class = "pins_cache_not_modified"
  )
})

test_that("no request if cache-control set", {
  skip_on_cran()
  path <- fs::dir_create(withr::local_tempdir())
  url <- "https://httpbin.org/cache/60"

  expect_condition(
    http_download(url, path, "test"),
    class = "pins_cache_downloaded"
  )

  cache <- read_cache(download_cache_path(path))
  expect_named(cache, url)

  expect_condition(
    http_download(url, path, "test"),
    class = "pins_cache_cached"
  )
})

test_that("http_download saves read-only file", {
  skip_on_cran()
  path <- fs::dir_create(withr::local_tempdir())
  cached <- http_download("https://httpbin.org/cache", path, "test")
  expect_false(fs::file_access(cached, "write"))
})
