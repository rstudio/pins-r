skip_if_not_installed("webfakes")

httpbin <- local_httpbin_app()
httpbin_port <- httpbin$get_port()
redact_port <- function(snapshot) {
  snapshot <- gsub(httpbin_port, "<port>", snapshot, fixed = TRUE)
}

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

test_that("can download pin from board_folder version dir", {
  b1 <- board_folder(withr::local_tempfile())
  b1 %>% pin_write(1:10, "x")
  b2_path <- fs::path(b1$path, "x", pin_versions(b1, "x")$version)

  b2_server <- webfakes::new_app()
  b2_server$use(webfakes::mw_static(root = b2_path))
  board_fake <- webfakes::new_app_process(b2_server)

  b2 <- board_url(c(x = board_fake$url()))
  b2 %>%
    pin_read("x") %>%
    expect_equal(1:10)
})

test_that("can download pin from versioned board_folder", {
  b1 <- board_folder(withr::local_tempdir(), versioned = TRUE)
  b1 %>% pin_write(1:10, "x", type = "json")
  b1 %>% pin_write(11:20, "y", type = "json")
  b1 %>% pin_write(1:20, "y", type = "csv")
  write_board_manifest(b1)
  b1_path <- fs::path(b1$path)

  b1_server <- webfakes::new_app()
  b1_server$use(webfakes::mw_static(root = b1_path))
  b1_process <- webfakes::new_app_process(b1_server)

  b2 <- board_url(b1_process$url())
  b2 %>%
    pin_read("x") %>%
    expect_equal(1:10)
})

test_that("pin_meta() works for versioned board", {
  b1 <- board_folder(withr::local_tempfile(), versioned = TRUE)
  b1 %>% pin_write(11:20, "y", type = "json")
  b1 %>% pin_write(1:20, "y", type = "csv")
  write_board_manifest(b1)
  b1_path <- fs::path(b1$path)

  b1_server <- webfakes::new_app()
  b1_server$use(webfakes::mw_static(root = b1_path))
  b1_process <- webfakes::new_app_process(b1_server)

  b2 <- board_url(b1_process$url())

  versions <- b2 %>% pin_versions("y")

  expect_s3_class(
    b2 %>% pin_meta("y"),
    "pins_meta"
  )

  expect_s3_class(
    b2 %>% pin_meta("y", version = versions$version[[1]]),
    "pins_meta"
  )

})

test_that("useful error for missing or unparseable manifest file", {
  b1 <- board_folder(withr::local_tempdir(), versioned = TRUE)
  b1 %>% pin_write(1:10, "x", type = "json")
  b1 %>% pin_write(1:20, "y", type = "csv")
  b1_path <- fs::path(b1$path)

  b1_server <- webfakes::new_app()
  b1_server$use(webfakes::mw_static(root = b1_path))
  b2 <- webfakes::new_app_process(b1_server)

  expect_error(
    board_url(b2$url()),
    "Failed to access manifest file"
  )

  write.csv(mtcars, file = fs::path(b1_path, "_pins.yaml"))
  b3_server <- webfakes::new_app()
  b3_server$use(webfakes::mw_static(root = b1_path))
  b4 <- webfakes::new_app_process(b3_server)

  expect_error(
    board_url(b4$url()),
    "Failed to parse manifest file at URL"
  )

})

test_that("useful errors for unsupported methods", {
  board <- board_url(c("x" = "foo"))

  expect_snapshot(error = TRUE, {
    board %>% pin_write(1:5, "x")
    board %>% pin_delete("x")
    board %>% pin_meta("froofy", version = "x")
    board %>% pin_meta("x", version = "x")
    board %>% pin_versions("x")
    board %>% pin_version_delete("x")
    board %>% board_deparse()
    pin(1:5, name = "x", board = board)
    pin_get(name = "x", board = board)
  })
})

test_that("useful errors for specifying board", {
  expect_snapshot(error = TRUE, {
    board_url(c("foo", "bar"))
    board_url(list("a", 1:2))
    board_url(1:10)
    board_url(c("x" = "foo"), headers = list(auth = "x"))
    board_url(c("x" = "foo"), headers = "my_api_key")
  })
})

# http_download ----------------------------------------------------------

test_that("use cache with Last-Modified header", {
  skip_on_cran()
  path <- fs::dir_create(withr::local_tempdir())
  url <- httpbin$url("/cache")

  expect_condition(
    http_download(url, path, "test", header = c(x = "potato")),
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
  url <- httpbin$url("/etag/xxx")

  expect_condition(
    http_download(url, path, "test"),
    class = "pins_cache_downloaded"
  )

  cache <- read_cache(download_cache_path(path))
  expect_equal(cache[[url]]$etag, "xxx")

  expect_condition(
    http_download(url, path, "test"),
    class = "pins_cache_not_modified"
  )
})

test_that("no request if cache-control set", {
  skip_on_cran()
  path <- fs::dir_create(withr::local_tempdir())
  url <- httpbin$url("/cache/60")

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
  url <- httpbin$url("/cache")
  cached <- http_download(url, path, "test")
  expect_false(fs::file_access(cached, "write"))
})
