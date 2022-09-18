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

# manifest ----------------------------------------------------------

test_that("url-modifying functions work as expected", {
  url_root <- "https://posit.co/"
  expect_identical(url_dir("https://posit.co/test/"), url_root)
  expect_identical(url_dir("https://posit.co/test"), url_root)
  expect_identical(url_dir("https://posit.co/"), url_root)
  expect_identical(url_dir("https://posit.co"), url_root)

  url_root <- "https://posit.co/test/"
  expect_identical(url_dir(paste0(url_root, "path")), url_root)
  expect_identical(url_dir(paste0(url_root, "path/")), url_root)
  expect_identical(url_dir(paste0(url_root, "path.txt")), url_root)

  url_root <- "https://posit.co/test/path/"
  append <- function(...) {
    paste0(url_root, ...)
  }
  expect_identical(url_path(url_root, "dat.txt"), append("dat.txt"))
  expect_identical(url_path(url_root, "dir", "dat.txt"), append("dir/dat.txt"))
  expect_identical(url_path(url_root, "dir/dat.txt"), append("dir/dat.txt"))
})

test_that("manifest downloads and parses properly", {
  skip_on_cran()
  # TODO: update in subsequent PR
  url_manifest <-
    github_raw("ijlyttle/pinsManifest/main/tests/testthat/pins/pins.txt")
  manifest <- get_manifest(url_manifest)

  expect_type(manifest, "list")
  expect_named(manifest, c("mtcars-csv", "mtcars-json"))

  # prepended the root URL
  walk(manifest, ~expect_match(.x, paste0("^", url_dir(url_manifest))))
})

test_that("board methods work using versioned manifest", {
  skip_on_cran()
  # TODO: update in subsequent PR
  url_manifest_root <-
    github_raw("ijlyttle/pinsManifest/main/tests/testthat/pins/")
  board <- board_url_test(url_manifest_root)

  # exercise version
  meta_latest <- pin_meta(board, "mtcars-json", version = NULL)
  mtcars_json_earlier <-
    pin_read(board, "mtcars-json", version = "20220811T155803Z-c2702")

  expect_identical(pin_list(board), c("mtcars-csv", "mtcars-json"))
  expect_s3_class(pin_versions(board, "mtcars-csv"), "data.frame")

  expect_identical(meta_latest$local$version, "20220811T155805Z-c2702")
  expect_identical(names(mtcars_json_earlier), names(mtcars))
  expect_identical(mtcars_json_earlier$mpg, mtcars$mpg)
})

test_that("useful errors for manifest problems", {
  skip_on_cran()
  expect_snapshot(error = TRUE, {
    # failed request
    board_url("https://not_real_url.posit.co")
    # file not found
    board_url(github_raw("rstudio/pins-r/master/tests/testthat/pin-rds/"))
    # file not parsable YAML
    # TODO: update in subsequent PR
    board_url(github_raw("ijlyttle/pinsManifest/main/tests/testthat/pins/mtcars-csv/20220811T155805Z-48c73/mtcars-csv.csv"))
    # not supported format
    board_url(3)
    # unnamed list
    board_url(list("a", "b"))
    # list values not all character
    board_url(list(a = "a", b = 2))
    # unnamed character vector
    board_url(c("a", "b"))
  })
})
