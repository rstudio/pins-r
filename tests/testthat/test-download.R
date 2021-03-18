test_that("use cache with Last-Modified header", {
  skip_on_cran()
  b <- board_temp()

  path <- download_cache(b, "https://httpbin.org/cache", "test1")
  expect_named(cache_read_metadata(b), "https://httpbin.org/cache")

  expect_condition(
    download_cache(b, "https://httpbin.org/cache", "test1"),
    class = "pins_cache_not_modified"
  )
})

test_that("use cache with etags header", {
  skip_on_cran()
  b <- board_temp()

  path <- download_cache(b, "https://httpbin.org/etag/xxx", "test1")
  meta <- cache_read_metadata(b)
  expect_equal(meta[["https://httpbin.org/etag/xxx"]]$etag, "xxx")

  expect_condition(
    download_cache(b, "https://httpbin.org/etag/xxx", "test1"),
    class = "pins_cache_not_modified"
  )
})

test_that("no request if cache-control set", {
  skip_on_cran()
  b <- board_temp()

  path <- download_cache(b, "https://httpbin.org/cache/60", "test1")
  expect_named(cache_read_metadata(b), "https://httpbin.org/cache/60")

  expect_condition(
    download_cache(b, "https://httpbin.org/cache/60", "test1"),
    class = "pins_cache_cached"
  )
})
