test_that("write_yaml can write non-UTF8 data", {
  str <- "fa\xE7ile"
  Encoding(str) <- "latin1"

  x <- list(str)
  names(x) <- str

  path <- tempfile()
  write_yaml(x, path)

  y <- yaml::read_yaml(path)
  expect_equal(y, list("fa\u00e7ile" = "fa\u00e7ile"))
})

test_that("can end with slash if needed", {
  expect_identical(end_with_slash("foo"), "foo/")
  expect_identical(end_with_slash("foo/"), "foo/")
  expect_identical(end_with_slash(c("foo/", "bar")), c("foo/", "bar/"))
  expect_identical(end_with_slash(c("foo", "bar/")), c("foo/", "bar/"))
})

test_that("http_utils_progress decides correctly", {
  http_utils_progress(is_interactive = FALSE) |>
    expect_null()

  withr::with_options(
    list(pins.progress = FALSE),
    {
      http_utils_progress(size = NULL, is_interactive = TRUE) |>
        expect_null()
    }
  )

  http_utils_progress(size = NULL, is_interactive = TRUE) |>
    inherits("request") |>
    expect_true()

  withr::with_options(
    list(pins.progress.size = 1),
    {
      http_utils_progress(size = "1", is_interactive = TRUE) |>
        expect_null()
    }
  )

  withr::with_options(
    list(pins.progress.size = 1),
    {
      http_utils_progress(size = 2, is_interactive = TRUE) |>
        inherits("request") |>
        expect_true()
    }
  )

  http_utils_progress(size = 10^7 + 1, is_interactive = TRUE) |>
    inherits("request") |>
    expect_true()

  http_utils_progress(size = 10^7, is_interactive = TRUE) |>
    expect_null()
})
