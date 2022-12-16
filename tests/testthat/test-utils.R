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
