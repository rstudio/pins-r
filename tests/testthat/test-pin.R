# main types --------------------------------------------------------------

test_that("can pin() a data frame", {
  board <- legacy_temp()

  df <- data.frame(
    raw = charToRaw("asdas"),
    posix = as.POSIXlt(Sys.time(), "EST"),
    date = as.Date(35981, origin = "1899-12-30"),
    integer = 1L,
    numeric = 1,
    logical = TRUE,
    stringsAsFactors = FALSE
  )
  pin(df, "df", board = board)
  expect_equal(pin_get("df", board = board), df)
})

test_that("can pin() a data.table", {
  board <- legacy_temp()

  dt <- data.table::data.table(x = 1:2, y = list("a", "b"))
  pin(dt, "dt", board = board)
  expect_equal(pin_get("dt", board = board), dt)

  # Check that pin_safe_csv() hasn't mutated original data.table
  expect_named(dt, c("x", "y"))
})

test_that("can pin an arbitrary object", {
  board <- legacy_temp()

  x <- list(1, letters, c(TRUE, FALSE, NA))
  pin(x, "x", board = board)
  expect_equal(pin_get("x", board = board), x)
})

test_that("AsIs class stripped when using I", {
  board <- legacy_temp()

  df <- data.frame(x = 1)
  pin(I(df), "df", board = board)
  expect_equal(pin_get("df", board = board), df)
})

test_that("can pin a file", {
  board <- legacy_temp()

  pin(test_path("files/hello.txt"), "hello", board = board)
  expect_equal(
    pin_get("hello", board = board),
    as.character(pin_registry_path(board, "hello", "hello.txt"))
  )
})

test_that("can pin() remote CSV with URL and name", {
  board <- legacy_temp()

  url <- "https://raw.githubusercontent.com/rstudio/pins/master/tests/testthat/datatxt/iris/data.csv"
  pin <- pin(url, "iris", board = board)

  expect_equal(dim(read.csv(pin)), c(150, 5))
})

test_that("unavailable url can use cache", {
  skip_on_cran()
  board <- legacy_temp()

  expect_snapshot(error = TRUE, {
    pin("http://httpstat.us/404", "test", board = board)
    pin(1:10, "test", board = board)
    x <- pin("http://httpstat.us/404", "test", board = board)
    expect_equal(x, 1:10)
  })
})

# custom metadata -------------------------------------------------------------------

test_that("can pin() with custom metadata", {
  withr::local_options(lifecycle_verbosity = "quiet")
  board <- legacy_temp()

  meta <- list(
    source = "The R programming language",
    extra_info = list(
      list(name = "Species", description = "Really like this column"),
      list(name = "Sepal.Length", description = "Sepal Length"),
      list(name = "Sepal.Width", description = "Sepal Width"),
      list(name = "Petal.Length", description = "Petal Length"),
      list(name = "Petal.Width", description = "Petal Width")
    )
  )
  pin(iris, "iris", metadata = meta, board = board)
  meta2 <- pin_info("iris", board)
  expect_equal(meta2[c("source", "extra_info")], meta)

  expect_snapshot(pin(iris, "iris2", board = board, custom_metadata = meta))
  meta2 <- pin_info("iris2", board)
  expect_equal(meta2[c("source", "extra_info")], meta)
})

# helpers -----------------------------------------------------------------

test_that("can sanitize data frame names", {
  name <- "___sdf ds32___42342     dsf dsf dsfds____"
  expect_equal(
    pin_default_name(name, board_temp()),
    "sdf-ds32-42342-dsf-dsf-dsfds"
  )
})

