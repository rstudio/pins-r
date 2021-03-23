# main types --------------------------------------------------------------

test_that("can pin() a data frame", {
  board <- board_temp()

  df <- data.frame(
    raw = charToRaw("asdas"),
    posix = as.POSIXlt(Sys.time(), "EST"),
    date = as.Date(35981, origin = "1899-12-30"),
    integer = 1L,
    numeric = 1,
    logical = TRUE,
    stringsAsFactors = FALSE
  )
  pin_write(board, df, "df")
  expect_equal(pin_read(board, "df"), df)
})

test_that("can pin() a data.table", {
  board <- board_temp()

  dt <- data.table::data.table(x = 1:2, y = list("a", "b"))
  pin_write(board, dt, "dt")
  expect_equal(pin_read(board, "dt"), dt)

  # Check that pin_safe_csv() hasn't mutated original data.table
  expect_named(dt, c("x", "y"))
})

test_that("can pin an arbitrary object", {
  board <- board_temp()

  x <- list(1, letters, c(TRUE, FALSE, NA))
  pin_write(board, x, "x")
  expect_equal(pin_read(board, "x"), x)
})

test_that("AsIs class stripped when using I", {
  board <- board_temp()

  df <- data.frame(x = 1)
  pin_write(board, I(df), "df")
  expect_equal(pin_read(board, "df"), df)
})

test_that("can pin a file", {
  board <- board_temp()

  pin_write(board, test_path("files/hello.txt"), "hello")
  expect_equal(
    pin_read(board, "hello"),
    as.character(pin_registry_path(board, "hello", "hello.txt"))
  )
})

test_that("can pin() remote CSV with URL and name", {
  board <- board_temp()

  url <- "https://raw.githubusercontent.com/rstudio/pins/master/tests/testthat/datatxt/iris/data.csv"
  pin <- pin(url, "iris", board = board)

  expect_equal(dim(read.csv(pin)), c(150, 5))
})

test_that("unavailable url can use cache", {
  board <- board_temp()

  expect_snapshot(error = TRUE, {
    pin("http://httpstat.us/404", "test", board = board)
    pin_write(board, 1:10, "test")
    x <- pin("http://httpstat.us/404", "test", board = board)
    expect_equal(x, 1:10)
  })
})

# custom metadata -------------------------------------------------------------------

test_that("can pin() with custom metadata", {
  board <- board_temp()

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
  pin_write(board, iris, "iris", metadata = meta)
  meta2 <- pin_info("iris", board = board)
  expect_equal(meta2[c("source", "extra_info")], meta)

  expect_snapshot(pin(iris, "iris2", board = board, custom_metadata = meta))
  meta2 <- pin_info("iris2", board = board)
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

