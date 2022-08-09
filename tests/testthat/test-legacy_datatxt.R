test_that("can board_register() a data.txt board", {
  skip_on_cran()

  board_register_datatxt(
    name = "simpletxt",
    url = "https://raw.githubusercontent.com/rstudio/pins-r/master/tests/testthat/datatxt/data.txt",
    cache = tempfile()
  )

  expect_true("simpletxt" %in% board_list())
})

test_that("can pin_get() iris from a data.txt board", {
  skip_on_cran()

  b <- local_legacy_datatxt()
  iris <- pin_get("iris", board = b)

  expect_equal(dim(iris), c(150, 5))
})

test_that("doen't evaluate expressions in data.txt", {
  skip_on_cran()

  b <- local_legacy_datatxt()
  json <- pin_find("mtcars_expr", board = b, metadata = TRUE)$metadata
  meta <- jsonlite::fromJSON(json)

  expect_equal(meta$rows, "30+2")
})

test_that("can board_register() with URL", {
  skip_on_cran()

  board_name <- board_register(
    "https://raw.githubusercontent.com/rstudio/pins-r/master/tests/testthat/datatxt/data.txt",
    name = "simpletxt",
    cache = tempfile()
  )
  withr::defer(board_deregister("simpletxt"))
  expect_equal(board_name, "simpletxt")

  board_name <- board_register(
    "https://raw.githubusercontent.com/rstudio/pins-r/master/tests/testthat/datatxt/data.txt",
    cache = tempfile()
  )
  withr::defer(board_deregister("raw"))
  expect_equal(board_name, "raw")
})
