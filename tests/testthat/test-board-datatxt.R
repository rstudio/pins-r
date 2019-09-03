context("board datatxt")

test_that("can board_register() a data.txt board", {
  skip_on_cran()

  board_register("datatxt",
                 name = "simpletxt",
                 url = "https://raw.githubusercontent.com/rstudio/pins/master/tests/testthat/datatxt/data.txt",
                 cache = tempfile())

  expect_true("simpletxt" %in% board_list())
})

test_that("can pin_get() iris from a data.txt board", {
  skip_on_cran()

  expect_equal(nrow(pin_get("iris", "simpletxt")), 150)
  expect_equal(ncol(pin_get("iris", "simpletxt")), 5)
})

test_that("can not evaluate expressions from data.txt board", {
  skip_on_cran()

  metadata <- jsonlite::fromJSON(pin_find("mtcars_expr", board = "simpletxt", metadata = TRUE)$metadata)
  expect_true(metadata$rows != 32)
  expect_true(metadata$cols != 11)

  expect_true(is.character(metadata$rows))
  expect_true(is.character(metadata$cols))
})

test_that("can board_deregister() a data.txt board", {
  skip_on_cran()

  board_deregister("simpletxt")

  expect_true(!"simpletxt" %in% board_list())
})
