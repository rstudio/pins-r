context("board datatxt")

test_that("can board_register() a data.txt board", {
  board_register("datatxt",
                 name = "simpletxt",
                 url = "https://raw.githubusercontent.com/rstudio/pins/master/tests/testthat/datatxt/data.txt")

  expect_true("simpletxt" %in% board_list())
})

test_that("can pin_get() iris from a data.txt board", {
  expect_equal(nrow(pin_get("iris", "simpletxt")), 150)
  expect_equal(ncol(pin_get("iris", "simpletxt")), 5)
})

test_that("can board_deregister() a data.txt board", {
  board_deregister("simpletxt")

  expect_true(!"simpletxt" %in% board_list())
})
