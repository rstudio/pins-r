test_that("can read from and write to registry ", {
  board <- local_registry_board()

  expect_equal(pin_registry_read(board), list())
  pin_registry_write(board, list(list(name = "x")))
  expect_equal(pin_registry_read(board), list(x = list(name = "x")))
})

test_that("can add and modify existing entries", {
  local_edition(3)
  board <- local_registry_board()

  pin_registry_update(board, "x", list(test = "x"))
  expect_equal(pin_registry_read(board), list(x = list(test = "x", name = "x")))

  pin_registry_update(board, "x", list(test = "y"))
  expect_equal(pin_registry_read(board), list(x = list(test = "y", name = "x")))
})

test_that("can access entries", {
  local_edition(3)
  board <- local_registry_board()

  pin_registry_update(board, "x", list(test = "x"))

  expect_equal(pin_registry_read(board), list(x = list(test = "x", name = "x")))

  pin_registry_update(board, "x", list(test = "y"))
  expect_equal(pin_registry_read(board), list(x = list(test = "y", name = "x")))
})

