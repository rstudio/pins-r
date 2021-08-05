test_that("can read from and write to registry ", {
  board <- legacy_temp()

  expect_equal(pin_registry_read(board), list())
  pin_registry_write(board, list(list(name = "x")))
  expect_equal(pin_registry_read(board), list(x = list(name = "x")))
})

test_that("can add, modify, and delete existing entries", {
  board <- legacy_temp()

  pin_registry_update(board, "x", list(test = "x"))
  expect_equal(pin_registry_read(board), list(x = list(test = "x", name = "x")))

  pin_registry_update(board, "x", list(test = "y"))
  expect_equal(pin_registry_read(board), list(x = list(test = "y", name = "x")))

  pin_registry_remove(board, "x")
  expect_equal(pin_registry_read(board), list())

  # no error if removing non-existent pin
  expect_error(pin_registry_remove(board, "x"), NA)
})

test_that("can access entries", {
  board <- legacy_temp()

  expect_snapshot(pin_registry_retrieve(board, "x"), error = TRUE)

  pin_registry_update(board, "x", list(test = "x"))
  expect_equal(pin_registry_retrieve(board, "x"), list(test = "x", name = "x"))
})
