test_that("can read from and write to registry ", {
  board <- local_registry_board()

  expect_equal(pin_registry_read(board), list())
  pin_registry_write(board, list(list(name = "x")))
  expect_equal(pin_registry_read(board), list(x = list(name = "x")))
})

test_that("can add, modify, and delete existing entries", {
  board <- local_registry_board()

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
  board <- local_registry_board()

  expect_snapshot(pin_registry_retrieve(board, "x"), error = TRUE)

  pin_registry_update(board, "x", list(test = "x"))
  expect_equal(pin_registry_retrieve(board, "x"), list(test = "x", name = "x"))
})

test_that("can pin() concurrently", {
  temp_path <- withr::local_tempdir()
  board <- board_local(temp_path)

  pin10 <- function(path, proc) {
    board <- pins::board_local(path)
    data <- list(message = "concurrent test")
    for (i in 1:10) {
      pins::pin(data, name = as.character(proc * 100 + i), board = board)
    }
  }

  processes <- list()
  for (i in 1:10) {
    processes[[i]] <- callr::r_bg(pin10, list(path = temp_path, proc = i))
  }
  for (i in 1:10) {
    processes[[i]]$wait()
  }

  index <- pin_registry_read(board)
  expect_equal(length(index), 100)
})
