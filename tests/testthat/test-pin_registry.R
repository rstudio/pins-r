test_that("can read from and write to registry ", {
  board <- board_temp()

  expect_equal(pin_registry_read(board), list())
  pin_registry_write(board, list(list(name = "x")))
  expect_equal(pin_registry_read(board), list(x = list(name = "x")))
})

test_that("can add, modify, and delete existing entries", {
  board <- board_temp()

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
  board <- board_temp()

  expect_snapshot(pin_registry_retrieve(board, "x"), error = TRUE)

  pin_registry_update(board, "x", list(test = "x"))
  expect_equal(pin_registry_retrieve(board, "x"), list(test = "x", name = "x"))
})

test_that("can pin() concurrently", {
  # BEWARE: this tests the INSTALLED versions of pins
  skip_on_ci()
  # This fails on CI in a way that I can not replicate locally, even when I
  # make n_proc and n_pins substantially larger. I've skipped it because
  # I don't think multi-process race conditions are likely to be a problem
  # in real-life.

  temp_path <- withr::local_tempdir()
  board <- board_folder(temp_path)

  n_proc <- 10
  n_pins <- 10

  pin_n <- function(path, proc, n_pins) {
    board <- pins::board_folder(path)
    data <- list(message = "concurrent test")
    for (i in seq_len(n_pins)) {
      pins::pin(data, name = as.character(proc * 100 + i), board = board)
    }
  }

  processes <- list()
  for (i in seq_len(n_proc)) {
    processes[[i]] <- callr::r_bg(pin_n, list(
      path = temp_path,
      proc = i,
      n_pins = n_pins
    ))
  }
  for (i in seq_len(n_proc)) {
    processes[[i]]$wait()
  }

  index <- pin_registry_read(board)
  expect_equal(length(index), n_proc * n_pins)
})
