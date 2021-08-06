test_that("can read and write simple pin", {
  board <- board_azure_test()
  name <- random_pin_name()

  pin_write(board, 1:3, name)
  expect_true(name %in% pin_list(board))
  expect_equal(pin_read(board, name), 1:3)

  pin_delete(board, name)
  expect_false(name %in% pin_list(board))
})

test_that("absent pins handled consistently", {
  board <- board_azure_test()
  name <- local_pin(board, 1)

  expect_true(name %in% pin_list(board))
  expect_equal(pin_exists(board, name), TRUE)
  expect_equal(pin_exists(board, "this_doesnt_exist"), FALSE)

  expect_error(pin_meta(board, "this_doesnt_exist"), class = "pins_pin_absent")
})

test_that("tracks versions as expected", {
  board <- board_azure_test()
  name <- local_pin(board, 1)

  versions <- pin_versions(board, name)
  expect_equal(nrow(versions), 1)
  pin_write(board, 2, name)
  pin_write(board, 3, name)
  expect_equal(nrow(pin_versions(board, name)), 3)

  x <- pin_read(board, name, version = versions$version[[1]])
  expect_equal(x, 1)
})

test_that("if versioning off, overwrites existing version", {
  board <- board_azure_test(versioned = FALSE)

  name <- local_pin(board, 1)
  expect_equal(nrow(pin_versions(board, name)), 1)

  pin_write(board, 2, name)
  expect_equal(nrow(pin_versions(board, name)), 1)
  expect_equal(pin_read(board, name), 2)
})

test_that("generates useful errors for missing pins/versions", {
  board <- board_azure_test()
  name <- local_pin(board, 1)
  pin_write(board, 2, name)

  expect_snapshot(error = TRUE, {
    board %>% pin_versions("missing")

    board %>% pin_read("missing")
    board %>% pin_read(name, version = 1)
    board %>% pin_read(name, version = "missing")

    board %>% pin_write(3, name, versioned = FALSE)
  })
})
