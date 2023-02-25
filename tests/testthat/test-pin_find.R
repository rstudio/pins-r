skip_if_not_installed("filelock")

test_that("can pin_find() entries across all boards", {
  withr::local_options(lifecycle_verbosity = "quiet")

  local_register(legacy_temp("test1"))
  local_register(legacy_temp("test2"))
  pin(list(x = 1), "one", board = "test1")
  pin(list(x = 2), "two", board = "test2")

  out <- pin_find()
  expect_equal(out$name[out$board != "local"], c("one", "two"))

  out <- pin_find("one", board = c("test1", "test2"))
  expect_equal(out$name, "one")

  out <- pin_find("one", board = "test1")
  expect_equal(out$name, "one")

  out <- pin_find("one", board = character())
  expect_equal(out$name, character())

  out <- pin_find("one", board = board_get("test1"))
  expect_equal(out$name, "one")
})
