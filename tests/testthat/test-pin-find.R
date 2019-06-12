context("pin find")

test_that("can pin_find() entries across all boards", {
  expect_gt(nrow(pin_find()), 0)
  expect_gt(nrow(pin_find("")), 0)
})
