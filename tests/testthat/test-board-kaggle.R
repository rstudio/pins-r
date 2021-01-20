context("board kaggle")

if (!has_envvars("TEST_KAGGLE_API")) {
  skip("requires env var TEST_KAGGLE_API")
}

path <- tempfile()
writeLines(Sys.getenv("TEST_KAGGLE_API"), path)

board_register_kaggle(
  name = "test-kaggle",
  token = path,
  cache = tempfile()
)
withr::defer(board_deregister("test-kaggle"))

test_that("can pin_find() 'seattle' in kaggle board", {
  searches <- pin_find("game of thrones", board = "kaggle")
  expect_gt(nrow(searches), 0)
})

test_that("can pin_get() 'got' in kaggle board", {
  dataset <- pin_get("gunnvant/game-of-thrones-srt", board = "kaggle")
  expect_gt(length(dataset), 0)
})
