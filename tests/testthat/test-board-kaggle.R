context("board kaggle")

test_github_repo <- Sys.getenv("TEST_KAGGLE_API", "")
if (nchar(test_github_repo) > 0) {
  if ("kaggle" %in% board_list()) board_deregister("kaggle")
}

if (test_board_is_registered("kaggle")) {
  board_test("kaggle", exclude = "remove")
} else if (nchar(test_github_repo) > 0) {
  test_that("can board_register() kaggle board", {
    base64enc::base64decode(test_github_repo) %>%
      rawToChar() %>%
      writeLines("kaggle.json")

    board_register("kaggle", token = "kaggle.json")

    expect_true("kaggle" %in% board_list())
  })

  test_that("can pin_find() 'seattle' in kaggle board", {
    searches <- pin_find("game of thrones", board = "kaggle")
    expect_gt(nrow(searches), 0)
  })

  test_that("can pin_get() 'got' in kaggle board", {
    dataset <- pin_get("gunnvant/game-of-thrones-srt", board = "kaggle")
    expect_gt(length(dataset), 0)
  })
} else {
  test_that("test kaggle board", {
    skip("kaggle board is not registered")
  })
}
