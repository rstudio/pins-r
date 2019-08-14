context("board metadata")

test_that("can roundtrip table metadata with no description", {
  metadata <- list(type = "table", rows = 10, cols = 5)

  board_metadata_to_text(metadata, "") %>%
    board_metadata_from_text() %>%
    expect_equal(metadata)
})

test_that("can roundtrip table metadata with description", {
  metadata <- list(type = "table", rows = 10, cols = 5)

  board_metadata_to_text(metadata, "A description.") %>%
    board_metadata_from_text() %>%
    expect_equal(metadata)
})

test_that("can roundtrip files metadata with no description", {
  metadata <- list(type = "files", extension = "xls")

  board_metadata_to_text(metadata, "") %>%
    board_metadata_from_text() %>%
    expect_equal(metadata)
})

test_that("can roundtrip files metadata with description", {
  metadata <- list(type = "files", extension = "xls")

  board_metadata_to_text(metadata, "A description.") %>%
    board_metadata_from_text() %>%
    expect_equal(metadata)
})

test_that("can read empty metadata from empty text", {
  board_metadata_from_text("") %>%
    expect_equal(list())

  board_metadata_from_text(NULL) %>%
    expect_equal(list())
})

test_that("can read empty metadata from invalid text", {
  board_metadata_from_text("Some other text with no metadata.") %>%
    expect_equal(list())
})

test_that("can read empty metadata from semi-valid text", {
  board_metadata_from_text("A table pin with 1 rows and abc columns.") %>%
    expect_equal(list())
})
