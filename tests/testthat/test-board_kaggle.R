# Kaggle API doesn't provide a way to delete datasets so we can't make
# use of the existing automated tests. Instead we test the reading as
# much as possible

# competitions ----------------------------------------------------------------

test_that("can list and search competitions", {
  board <- board_kaggle_competitions_test()
  expect_equal(pin_list(board), NA)

  search <- pin_search(board, "cats")
  expect_s3_class(search, "tbl_df")
  expect_named(search, c("name", "type", "description", "created", "deadline"))

  expect_true(pin_exists(board, "dogs-vs-cats"))
  expect_false(pin_exists(board, "doesntexist"))
  expect_snapshot(board_deparse(board))
})

test_that("can get data and metadata", {
  skip_on_cran()

  board <- board_kaggle_competitions_test()
  meta <- pin_meta(board, "house-prices-advanced-regression-techniques")
  expect_s3_class(meta, "pins_meta")
  expect_equal(meta$type, "file")

  paths <- pin_download(board, "house-prices-advanced-regression-techniques")

  data_dict <- paths[grepl("data_description", paths)]
  expect_match(readLines(data_dict)[[1]], "MSSubClass")
})

test_that("is write only", {
  board <- board_kaggle_competitions_test()
  expect_error(pin_write(board, 1, "test"), class = "pins_board_read_only")
  expect_error(pin_delete(board, "test"), class = "pins_board_read_only")
})

# datasets ----------------------------------------------------------------

test_that("can list and search datasets", {
  board <- board_kaggle_dataset_test()
  expect_equal(pin_list(board), NA)

  search <- pin_search(board, "cats")
  expect_s3_class(search, "tbl_df")
  expect_named(search, c("name", "type", "description", "created", "file_size", "license"))

  expect_true(pin_exists(board, search$name[[1]]))
  expect_false(pin_exists(board, "doesntexist/doesntexist"))
  expect_snapshot(board_deparse(board))
})

test_that("can retrieve data and metadata from dataset", {
  board <- board_kaggle_dataset_test()
  meta <- pin_meta(board, "hadleywickham1/mtcars")
  expect_s3_class(meta, "pins_meta")
  expect_equal(meta$file, "mtcars.csv")

  path <- pin_download(board, "hadleywickham1/mtcars")
  expect_equal(read.csv(path), `rownames<-`(mtcars, NULL))
})

test_that("can update and retrieve versions", {
  board <- board_kaggle_dataset_test()

  # pin_write(board, 1, "version-test2")
  v_old <- pin_versions(board, "hadleywickham1/version-test2")
  pin_write(board, 2, "version-test2")
  v_new <- pin_versions(board, "hadleywickham1/version-test2")
  expect_equal(v_new$version[[1]], v_old$version[[1]] + 1)

  path <- pin_download(board, "hadleywickham1/version-test2", version = 1)
  expect_equal(readRDS(path), 1)
})
