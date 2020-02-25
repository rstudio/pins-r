context("pin object")

test_that("can pin() object", {
  an_object <- data.frame(numbers = c(1, 2, 3),
                          text = c("a", "b", "c"),
                          stringsAsFactors = FALSE)

  roundtrip <- pin(an_object, "anobject")

  expect_equal(an_object, as.data.frame(roundtrip))
})

test_that("can pin() concurrently", {

  temp_path <- tempfile()
  if (dir.exists(temp_path)) unlink(temp_path, recursive = TRUE)
  dir.create(temp_path)

  processes <- list()
  for (i in 1:10) {
    processes[[i]] <- callr::r_bg(function(temp_path, proc) {
      options(pins.path = temp_path)

      for (i in 1:10) {
        pins::pin(list(message = "concurrent test"), name = (proc * 100 + i), board = "local")
      }
    }, args = list(temp_path, i))
  }

  for (i in 1:10) {
    processes[[i]]$wait()
  }

  index <- yaml::read_yaml(file.path(temp_path, "local", "data.txt"))

  expect_equal(length(index), 100)
})
