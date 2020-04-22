context("pin metadata")

test_that("can pin() with custom metadata", {
  skip_on_cran()

  pin(iris, "iris-metadata", custom_metadata = list(
    source = "The R programming language",
    columns = list(
      list(name = "Species", description = "Really like this column"),
      list(name = "Sepal.Length", description = "Sepal Length"),
      list(name = "Sepal.Width", description = "Sepal Width"),
      list(name = "Petal.Length", description = "Petal Length"),
      list(name = "Petal.Width", description = "Petal Width"))
    )
  )

  info <- pin_info("iris-metadata", board = "local")

  expect_equal(length(info$columns), 5L)
  expect_equal(names(info$columns[[1]]), c("name", "type", "description"))
  expect_equal(info$source, "The R programming language")
})

