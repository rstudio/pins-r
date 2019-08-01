context("pin object")

test_that("can pin() object", {
  an_object <- data.frame(numbers = c(1, 2, 3),
                          text = c("a", "b", "c"),
                          stringsAsFactors = FALSE)

  roundtrip <- pin(an_object, "anobject")

  expect_equal(an_object, as.data.frame(roundtrip))
})
