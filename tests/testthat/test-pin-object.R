context("pin object")

test_that("can pin() object", {
  an_object <- tibble::tibble(numbers = c(1, 2, 3),
                              text = c("a", "b", "c"))

  roundtrip <- pin(an_object, "anobject")

  expect_equal(an_object, roundtrip)
  expect_equal(an_object, pin_get("anobject"))
})
