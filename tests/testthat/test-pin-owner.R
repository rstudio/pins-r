context("pin owner")

test_that("can retrieve name with pin_content_name()", {
  expect_equal(
    pin_content_name("named-pin"),
    "named-pin"
  )
})

test_that("can retrieve empty owner with pin_content_owner()", {
  expect_equal(
    pin_content_owner("named-pin"),
    NULL
  )
})

test_that("can retrieve empty name with pin_content_owner()", {
  expect_equal(
    pin_content_owner(""),
    NULL
  )
})

test_that("can parse simple names with pin_content_owner() and pin_content_name()", {
  name <- "the-owner/named-pin"

  expect_equal(
    pin_content_owner(name),
    "the-owner"
  )

  expect_equal(
    pin_content_name(name),
    "named-pin"
  )
})

test_that("can parse simple names with pin_content_owner() and pin_content_name()", {
  name <- "parent-owner/the-owner/named-pin"

  expect_equal(
    pin_content_owner(name),
    "parent-owner/the-owner"
  )

  expect_equal(
    pin_content_name(name),
    "named-pin"
  )
})
