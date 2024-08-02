test_that("rsc_content_find correctly filters multiple results by owner name", {
  local_mocked_bindings(
    rsc_GET = function(board, path, query) {
      list(
        list(
          name = "penguins",
          owner_guid = "taylor_guid",
          guid = "bad_content_guid",
          content_url = "wrong.url"
        ),
        list(
          name = "penguins",
          owner_guid = "toph_guid",
          guid = "good_content_guid",
          content_url = "correct.url"
        )
      )
    },
    rsc_user_name = function(board, guid) {
      c(taylor_guid = "taylor", toph_guid = "toph")[[guid]]
    }
  )
  found <- rsc_content_find("a board", "toph/penguins")
  expect_identical(found, list(guid = "good_content_guid", url = "correct.url"))
})
