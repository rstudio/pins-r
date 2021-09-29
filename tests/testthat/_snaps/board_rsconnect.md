# can deparse

    Code
      board_deparse(board)
    Output
      board_rsconnect(server = "https://connect.rstudioservices.com")

# can find content by full/partial name

    Code
      rsc_content_find(board, "marjory/test-partial")
    Error <rlang_error>
      Can't find pin named 'test-partial' with owner 'marjory'

# can create and delete content

    Code
      rsc_content_create(board, "test-1", list())
    Error <rlang_error>
      RStudio Connect API failed [409]
      * An object with that name already exists.

---

    Code
      rsc_content_delete(board, "test-1")
    Error <pins_pin_missing>
      Can't find pin called 'test-1'
      i Use `pin_list()` to see all available pins in this board

