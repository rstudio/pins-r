# can find content by full/partial name

    Code
      rsc_content_find(board, "susan/test-partial")
    Error <rlang_error>
      Can't find pin named 'test-partial' with owner 'susan'

# can create and delete content

    Code
      rsc_content_create(board, "test-1", list())
    Error <rlang_error>
      RStudio Connect API failed [409]
      * An object with that name already exists.

---

    Code
      rsc_content_delete(board, "test-1")
    Error <pins_pin_absent>
      Can't find pin with name 'test-1'

