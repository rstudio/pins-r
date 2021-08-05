# can't accidentally switch from versioned to unversioned

    Code
      pin_write(board, df1, "test-df1", type = "rds", versioned = FALSE)
    Error <rlang_error>
      Pin is versioned, but you have requested not to use versions
      * To un-version this pin you will need to delete it

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

