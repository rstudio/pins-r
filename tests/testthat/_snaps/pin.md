# unavailable url can use cache

    Code
      pin(url, "test", board = board)
    Condition
      Error in `pin()`:
      ! Client error: (404) Not Found. Failed to download remote file: http://127.0.0.1:<port>/status/404
    Code
      pin(1:10, "test", board = board)
      x <- pin(url, "test", board = board)
    Condition
      Warning:
      Failed to re-download pin; using cached value
      * Client error: (404) Not Found. Failed to download remote file: http://127.0.0.1:<port>/status/404
    Code
      expect_equal(x, 1:10)

# can pin() with custom metadata

    Code
      pin(iris, "iris2", board = board, custom_metadata = meta)
    Condition
      Warning:
      `custom_metadata` is deprecated; please use `metadata` instead

