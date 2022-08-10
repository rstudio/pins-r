# unavailable url can use cache

    Code
      pin("http://httpstat.us/404", "test", board = board)
    Condition
      Error in `pin()`:
      ! Client error: (404) Not Found. Failed to download remote file: http://httpstat.us/404
    Code
      pin(1:10, "test", board = board)
      x <- pin("http://httpstat.us/404", "test", board = board)
    Condition
      Warning:
      Failed to re-download pin; using cached value
      * Client error: (404) Not Found. Failed to download remote file: http://httpstat.us/404
    Code
      expect_equal(x, 1:10)

# can pin() with custom metadata

    Code
      pin(iris, "iris2", board = board, custom_metadata = meta)
    Condition
      Warning:
      `custom_metadata` is deprecated; please use `metadata` instead

