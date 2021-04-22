# pin_upload generated useful messages

    Code
      pin_upload(board, 1:10)
    Error <rlang_error>
      `path` must be a character vector
    Code
      pin_upload(board, "this-path-doesn't-exist")
    Error <rlang_error>
      All elements of `path` must exist
    Code
      path <- fs::file_touch(fs::path_temp("test.txt"))
      pin_upload(board, path)
    Message <message>
      Guessing `name = 'test.txt'`
      Creating new version 'ef46db3751d8e999'

# can pin file called data.txt

    Code
      pin_upload(board, fs::path(path, "data.txt"))
    Error <rlang_error>
      Can pin file called `data.txt`

