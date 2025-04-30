# pin_upload generated useful messages

    Code
      pin_upload(board, 1:10)
    Condition
      Error in `pin_upload()`:
      ! `path` must be a character vector
    Code
      pin_upload(board, "this-path-doesn't-exist")
    Condition
      Error in `pin_upload()`:
      ! All elements of `paths` must exist
    Code
      path <- fs::file_touch(fs::path_temp("test.txt"))
      pin_upload(board, path)
    Message
      Guessing `name = 'test.txt'`
      Creating new version '20120304T050607Z-xxxxx'
    Code
      pin_upload(board, path, "test", c("blue", "green"))
    Condition
      Error in `pin_upload()`:
      ! Arguments after the dots `...` must be named, like `tags = "my-great-tag"`.

# can pin file called data.txt

    Code
      pin_upload(board, fs::path(path, "data.txt"))
    Condition
      Error in `pin_upload()`:
      ! Can't pin file called `data.txt`

