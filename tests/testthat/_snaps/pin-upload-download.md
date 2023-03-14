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

# can pin file called data.txt

    Code
      pin_upload(board, fs::path(path, "data.txt"))
    Condition
      Error in `pin_upload()`:
      ! Can't pin file called `data.txt`

# informative error for legacy boards

    Code
      board <- legacy_temp()
      board %>% pin_upload(1:10, "x")
    Condition
      Error in `pin_upload()`:
      ! Use `pin()` with this board, not `pin_upload()`
    Code
      board %>% pin_download("x")
    Condition
      Error in `pin_download()`:
      ! Use `pin_get()` with this board, not `pin_download()`

