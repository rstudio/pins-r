# can use old pin_versions() api

    Code
      x <- pin_versions("x")
      x <- pin_versions("x", "local")
      x <- pin_versions("x", board)

# can't swap arguments or omit name with modern api

    Code
      pin_versions(name, board)
    Condition
      Error in `pin_versions()`:
      ! Please supply `board` then `name` when working with modern boards

---

    Code
      pin_versions(board)
    Condition
      Error in `pin_versions()`:
      ! Argument `name` is missing, with no default

# informative error for writing with same version

    Code
      board %>% pin_write(1:10, "x")
      board %>% pin_write(1:10, "x", force_identical_write = TRUE)
    Condition
      Error in `pin_store()`:
      ! The new version "20120304T050607Z-xxxxx" is the same as the most recent version.
      i Did you try to create a new version with the same timestamp as the last version?

# can prune old versions

    Code
      pin_versions_prune(board, "x", n = 1)
    Message
      Deleting versions: 20130104T050607Z-xxxxx, 20130204T050607Z-yyyyy, 20130304T050607Z-zzzzz
    Code
      pin_versions_prune(board, "x", n = 1)
    Message
      No old versions to delete

# versions_keep() gives useful errors

    Code
      versions_keep(NULL)
    Condition
      Error in `versions_keep()`:
      ! Internal error: invalid result from pin_versions()
    Code
      versions_keep(Sys.time())
    Condition
      Error in `versions_keep()`:
      ! Must supply exactly one of `n` and `days`
    Code
      versions_keep(Sys.time(), n = "a")
    Condition
      Error in `versions_keep()`:
      ! `n` must be a single integer
    Code
      versions_keep(Sys.time(), days = "a")
    Condition
      Error in `versions_keep()`:
      ! `days` must be a single integer

