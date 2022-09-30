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

# `full` is deprecated

    Code
      x <- pin_versions(board, "x", full = TRUE)
    Condition
      Warning:
      The `full` argument of `pin_versions()` is deprecated as of pins 1.0.0.

# can prune old versions

    Code
      pin_versions_prune(board, "x", n = 1)
    Message
      Deleting versions: 20120304T050607Z-3de1f, 20120304T050607Z-caa1b, 20120304T050607Z-e4e08
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

