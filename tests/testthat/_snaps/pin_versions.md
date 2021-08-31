# can use old pin_versions() api

    Code
      x <- pin_versions("x")
      x <- pin_versions("x", "local")
      x <- pin_versions("x", board)

# can't swap arguments with modern api

    Code
      pin_versions(name, board)
    Error <rlang_error>
      Please supply `board` then `name` when working with modern boards

# `full` is deprecated

    Code
      x <- pin_versions(board, "x", full = TRUE)
    Warning <lifecycle_warning_deprecated>
      The `full` argument of `pin_versions()` is deprecated as of pins 1.0.0.

# can prune old versions

    Code
      pin_versions_prune(board, "x", n = 1)
    Message <message>
      Deleting versions: 20120304T050607Z-3de1f, 20120304T050607Z-caa1b, 20120304T050607Z-e4e08
    Code
      pin_versions_prune(board, "x", n = 1)
    Message <message>
      No old versions to delete

# versions_keep() gives useful errors

    Code
      versions_keep(NULL)
    Error <rlang_error>
      Internal error: invalid result from pin_versions()
    Code
      versions_keep(Sys.time())
    Error <rlang_error>
      Must supply exactly one of `n` and `days`
    Code
      versions_keep(Sys.time(), n = "a")
    Error <rlang_error>
      `n` must be a single integer
    Code
      versions_keep(Sys.time(), days = "a")
    Error <rlang_error>
      `days` must be a single integer

