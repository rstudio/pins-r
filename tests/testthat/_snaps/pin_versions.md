# can use old pin_version() api

    Code
      x <- pin_versions("x")
    Warning <warning>
      `pin_version()` now takes `board` as first argument
    Code
      x <- pin_versions("x", "local")
    Warning <warning>
      `pin_version()` now takes `board` as first argument
    Code
      x <- pin_versions("x", board)
    Warning <warning>
      `pin_version()` now takes `board` as first argument

# `full` is deprecated

    Code
      x <- pin_versions(board, "x", full = TRUE)
    Warning <lifecycle_warning_deprecated>
      The `full` argument of `pin_versions()` is deprecated as of pins 1.0.0.

