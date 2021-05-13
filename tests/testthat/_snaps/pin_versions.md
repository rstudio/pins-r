# can use old pin_versions() api

    Code
      x <- pin_versions("x")
      x <- pin_versions("x", "local")
      x <- pin_versions("x", board)

# `full` is deprecated

    Code
      x <- pin_versions(board, "x", full = TRUE)
    Warning <lifecycle_warning_deprecated>
      The `full` argument of `pin_versions()` is deprecated as of pins 1.0.0.

