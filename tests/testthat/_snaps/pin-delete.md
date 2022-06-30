# informative error for legacy boards

    Code
      board <- legacy_temp()
      board %>% pin_delete("x")
    Condition
      Error in `this_not_that()`:
      ! Use `pin_remove()` with this board, not `pin_delete()`

