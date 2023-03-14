# informative error for legacy boards

    Code
      board <- legacy_temp()
      board %>% pin_delete("x")
    Condition
      Error in `pin_delete()`:
      ! Use `pin_remove()` with this board, not `pin_delete()`

