# informative error for legacy boards

    Code
      board <- legacy_temp()
      board %>% pin_search("x")
    Condition
      Error in `pin_search()`:
      ! Use `pin_find()` with this board, not `pin_search()`

