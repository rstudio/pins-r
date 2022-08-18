# informative error for legacy boards

    Code
      board <- legacy_temp()
      board %>% pin_search("x")
    Condition
      Error in `this_not_that()`:
      ! Use `pin_find()` with this board, not `pin_search()`

