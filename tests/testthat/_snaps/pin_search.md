# informative error for legacy boards

    Code
      board <- legacy_temp()
      board %>% pin_search("x")
    Error <rlang_error>
      Use `pin_find()` with this board, not `pin_search()`

