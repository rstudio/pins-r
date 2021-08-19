# informative error for legacy boards

    Code
      board <- legacy_temp()
      board %>% pin_delete("x")
    Error <rlang_error>
      Use `pin_remove()` with this board, not `pin_delete()`

