board_register("local", cache = tempfile(), connect = FALSE)
board_register("packages", cache = tempfile(), connect = FALSE)

test_board_is_registered <- function(board) {
  tryCatch({
    if (!board %in% board_list()) {
      board_register(board)
      TRUE
    }
    else {
      TRUE
    }
  }, error = function(e) {
    FALSE
  })
}
