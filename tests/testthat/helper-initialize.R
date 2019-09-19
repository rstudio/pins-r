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

test_local_files <- function() {
  pin_folders <- dir(board_cache_path(), recursive = TRUE)

  if (!identical(pin_folders, character(0)))
    stop("Found local files: ", paste(pin_folders, collapse = ", "))

  succeed()
}
