board_wait_create <- function(board, name) {
  retrieved <- NULL
  retries <- 10
  while (retries > 0 && is.null(retrieved)) {
    retrieved <- suppressWarnings(tryCatch({
      pin_get(name, board$name)
    }, error = function(e) NULL))

    Sys.sleep(1)
    retries <- retries - 1
  }
}
