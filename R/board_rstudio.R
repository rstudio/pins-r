board_initialize.rstudio <- function(board, host, ...) {
  board$host <- host

  if (is.null(board$host)) board$host <- args[[1]]

  board
}

pin_create.rstudio <- function(board, x, name, description, type, metadata) {
  stop("Not yet implemented!")
}

pin_find.rstudio <- function(board, text) {
  api_key <- rstudioapi::askForSecret("pins_rstudio", paste("Please provide API key for", host))
  results <- httr::content(httr::GET(
    paste0(host, "/__api__/applications?count=100&search=&start=0"),
    add_headers(Authorization = paste("Key", api_key))
  ))

  View(as.data.frame(do.call("rbind", results$applications)))
}

pin_retrieve.rstudio <- function(board, name) {
  stop("Not yet implemented!")
}

pin_remove.rstudio <- function(board, name) {
  stop("Not yet implemented!")
}

board_info.memory <- function(board) {
}
