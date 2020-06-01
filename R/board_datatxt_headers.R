board_datatxt_headers <- function(board, path, verb = "GET", file = NULL) {
  if (!is.null(board$url)) {
    # remove base url form path since S3 and others require relative paths when using custom domains
    path <- gsub(paste0("^", board$url, "/?"), "", path)
  }

  if (is.list(board$headers)) {
    httr::add_headers(.headers = unlist(board$headers))
  }
  else if (is.character(board$headers)) {
    httr::add_headers(.headers = board$headers)
  }
  else if ("request" %in% class(board$headers) || is.null(board$headers)) {
    board$headers
  }
  else if (is.function(board$headers)) {
    board$headers(board, verb, path, file)
  }
  else {
    stop("Unsupported '", class(board$headers)[[1]], "' class for board headers.")
  }
}
