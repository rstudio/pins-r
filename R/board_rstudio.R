rstudio_dependencies <- function() {
  if ("rstudioapi" %in% installed.packages() &&
      get("isAvailable", envir = asNamespace("rstudioapi"))()) {
    list (
      get_secret = get("askForSecret", envir = asNamespace("rstudioapi"))
    )
  }
  else {
    list (
      get_secret = function(name, message, title = NULL) {
        readline(paste(message, ": "))
      }
    )
  }
}

rstudio_get_key <- function(board) {
  deps <- rstudio_dependencies()

  env_key <- Sys.getenv("RSTUDIO_CONNECT_API_KEY")
  if (nchar(env_key) > 0)
    return(api_key)

  deps$get_secret("pins_rstudio", paste("Please provide API key for", board$host))
}

board_initialize.rstudio <- function(board, host = NULL, ...) {
  args <- list(...)

  env_host <- Sys.getenv("RSTUDIO_CONNECT_SERVER")
  if (is.null(host) && nchar(env_host) > 0) host <- env_host

  board$host <- host

  if (is.null(board$host)) stop("Parameter 'host' must be specified while initializing RStudio board.")

  board
}

pin_create.rstudio <- function(board, x, name, description, type, metadata) {
  stop("Not yet implemented!")
}

pin_find.rstudio <- function(board, text) {
  api_key <- rstudio_get_key(board)
  results <- httr::content(httr::GET(
    paste0(board$host, "/__api__/applications?count=100&search=&start=0"),
    add_headers(Authorization = paste("Key", api_key))
  ))

  as.data.frame(do.call("rbind", results$applications))
}

pin_retrieve.rstudio <- function(board, name) {
  stop("Not yet implemented!")
}

pin_remove.rstudio <- function(board, name) {
  stop("Not yet implemented!")
}

board_info.memory <- function(board) {
}
