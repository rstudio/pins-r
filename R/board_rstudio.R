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

rstudio_api_get <- function(board, url) {
  api_key <- rstudio_get_key(board)

  httr::content(httr::GET(
    url,
    add_headers(Authorization = paste("Key", api_key)),
    httr::timeout(as.integer(Sys.getenv("RSTUDIO_CONNECT_API_TIMEOUT", 3)))
  ))
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

pin_find.rstudio <- function(board, text, ...) {
  extended <- identical(list(...)$extended, TRUE)
  results <- rstudio_api_get(board, paste0(board$host, "/__api__/applications?count=100&search=", text, "&start=0"))
  results <- as.data.frame(do.call("rbind", results$applications))

  results$name <- as.character(results$name)
  results$type <- "files"
  results$metadata <- "{}"
  results$description <- as.character(lapply(results$title, function(e) paste0("", e)))

  if (extended) {
    results
  }
  else {
    results[c("name", "description", "type", "metadata")]
  }
}

pin_retrieve.rstudio <- function(board, name, details) {
  rstudio_api_get(board, as.character(details$url))
}

pin_remove.rstudio <- function(board, name) {
  stop("Not yet implemented!")
}

board_info.memory <- function(board) {
}
