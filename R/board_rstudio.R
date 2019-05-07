rstudio_dependencies <- function() {
  if (!"rsconnect" %in% installed.packages()) stop("Package 'rsconnect' needs to be installed to use a 'rstudio' board.")

  rsconnect <- list(
    deployApp = get("deployApp", envir = asNamespace("rsconnect"))
  )

  if ("rstudioapi" %in% installed.packages() &&
      get("isAvailable", envir = asNamespace("rstudioapi"))()) {
    rspapi <- list (
      get_secret = get("askForSecret", envir = asNamespace("rstudioapi"))
    )
  }
  else {
    rspapi <- list (
      get_secret = function(name, message, title = NULL) {
        readline(paste(message, ": "))
      }
    )
  }

  c(rsconnect, rspapi)
}

rstudio_api_get <- function(board, url) {
  api_key <- rstudio_get_key(board)

  httr::content(httr::GET(
    url,
    httr::add_headers(Authorization = paste("Key", api_key)),
    httr::timeout(as.integer(Sys.getenv("RSTUDIO_CONNECT_API_TIMEOUT", 3)))
  ))
}

rstudio_api_post <- function(board, url, object) {
  api_key <- rstudio_get_key(board)

  httr::content(httr::POST(
    url,
    httr::add_headers(Authorization = paste("Key", api_key)),
    httr::timeout(as.integer(Sys.getenv("RSTUDIO_CONNECT_API_TIMEOUT", 3))),
    body = object
  ))
}

rstudio_get_key <- function(board) {
  deps <- rstudio_dependencies()

  env_key <- Sys.getenv("RSTUDIO_CONNECT_API_KEY")
  if (nchar(env_key) > 0)
    return(api_key)

  deps$get_secret("pins_rstudio", paste("Please provide API key for", board$host))
}

board_initialize.rstudio <- function(board, ...) {
  args <- list(...)

  board$server <- args$server
  board$account <- args$account

  board
}

pin_create.rstudio <- function(board, x, name, description, type, metadata) {
  deps <- rstudio_dependencies()

  csv_file <- tempfile(fileext = ".csv")
  write.csv(x, csv_file, row.names = FALSE)

  app <- deps$deployApp(dirname(csv_file),
                        appPrimaryDoc = basename(csv_file),
                        lint = FALSE,
                        appName = name,
                        server = board$server,
                        account = board$account,
                        contentCategory = "data"
                        )

  unlink(csv_file)

  app
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

}

board_info.memory <- function(board) {
}
