rstudio_dependencies <- function() {
  if (!"rsconnect" %in% installed.packages()) stop("Package 'rsconnect' needs to be installed to use a 'rstudio' board.")

  list(
    deploy_app = get("deployApp", envir = asNamespace("rsconnect")),
    resolve_account = get("resolveAccount", envir = asNamespace("rsconnect")),
    account_info = get("accountInfo", envir = asNamespace("rsconnect")),
    client_for_account = get("clientForAccount", envir = asNamespace("rsconnect"))
  )
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

  app <- deps$deploy_app(dirname(csv_file),
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
  deps <- rstudio_dependencies()
  extended <- identical(list(...)$extended, TRUE)

  account_name <- deps$resolve_account(account = NULL, server = board$server)
  account_info <-deps$account_info(account_name)
  client <- deps$client_for_account(account_info)

  results <- client$listApplications(accountId = account_info$accountId)
  results <- as.data.frame(do.call("rbind", results))

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
