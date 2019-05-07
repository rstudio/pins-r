rstudio_dependencies <- function() {
  if (!"rsconnect" %in% installed.packages()) stop("Package 'rsconnect' needs to be installed to use a 'rstudio' board.")

  list(
    deploy_app = get("deployApp", envir = asNamespace("rsconnect")),
    resolve_account = get("resolveAccount", envir = asNamespace("rsconnect")),
    account_info = get("accountInfo", envir = asNamespace("rsconnect")),
    server_info = get("serverInfo", envir = asNamespace("rsconnect")),
    client_for_account = get("clientForAccount", envir = asNamespace("rsconnect")),
    accounts = get("accounts", envir = asNamespace("rsconnect")),
    parse_http_url = get("parseHttpUrl", envir = asNamespace("rsconnect")),
    get = get("GET", envir = asNamespace("rsconnect"))
  )
}

rstudio_account_info <- function(board) {
  deps <- rstudio_dependencies()

  account_name <- deps$resolve_account(account = board$account,
                                       server = board$server)

  deps$account_info(account_name)
}

rstudio_api_get <- function(board, path, root = FALSE) {
  deps <- rstudio_dependencies()

  server_info <- deps$server_info(board$server)
  service <- deps$parse_http_url(server_info$url)
  account_info <- rstudio_account_info(board)

  if (root) service$path = gsub("/__api__", "", service$path)

  deps$get(service, authInfo = account_info, path = path)
}

board_initialize.rstudio <- function(board, ...) {
  args <- list(...)
  deps <- rstudio_dependencies()

  board$server <- args$server
  board$account <- args$account

  if (is.null(args$server)) board$server <- deps$accounts()$server[1]

  board
}

pin_create.rstudio <- function(board, x, name, description, type, metadata) {
  deps <- rstudio_dependencies()

  temp_dir <- tempfile()
  dir.create(temp_dir)

  rds_file <- file.path(temp_dir, "data.rds")
  csv_file <- file.path(temp_dir, "data.csv")
  html_file <- file.path(temp_dir, "index.html")

  saveRDS(x, rds_file, version = 2)
  write.csv(x, csv_file, row.names = FALSE)
  file.copy(
    system.file("views/data/index.html", package = "pins"),
    html_file
  )

  app <- deps$deploy_app(dirname(csv_file),
                         appPrimaryDoc = basename(html_file),
                         lint = FALSE,
                         appName = paste0(name, "_pin"),
                         server = board$server,
                         account = board$account,
                         appTitle = name,
                         contentCategory = "data")

  unlink(csv_file)

  app
}

pin_find.rstudio <- function(board, text, ...) {
  deps <- rstudio_dependencies()
  extended <- identical(list(...)$extended, TRUE)
  everything <- identical(list(...)$everything, TRUE)

  account_info <- rstudio_account_info(board)
  client <- deps$client_for_account(account_info)

  results <- client$listApplications(accountId = account_info$accountId)
  results <- as.data.frame(do.call("rbind", results))

  if (!everything) results <- results[grepl("_pin$", results$name),]

  results$name <- gsub("_pin$", "", as.character(results$name))
  results$type <- "table"
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
  rstudio_api_get(board, paste0(gsub(".*/content", "/content", details$url), "data.csv"), root = TRUE)
}

pin_remove.rstudio <- function(board, name) {

}

board_info.memory <- function(board) {
}
