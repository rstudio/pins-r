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
    get = get("GET", envir = asNamespace("rsconnect")),
    account_config_file = get("accountConfigFile", envir = asNamespace("rsconnect")),
    register_user_token = get("registerUserToken", envir = asNamespace("rsconnect")),
    list_request = get("listRequest", envir = asNamespace("rsconnect"))
  )
}

rstudio_account_info <- function(board) {
  deps <- rstudio_dependencies()

  account_name <- deps$resolve_account(account = board$account,
                                       server = board$server)

  deps$account_info(account_name)
}

rstudio_account_dcf <- function(board) {
  deps <- rstudio_dependencies()

  server <- deps$server_info(board$server)

  secret <- read.dcf(deps$account_config_file(board$account, board$server))
  secret <- cbind(secret, data.frame(url = server$url))

  temp_dcf <- tempfile(fileext = ".dcf")
  on.exit(unlink(temp_dcf))
  write.dcf(secret, temp_dcf)
  cat(base64enc::base64encode(temp_dcf))
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

  if (!is.null(args$secret) && nchar(args$secret) > 0) {
    temp_dcf <- tempfile(fileext = ".dcf")
    writeBin(base64enc::base64decode(args$secret), temp_dcf)
    secret <- read.dcf(temp_dcf)
    on.exit(unlink(temp_dcf))

    rsconnect::addServer(
      as.character(secret[,"url"]),
      as.character(secret[,"server"]))

    deps$register_user_token(
      serverName = as.character(secret[,"server"]),
      userId = as.character(secret[,"accountId"]),
      accountName = as.character(secret[,"username"]),
      token = as.character(secret[,"token"]),
      privateKey = as.character(secret[,"private_key"])
    )
  }

  accounts <- deps$accounts()
  if (is.null(args$server)) board$server <- accounts$server[1]
  if (is.null(args$account)) board$account <- accounts[accounts$server == board$server,]$name

  board$secret <- function() rstudio_account_dcf(board)

  board
}

board_load.rstudio <- function(board) {
  board$secret <- function() rstudio_account_dcf(board)
  board
}

board_persist.rstudio <- function(board) {
  board$secret <- NULL
  board
}

rstudio_create_pin.data.frame <- function(x, temp_dir) {
  rds_file <- file.path(temp_dir, "data.rdsb64")
  csv_file <- file.path(temp_dir, "data.csv")

  write.csv(x, csv_file, row.names = FALSE)
  writeLines(base64enc::base64encode(serialize(x, NULL, version = 2)), rds_file)

  file.copy(
    dir(system.file("views/data", package = "pins"), full.names = TRUE),
    temp_dir,
    recursive = TRUE)

  max_rows <- min(nrow(x), getOption("pins.preview.rows", 10^4))
  data_preview <- list(
    columns = lapply(colnames(x), function(e) {
      list(
        align = "right",
        label = e,
        name = e,
        type = ""
      )
    }),
    data = x[1:max_rows,],
    options = list(
      columns = list( max = 10 ),
      rows = list ( min = 1, total = nrow(x))
    )
  )

  html_file <- file.path(temp_dir, "index.html")
  html_index <- readLines(html_file)
  html_index <- gsub("\\{\\{data_preview\\}\\}", jsonlite::toJSON(data_preview), html_index)
  writeLines(html_index, html_file)
}

rstudio_create_pin.default <- function(x, temp_dir) {
  path <- tempfile(fileext = ".rdsb64")
  writeLines(base64enc::base64encode(serialize(x, NULL, version = 2)), path)
  on.exit(unlink(path))

  file.copy(path, temp_dir)
}

rstudio_create_pin.character <- function(x, temp_dir) {
  path <- tempfile(fileext = ".rdsb64")
  writeLines(base64enc::base64encode(x), path)
  file.copy(path, temp_dir)
}

rstudio_create_pin.default <- function(x, temp_dir) {
  rds_file <- file.path(temp_dir, "data.rdsb64")
  writeLines(base64enc::base64encode(serialize(x, NULL, version = 2)), rds_file)
}

rstudio_create_pin <- function(x, temp_dir) {
  UseMethod("rstudio_create_pin")
}

board_create_pin.rstudio <- function(board, path, name, description, type, metadata) {
  deps <- rstudio_dependencies()

  temp_dir <- tempfile()
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE))

  x <- if (identical(tools::file_ext(path), "rds")) readRDS(path) else path

  rstudio_create_pin(x, temp_dir)

  deps$deploy_app(temp_dir,
                  appPrimaryDoc = "index.html",
                  lint = FALSE,
                  appName = name,
                  server = board$server,
                  account = board$account,
                  appTitle = name,
                  contentCategory = "data")

  pin_get(name, board$name)
}

board_find_pin.rstudio <- function(board, text, ...) {
  deps <- rstudio_dependencies()
  extended <- identical(list(...)$extended, TRUE)

  if (is.null(text)) text <- ""

  account_info <- rstudio_account_info(board)
  client <- deps$client_for_account(account_info)

  server_info <- deps$server_info(board$server)
  service <- deps$parse_http_url(server_info$url)

  if (nchar(text) == 0) {
    # it can be quite slow to list all content in RStudio Connect so we scope to the user content
    apps_filter <- paste0("filter=account_id:", account_info$accountId, "&accountId:", account_info$accountId)
    results <- deps$list_request(service, account_info, "/applications", apps_filter, "applications")
  }
  else {
    results <- deps$list_request(service, account_info, "/applications", paste0("search=", text), "applications")
  }

  results <- as.data.frame(do.call("rbind", results))

  results <- results[results$content_category == "data",]

  results$name <- paste(results$owner_username, results$name, sep = "/")

  if (nrow(results) == 0) {
    return(
      data.frame(name = c(), description = c(), type = c(), metadata = c())
    )
  }

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

board_pin_get.rstudio <- function(board, name, details) {
  name_pattern <- if (grepl("/", name)) name else paste0(".*/", name)
  only_name <- pin_without_owner(name)

  details <- board_find_pin(board, only_name, extended = TRUE)

  details <- details[grepl(name_pattern, details$name) & details$content_category == "data",]

  if (nrow(details) > 1) details <- details[details$owner_username == board$account,]
  if (nrow(details) > 1) stop("Multiple pins named '", name, "' in board '", board$name, "'")
  if (nrow(details) == 0) stop("Pin '", name, "' not found in board '", board$name, "'")

  rdsb64_path <- rstudio_api_get(board, paste0(gsub(".*/content", "/content", details$url), "data.rdsb64"), root = TRUE)

  path <- tempfile()
  writeBin(base64enc::base64decode(rdsb64_path$content), path)
  attr(path, "pin_type") <- details$type
  path
}

board_remove_pin.rstudio <- function(board, name) {
  stop("Removing pins from 'rstudio' boards is currently unsupported.")
}

board_info.memory <- function(board) {
}
