rstudio_dependencies <- function() {
  if (!"rsconnect" %in% installed.packages()) stop("Package 'rsconnect' needs to be installed to use a 'rstudio' board.")

  list(
    deploy_app = get("deployApp", envir = asNamespace("rsconnect")),
    resolve_account = get("resolveAccount", envir = asNamespace("rsconnect")),
    account_info = get("accountInfo", envir = asNamespace("rsconnect")),
    server_info = get("serverInfo", envir = asNamespace("rsconnect")),
    accounts = get("accounts", envir = asNamespace("rsconnect")),
    parse_http_url = get("parseHttpUrl", envir = asNamespace("rsconnect")),
    get = get("GET", envir = asNamespace("rsconnect")),
    account_config_file = get("accountConfigFile", envir = asNamespace("rsconnect")),
    register_user_token = get("registerUserToken", envir = asNamespace("rsconnect")),
    list_request = get("listRequest", envir = asNamespace("rsconnect")),
    signature_headers = get("signatureHeaders", envir = asNamespace("rsconnect")),
    current_input = get0("current_input", envir = asNamespace("knitr")),
    output_metadata = get0("output_metadata", envir = asNamespace("rmarkdown"))
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

  if (root) service$path <- gsub("/__api__", "", service$path)

  deps$get(service, authInfo = account_info, path = path)
}

rstudio_api_download <- function(board, path, download) {
  deps <- rstudio_dependencies()

  deps <- rstudio_dependencies()

  if (rstudio_board_api_auth(board)) {
    headers <- list("Authorization" = paste("Key", board$key))

    url <- paste0(board$server, path)
  } else {
    server_info <- deps$server_info(board$server)
    service <- deps$parse_http_url(server_info$url)
    account_info <- rstudio_account_info(board)

    server_info$url <- gsub("/__api__", "", server_info$url)

    headers <- deps$signature_headers(account_info, "GET", path, NULL)

    url <- paste0(server_info$url, path)
    httr::GET(url,
              httr::write_disk(download),
              httr::add_headers(.headers = unlist(headers)))
  }

  httr::GET(url,
            httr::write_disk(download),
            httr::add_headers(.headers = unlist(headers)))
}

rstudio_api_version <- function(board) {
  jsonlite::fromJSON(rstudio_api_get(board, "/server_settings")$content)$version
}

rstudio_api_pins_supported <- function(board) {
  package_version(rstudio_api_version(board)) > package_version("1.7.7")
}

rstudio_pkg_supported <- function() {
  packageVersion("rsconnect") > package_version("0.8.13") &&
    !identical(get0("deployResource", envir = asNamespace("rsconnect")), NULL)
}

rstudio_board_api_auth <- function(board) !is.null(board$key)

board_initialize.rstudio <- function(board, ...) {
  args <- list(...)
  deps <- rstudio_dependencies()

  board$server <- args$server
  board$account <- args$account
  board$key <- args$key

  if (!is.null(board$key) && is.null(board$server)) {
    stop("Please specify the 'server' parameter when using API keys.")
  }

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

  if (!rstudio_board_api_auth(board)) {
    accounts <- deps$accounts()
    if (is.null(args$server)) board$server <- accounts$server[1]
    if (is.null(args$account)) board$account <- accounts[accounts$server == board$server,]$name
  }

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

rstudio_template_index_html <- function(temp_dir, template, value) {
  html_file <- file.path(temp_dir, "index.html")
  html_index <- readLines(html_file)
  html_index <- gsub(paste0("\\{\\{", template, "\\}\\}"), value, html_index)
  writeLines(html_index, html_file)
}

rstudio_create_pin.data.frame <- function(x, temp_dir) {
  rds_file <- file.path(temp_dir, "data.rds")
  csv_file <- file.path(temp_dir, "data.csv")

  saveRDS(x, rds_file, version = 2)

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
    data = head(x, n = max_rows),
    options = list(
      columns = list( max = 10 ),
      rows = list ( min = 1, total = nrow(x))
    )
  )

  rstudio_template_index_html(temp_dir, "data_preview", jsonlite::toJSON(data_preview))
}

rstudio_create_pin.default <- function(x, temp_dir) {
  html_file <- file.path(temp_dir, "index.html")

  file.copy(
    dir(system.file("views/files", package = "pins"), full.names = TRUE),
    temp_dir,
    recursive = TRUE)

  saveRDS(x, file.path(temp_dir, "data.rds"), version = 2)

  files <- dir(temp_dir, recursive = TRUE)
  files <- files[!grepl("index\\.html", files)]

  rstudio_template_index_html(temp_dir, "file_name", paste(files, collapse = "\n"))
}

rstudio_create_pin.character <- function(x, temp_dir) {
  html_file <- file.path(temp_dir, "index.html")

  file.copy(
    dir(system.file("views/files", package = "pins"), full.names = TRUE),
    temp_dir,
    recursive = TRUE)

  file.copy(x, temp_dir)

  files <- dir(temp_dir, recursive = TRUE)
  files <- files[!grepl("index\\.html", files)]

  rstudio_template_index_html(temp_dir, "file_name", files)
}

rstudio_create_pin <- function(x, temp_dir) {
  UseMethod("rstudio_create_pin")
}

rstudio_is_authenticated <- function(board) {
  !is.null(board$account)
}

is_knitting <- function(deps) {
  !is.null(deps$current_input) && !is.null(deps$output_metadata) && !is.null(deps$current_input())
}

board_pin_create.rstudio <- function(board, path, name, description, type, metadata, ...) {
  on.exit(board_connect(board$name))

  deps <- rstudio_dependencies()

  temp_dir <- file.path(tempfile(), name)
  dir.create(temp_dir, recursive = TRUE)
  on.exit(unlink(temp_dir, recursive = TRUE))

  x <- if (length(dir(path)) == 1 && identical(tools::file_ext(dir(path)), "rds"))
    readRDS(dir(path, full.names = TRUE)) else path

  rstudio_create_pin(x, temp_dir)
  pin_manifest_create(temp_dir, type, metadata)

  if (is_knitting(deps) && !rstudio_is_authenticated(board)) {
    # use rsc output files when not authenticated, warn if we thing we might not be running under RSC
    if (nchar(Sys.getenv("R_CONFIG_ACTIVE")) == 0)
      warning("Not authenticated to RStudio Connecet, creating output file for pin.")

    knit_pin_dir <- file.path(name)
    file.copy(temp_dir, getwd(), recursive = TRUE)
    deps$output_metadata$set(rsc_output_files = file.path(knit_pin_dir, dir(knit_pin_dir, recursive = TRUE)))
  }
  else if (rstudio_pkg_supported()) {
    rsconnect::deployResource(temp_dir,
                              name = name,
                              server = board$server,
                              account = board$account,
                              appTitle = name)
  }
  else {
    deps$deploy_app(temp_dir,
                    appPrimaryDoc = "index.html",
                    lint = FALSE,
                    appName = name,
                    server = board$server,
                    account = board$account,
                    appTitle = name,
                    contentCategory = "data")
  }

  pin_get(name, board$name)
}

rstudio_list_request <- function(board, filter) {
  deps <- rstudio_dependencies()

  if (rstudio_board_api_auth(board)) {
    result <- httr::GET(paste0(board$server, "/__api__/applications/?", filter),
                        httr::add_headers("Authorization" = paste("Key", board$key))) %>%
      httr::content()

    result$applications
  } else {
    account_info <- rstudio_account_info(board)

    server_info <- deps$server_info(board$server)
    service <- deps$parse_http_url(server_info$url)

    deps$list_request(service, account_info, "/applications", filter, "applications")
  }
}

board_pin_find.rstudio <- function(board, text, ...) {
  deps <- rstudio_dependencies()
  extended <- identical(list(...)$extended, TRUE)

  if (is.null(text)) text <- ""

  if (nchar(text) == 0) {
    # it can be quite slow to list all content in RStudio Connect so we scope to the user content
    account_info <- rstudio_account_info(board)
    filter <- paste0("filter=account_id:", account_info$accountId, "&accountId:", account_info$accountId)
  }
  else {
    filter <- paste0("search=", text)
  }

  results <- rstudio_list_request(board, filter)

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
  url <- name

  if (!grepl("^http://|^https://|^/content/", name)) {
    name_pattern <- if (grepl("/", name)) name else paste0(".*/", name)
    only_name <- pin_content_name(name)

    details <- board_pin_find(board, only_name, extended = TRUE)

    details <- details[grepl(name_pattern, details$name) & details$content_category == "data",]

    if (nrow(details) > 1) {
      details <- details[details$owner_username == board$account,]
    }

    if (nrow(details) > 1) {
      stop("Multiple pins named '", name, "' in board '", board$name, "'")
    }

    url <- details$url
  }

  url <- gsub("/$", "", url)
  remote_path <- gsub("//", "/", file.path("/content", gsub("(^.*/|^)content/", "", url)))

  local_path <- tempfile()
  dir.create(local_path)

  rstudio_api_download(board, file.path(remote_path, "pin.json"), file.path(local_path, "pin.json"))
  manifest <- jsonlite::read_json(file.path(local_path, "pin.json"))

  for (file in manifest$files) {
    rstudio_api_download(board, file.path(remote_path, file), file.path(local_path, file))
  }

  unlink(dir(local_path, "index\\.html$|pagedtable-1\\.1$|pin\\.json$", full.names = TRUE))

  attr(local_path, "pin_type") <- pin_manifest_get(local_path)$type
  local_path
}

board_pin_remove.rstudio <- function(board, name) {
  stop("Removing pins from 'rstudio' boards is currently unsupported.")
}

board_info.rstudio <- function(board) {
}
