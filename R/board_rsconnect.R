rsconnect_dependencies <- function() {
  list(
    current_input = get0("current_input", envir = asNamespace("knitr")),
    output_metadata = get0("output_metadata", envir = asNamespace("rmarkdown"))
  )
}

rsconnect_pins_supported <- function(board) {
  package_version(rsconnect_api_version(board)) > package_version("1.7.7")
}

board_initialize.rsconnect <- function(board, ...) {
  args <- list(...)

  board$server <- args$server
  board$server_name <- if (!is.null(args$server)) gsub("https?://|:[0-9]+/?", "", args$server) else NULL
  board$account <- args$account

  if (identical(args$key, "")) stop("Invalid API key, the API key is empty.")

  board$key <- args$key

  if (!is.null(board$key) && is.null(board$server)) {
    stop("Please specify the 'server' parameter when using API keys.")
  }

  if (!rsconnect_api_auth(board)) {
    board <- rsconnect_token_initialize(board)
  }

  board
}

board_load.rsconnect <- function(board) {
  board
}

board_persist.rsconnect <- function(board) {
  board
}

board_pin_create.rsconnect <- function(board, path, name, ...) {
  description <- if (is.null(list(...)$description)) "" else list(...)$description
  on.exit(board_connect(board$name))

  deps <- rsconnect_dependencies()

  temp_dir <- file.path(tempfile(), name)
  dir.create(temp_dir, recursive = TRUE)
  on.exit(unlink(temp_dir, recursive = TRUE))

  x <- if (identical(dir(path, "data\\.rds"), "data.rds"))
    readRDS(dir(path, "data\\.rds", full.names = TRUE)) else path

  account_name <- board$account
  if (is.null(account_name)) {
    account_name <- rsconnect_api_get(board, "/__api__/users/current/")$username
  }

  file.copy(dir(path, full.names = TRUE), temp_dir)
  data_files <- rsconnect_bundle_create(x, temp_dir, name, board, account_name)

  rsconnect_is_authenticated <- function(board) {
    !is.null(board$key) || !is.null(board$account)
  }

  is_knitting <- function() {
    !is.null(deps$current_input) && !is.null(deps$output_metadata) && !is.null(deps$current_input())
  }

  if (is_knitting() && !rsconnect_is_authenticated(board)) {
    # use rsc output files when not authenticated, warn if we thing we might not be running under RSC
    if (nchar(Sys.getenv("R_CONFIG_ACTIVE")) == 0)
      warning("Not authenticated to RStudio Connect, creating output file for pin.")

    knit_pin_dir <- file.path(name)
    file.copy(temp_dir, getwd(), recursive = TRUE)
    deps$output_metadata$set(rsc_output_files = file.path(knit_pin_dir, dir(knit_pin_dir, recursive = TRUE)))
  }
  else {
    existing <- rsconnect_get_by_name(board, name)
    if (nrow(existing) == 0) {
      content <- rsconnect_api_post(board,
                                  paste0("/__api__/v1/experimental/content"),
                                  list(
                                    app_mode = "static",
                                    content_category = "pin",
                                    name = name,
                                    description = description
                                  ))
      if (!is.null(content$error)) {
        stop("Failed to create pin: ", content$error)
      }

      guid <- content$guid
    }
    else {
      guid <- existing$guid
    }

    files <- lapply(dir(temp_dir, recursive = TRUE, full.names = TRUE), function(path) {
      list(
        checksum = rsconnect_bundle_file_md5(path)
      )
    })
    names(files) <- dir(temp_dir, recursive = TRUE)

    manifest <- list(
      version = 1,
      locale = "en_US",
      platform = "3.5.1",
      metadata = list(
        appmode = "static",
        primary_rmd = NA,
        primary_html = "index.html",
        content_category = "pin",
        has_parameters = FALSE
      ),
      packages = NA,
      files = files,
      users = NA
    )

    bundle <- rsconnect_bundle_compress(temp_dir, manifest)

    upload <- rsconnect_api_post(board,
                                 paste0("/__api__/v1/experimental/content/", guid, "/upload"),
                                 httr::upload_file(normalizePath(bundle)),
                                 http_utils_progress("up"))

    if (!is.null(upload$error)) {
      stop("Failed to upload pin: ", upload$error)
    }

    result <- rsconnect_api_post(board,
                                 paste0("/__api__/v1/experimental/content/", guid, "/deploy"),
                                 list(
                                   bundle_id = upload$bundle_id
                                 ))

    if (!is.null(result$error)) {
      stop("Failed to activate pin: ", result$error)
    }
  }
}

board_pin_find.rsconnect <- function(board, text, ...) {
  all_content  <- identical(list(...)$all_content, TRUE)

  if (is.null(text)) text <- ""

  if (nchar(text) == 0) {
    # it can be quite slow to list all content in RStudio Connect so we scope to the user content
    account_id <- rsconnect_api_get(board, "/__api__/users/current/")$id
    filter <- paste0("filter=account_id:", account_id, "&accountId:", account_id)
  }
  else {
    filter <- paste0("search=", text)
  }

  entries <- rsconnect_api_get(board, paste0("/__api__/applications/?", filter))$applications
  if (!all_content) entries <- Filter(function(e) e$content_category == "pin", entries)

  results <- pin_results_from_rows(entries)

  results$name <- sapply(entries, function(e) paste(e$owner_username, e$name, sep = "/"))

  if (nrow(results) == 0) {
    return(
      data.frame(name = c(), description = c(), type = c(), metadata = c())
    )
  }

  results$name <- as.character(results$name)
  results$type <- "files"
  results$description <- as.character(lapply(results$description, function(e) paste0("", e)))

  if (nrow(results) == 1) {
    # enhance with pin information
    remote_path <- rsconnect_remote_path_from_url(entries[[1]]$url)
    etag <- as.character(entries[[1]]$last_deployed_time)

    local_path <- rsconnect_api_download(board, entries[[1]]$name, file.path(remote_path, "data.txt"), etag = etag)
    manifest <- pin_manifest_get(local_path)

    manifest <- c(entries[[1]], manifest)

    results$type <- manifest$type
    results$metadata <- as.character(jsonlite::toJSON(manifest))
  }

  results
}

rsconnect_get_by_name <- function(board, name) {
  name_pattern <- if (grepl("/", name)) name else paste0(".*/", name, "$")
  only_name <- pin_content_name(name)

  details <- board_pin_find(board, only_name)
  details <- pin_results_extract_column(details, "content_category")
  details <- pin_results_extract_column(details, "url")
  details <- pin_results_extract_column(details, "guid")

  details <- details[grepl(name_pattern, details$name) & details$content_category == "pin",]

  if (nrow(details) > 1) {
    details <- details[details$owner_username == board$account,]
  }

  if (nrow(details) > 1) {
    stop("Multiple pins named '", name, "' in board '", board$name, "'")
  }

  details
}

rsconnect_remote_path_from_url <- function(url) {
  url <- gsub("/$", "", url)
  gsub("//", "/", file.path("/content", gsub("(^.*/|^)content/", "", url)))
}

board_pin_get.rsconnect <- function(board, name) {
  url <- name

  if (!grepl("^http://|^https://|^/content/", name)) {
    details <- rsconnect_get_by_name(board, name)
    url <- details$url
    etag <- as.character(details$last_deployed_time)
  }

  remote_path <- rsconnect_remote_path_from_url(url)

  local_path <- rsconnect_api_download(board, name, file.path(remote_path, "data.txt"), etag = etag)
  manifest <- pin_manifest_get(local_path)

  for (file in manifest$path) {
    rsconnect_api_download(board, name, file.path(remote_path, file), etag = etag)
  }

  unlink(dir(local_path, "index\\.html$|pagedtable-1\\.1$", full.names = TRUE))

  local_path
}

board_pin_remove.rsconnect <- function(board, name) {
  details <- rsconnect_get_by_name(board, name)
  details <- pin_results_extract_column(details, "guid")

  invisible(rsconnect_api_delete(board, paste0("/__api__/v1/experimental/content/", details$guid)))
}

board_browse.rsconnect <- function(board) {
  utils::browseURL(paste0("http://", board$server))
}
