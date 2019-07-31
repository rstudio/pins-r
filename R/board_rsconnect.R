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
  board$account <- args$account
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

board_pin_create.rsconnect <- function(board, path, name, description, type, metadata, ...) {
  on.exit(board_connect(board$name))

  deps <- rsconnect_dependencies()

  temp_dir <- file.path(tempfile(), name)
  dir.create(temp_dir, recursive = TRUE)
  on.exit(unlink(temp_dir, recursive = TRUE))

  x <- if (length(dir(path)) == 1 && identical(tools::file_ext(dir(path)), "rds"))
    readRDS(dir(path, full.names = TRUE)) else path

  data_files <- rsconnect_bundle_create(x, temp_dir)
  pin_manifest_create(temp_dir, type, description, metadata, data_files)

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
                                    content_category = "data",
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
        content_category = "data",
        has_parameters = FALSE
      ),
      packages = NA,
      files = files,
      users = NA
    )

    bundle <- rsconnect_bundle_compress(temp_dir, manifest)

    upload <- rsconnect_api_post(board,
                                 paste0("/__api__/v1/experimental/content/", guid, "/upload"),
                                 httr::upload_file(normalizePath(bundle)))

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
  extended <- identical(list(...)$extended, TRUE)
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

  results <- rsconnect_api_get(board, paste0("/__api__/applications/?", filter))$applications

  results <- as.data.frame(do.call("rbind", results))

  if (!all_content) results <- results[results$content_category == "data",]

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

rsconnect_get_by_name <- function(board, name) {
  name_pattern <- if (grepl("/", name)) name else paste0(".*/", name, "$")
  only_name <- pin_content_name(name)

  details <- board_pin_find(board, only_name, extended = TRUE)

  details <- details[grepl(name_pattern, details$name) & details$content_category == "data",]

  if (nrow(details) > 1) {
    details <- details[details$owner_username == board$account,]
  }

  if (nrow(details) > 1) {
    stop("Multiple pins named '", name, "' in board '", board$name, "'")
  }

  details
}

board_pin_get.rsconnect <- function(board, name) {
  url <- name

  if (!grepl("^http://|^https://|^/content/", name)) {
    details <- rsconnect_get_by_name(board, name)
    url <- details$url
  }

  url <- gsub("/$", "", url)
  remote_path <- gsub("//", "/", file.path("/content", gsub("(^.*/|^)content/", "", url)))

  local_path <- tempfile()
  dir.create(local_path)

  rsconnect_api_download(board, file.path(remote_path, "pin.json"), file.path(local_path, "pin.json"))
  manifest <- jsonlite::read_json(file.path(local_path, "pin.json"))

  for (file in manifest$files) {
    rsconnect_api_download(board, file.path(remote_path, file), file.path(local_path, file))
  }

  unlink(dir(local_path, "index\\.html$|pagedtable-1\\.1$|pin\\.json$", full.names = TRUE))

  attr(local_path, "pin_type") <- manifest$type
  local_path
}

board_pin_remove.rsconnect <- function(board, name) {
  details <- rsconnect_get_by_name(board, name)

  invisible(rsconnect_api_delete(board, paste0("/__api__/v1/experimental/content/", details$guid)))
}

board_info.rsconnect <- function(board) {
}
