rsconnect_dependencies <- function() {
  list(
    output_metadata = get_function("output_metadata", "rmarkdown")
  )
}

rsconnect_pins_supported <- function(board) {
  package_version(rsconnect_api_version(board)) > package_version("1.7.7")
}

board_initialize.rsconnect <- function(board, ...) {
  args <- list(...)

  envvar_key <- Sys.getenv("CONNECT_API_KEY", Sys.getenv("RSCONNECT_API"))
  if (is.null(args$key) && nchar(envvar_key) > 0) {
    args$key <- envvar_key
  }

  envvar_server <- Sys.getenv("CONNECT_SERVER", Sys.getenv("RSCONNECT_SERVER"))
  if (is.null(args$server) && nchar(envvar_server) > 0) {
    args$server <- envvar_server
  }

  if (!is.null(args$server)) {
    board$server <-  gsub("/$", "", args$server)
    board$server_name <- gsub("https?://|(:[0-9]+)?/.*", "", args$server)
  }

  board$account <- args$account
  board$output_files <- args$output_files

  if (identical(args$key, "")) stop("Invalid API key, the API key is empty.")

  board$key <- args$key

  if (!is.null(board$key) && is.null(board$server)) {
    stop("Please specify the 'server' parameter when using API keys.")
  }

  if (!rsconnect_api_auth(board) && !identical(board$output_files, TRUE)) {
    board <- rsconnect_token_initialize(board)
  }

  board$pins_supported <- tryCatch(rsconnect_pins_supported(board), error = function(e) FALSE)

  if (is.null(board$account )) {
    board$account  <- rsconnect_api_get(board, "/__api__/users/current/")$username
  }

  board
}

board_pin_create.rsconnect <- function(board, path, name, metadata, code = NULL,
                                      ...) {
  dots <- list(...)
  access_type <- if (!is.null(access_type <- dots[["access_type"]])) {
    match.arg(access_type, c("acl", "logged_in", "all"))
  } else {
    NULL
  }

  deps <- rsconnect_dependencies()

  temp_dir <- file.path(tempfile(), name)
  dir.create(temp_dir, recursive = TRUE)
  on.exit(unlink(temp_dir, recursive = TRUE))

  x <- if (identical(dir(path, "data\\.rds"), "data.rds"))
    readRDS(dir(path, "data\\.rds", full.names = TRUE)) else path

  if (grepl("/", name)) {
    name_qualified <- name
    name <- gsub(".*/", "", name_qualified)
  }
  else {
    name_qualified <- paste0(board$account, "/", name)
  }

  account_name <- board$account
  if (identical(board$output_files, TRUE)) {
    account_name <- "https://rstudio-connect-server/content/app-id"
  }

  file.copy(dir(path, full.names = TRUE), temp_dir)
  data_files <- tryCatch({
    rsconnect_bundle_create(x, temp_dir, name, board, account_name, retrieve_command = code)
  }, error = function(e) {
    NULL
  })

  # handle unexepcted failures gracefully
  if (is.null(data_files)) {
    warning("Falied to create preview files for pin.")
    unlink(temp_dir, recursive = TRUE)
    dir.create(temp_dir, recursive = TRUE)
    file.copy(dir(path, full.names = TRUE), temp_dir)
    data_files <- rsconnect_bundle_create.default(x, temp_dir, name, board, account_name)
  }

  rsconnect_is_authenticated <- function(board) {
    !is.null(board$key) || !is.null(board$account)
  }

  if (identical(board$output_files, TRUE)) {
    knit_pin_dir <- file.path(name)
    file.copy(temp_dir, getwd(), recursive = TRUE)
    deps$output_metadata$set(rsc_output_files = file.path(knit_pin_dir, dir(knit_pin_dir, recursive = TRUE)))
  }
  else {
    previous_versions <- NULL

    existing <- rsconnect_get_by_name(board, name_qualified)
    if (nrow(existing) == 0) {
      content <- rsconnect_api_post(board,
                                  paste0("/__api__/v1/experimental/content"),
                                  list(
                                    app_mode = "static",
                                    content_category = "pin",
                                    name = name,
                                    description = board_metadata_to_text(metadata, metadata$description)
                                  ))
      if (!is.null(content$error)) {
        pin_log("Failed to create pin with name '", name, "'.")
        stop("Failed to create pin: ", content$error)
      }

      guid <- content$guid
    }
    else {
      guid <- existing$guid

      # when versioning is turned off we also need to clean up previous bundles so we store the current versions
      if (!board_versions_enabled(board, TRUE)) {
        previous_versions <- board_pin_versions(board, name_qualified)
      }

      content <- rsconnect_api_post(board,
                                    paste0("/__api__/v1/experimental/content/", guid),
                                    list(
                                      app_mode = "static",
                                      content_category = "pin",
                                      name = name,
                                      description = board_metadata_to_text(metadata, metadata$description),
                                      access_type = access_type
                                    ))

      if (!is.null(content$error)) {
        pin_log("Failed to update pin with GUID '", guid, "' and name '", name, "'.")
        stop("Failed to create pin: ", content$error)
      }
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
                                 progress = http_utils_progress("up", size = file.info(normalizePath(bundle))$size))

    if (!is.null(upload$error)) {
      # before we fail, clean up rsconnect content
      rsconnect_api_delete(board, paste0("/__api__/v1/experimental/content/", guid))

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

    # it might take a few seconds for the pin to register in rsc, see travis failures, wait 5s
    result <- rsconnect_wait_by_name(board, name_qualified)

    # when versioning is turned off we also need to clean up previous bundles
    if (!board_versions_enabled(board, TRUE) && !is.null(previous_versions)) {
      for (idx in 1:nrow(previous_versions)) {
        delete_version <- previous_versions[idx,]

        delete_path <- paste0("/__api__/v1/experimental/bundles/", delete_version$version)
        pin_log("Deleting previous version ", delete_path)
        rsconnect_api_delete(board, delete_path)
      }
    }

    result
  }
}

board_pin_find.rsconnect <- function(board,
                                     text = NULL,
                                     name = NULL,
                                     all_content = FALSE,
                                     extended = FALSE,
                                     metadata = FALSE,
                                     ...) {
  if (is.null(text)) text <- ""
  if (!is.null(name)) text <- pin_content_name(name)

  filter <- paste0("search=", text)
  content_filter <- ""

  if (identical(board$pins_supported, TRUE)) content_filter <- "filter=content_type:pin&"

  entries <- rsconnect_api_get(board, paste0("/__api__/applications/?", content_filter, utils::URLencode(filter)))$applications
  if (!all_content) entries <- Filter(function(e) e$content_category == "pin", entries)

  entries <- lapply(entries, function(e) { e$name <- paste(e$owner_username, e$name, sep = "/") ; e })

  if (!is.null(name)) {
    name_pattern <- if (grepl("/", name)) paste0("^", name, "$") else paste0(".*/", name, "$")
    entries <- Filter(function(e) grepl(name_pattern, e$name), entries)
  }

  results <- pin_results_from_rows(entries)

  if (nrow(results) == 0) {
    return(
      board_empty_results()
    )
  }

  null_or_value <- function(e, value) if (is.null(e)) value else e
  results$name <- as.character(results$name)
  results$type <- unname(sapply(results$description, function(e) null_or_value(board_metadata_from_text(e)$type, "files")))

  if (identical(metadata, TRUE)) {
    results$metadata <- sapply(results$description, function(e) as.character(jsonlite::toJSON(board_metadata_from_text(e), auto_unbox = TRUE)))
  }

  results$description <- board_metadata_remove(results$description)

  if (length(entries) == 1) {
    # enhance with pin information
    remote_path <- rsconnect_remote_path_from_url(board, entries[[1]]$url)
    etag <- as.character(entries[[1]]$last_deployed_time)

    manifest <- list()
    if (identical(metadata, TRUE)) {
      local_path <- rsconnect_api_download(board, entries[[1]]$name, file.path(remote_path, "data.txt"), etag = etag)
      manifest <- pin_manifest_get(local_path)
    }

    if (identical(extended, TRUE)) {
      manifest <- c(entries[[1]], manifest)
    }

    results$type <- manifest$type

    if (identical(metadata, TRUE)) {
      results$metadata <- as.character(jsonlite::toJSON(manifest, auto_unbox = TRUE))
    }
  }

  results
}

rsconnect_get_by_name <- function(board, name) {
  only_name <- pin_content_name(name)

  details <- board_pin_find(board, text = only_name, name = name)
  details <- pin_results_extract_column(details, "content_category")
  details <- pin_results_extract_column(details, "url")
  details <- pin_results_extract_column(details, "guid")

  if (nrow(details) > 1) {
    owner_details <- details[details$owner_username == board$account,]
    if (nrow(owner_details) == 1) {
      details <- owner_details
    }
  }

  if (nrow(details) > 1) {
    stop("Multiple pins named '", name, "' in board '", board$name,
         "', choose from: ", paste0("'", paste0(details$name, collapse = "', '"), "'."))
  }

  details
}

rsconnect_wait_by_name <- function(board, name) {
  wait_time <- 0
  while (nrow(rsconnect_get_by_name(board, name)) == 0 && wait_time < getOption("pins.rsconnect.wait", 5)) {
    Sys.sleep(1)
    wait_time <- wait_time + 1
  }
}

rsconnect_remote_path_from_url <- function(board, url) {
  url <- gsub("^https?://", "", url)
  server <- gsub("^https?://", "", board$server)

  url <- gsub(paste0("^.*", server), "", url)
  gsub("/$", "", url)
}

board_pin_get.rsconnect <- function(board, name, version = NULL, ...) {
  url <- name

  if (identical(board$output_files, TRUE)) {
    return(name)
  }

  etag <- ""
  if (!grepl("^http://|^https://|^/content/", name)) {
    details <- rsconnect_get_by_name(board, name)
    if (nrow(details) == 0) stop("The pin '", name, "' is not available in the '", board$name, "' board.")
    url <- details$url
    name <- details$name
    etag <- jsonlite::fromJSON(details$metadata)$last_deployed_time
  }

  remote_path <- rsconnect_remote_path_from_url(board, url)
  download_name <- name

  if (!is.null(version)) {
    remote_path <- paste0(remote_path, "/_rev", version)
    download_name <- file.path(name, pin_versions_path_name(), version)
  }

  local_path <- rsconnect_api_download(board, download_name, file.path(remote_path, "data.txt"), etag = etag)
  manifest_paths <- pin_manifest_download(local_path)

  for (file in manifest_paths) {
    rsconnect_api_download(board, download_name, file.path(remote_path, file), etag = etag)
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
  utils::browseURL(board$server)
}

board_pin_versions.rsconnect <- function(board, name) {
  details <- rsconnect_get_by_name(board, name)
  if (nrow(details) == 0) stop("The pin '", name, "' is not available in the '", board$name, "' board.")

  details$guid

  bundles <- rsconnect_api_get(board, paste0("/__api__/v1/experimental/content/", details$guid, "/bundles/"))

  data.frame(
    version = sapply(bundles$results, function(e) e$id),
    created = sapply(bundles$results, function(e) e$created_time),
    size = sapply(bundles$results, function(e) e$size),
    stringsAsFactors = FALSE) %>%
    format_tibble()
}

