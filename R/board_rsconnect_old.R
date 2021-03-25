
#' @export
board_pin_create.pins_board_rsconnect <- function(board, path, name, metadata, code = NULL,
                                       search_all = FALSE,
                                       ...) {
  dots <- list(...)
  access_type <- if (!is.null(access_type <- dots[["access_type"]])) {
    match.arg(access_type, c("acl", "logged_in", "all"))
  } else {
    NULL
  }

  temp_dir <- file.path(tempfile(), name)
  dir.create(temp_dir, recursive = TRUE)
  on.exit(unlink(temp_dir, recursive = TRUE))

  x <- if (identical(dir(path, "data\\.rds"), "data.rds")) {
    readRDS(dir(path, "data\\.rds", full.names = TRUE))
  } else {
    path
  }

  if (grepl("/", name)) {
    name_qualified <- name
    name <- gsub(".*/", "", name_qualified)
    account_name <- gsub("/.*", "", name_qualified)

    if (grepl("/", name) || grepl("/", account_name)) {
      stop("Pin names must follow the user/name convention.")
    }
  }
  else {
    name_qualified <- paste0(board$account, "/", name)
    account_name <- board$account
  }

  if (identical(board$output_files, TRUE)) {
    account_name <- "https://rstudio-connect-server/content/app-id"
  }

  file.copy(dir(path, full.names = TRUE), temp_dir)
  data_files <- tryCatch(
    {
      rsconnect_bundle_create(x, temp_dir, name, board, account_name, retrieve_command = code)
    },
    error = function(e) {
      NULL
    }
  )

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
    rmarkdown::output_metadata$set(rsc_output_files = file.path(knit_pin_dir, dir(knit_pin_dir, recursive = TRUE)))
  }
  else {
    previous_versions <- NULL

    # use all_content to ensure broken pins can be overwritten
    existing <- rsconnect_get_by_name(board, name_qualified, all_content = search_all)
    if (nrow(existing) == 0) {
      content <- rsconnect_api_post(
        board,
        paste0("/__api__/v1/experimental/content"),
        list(
          app_mode = "static",
          content_category = "pin",
          name = name,
          description = board_metadata_to_text(metadata, metadata$description)
        )
      )
      if (!is.null(content$error)) {
        # we might fail to create pins that exists (code 26) but are not shown as pins since they fail while being created
        if (identical(content$code, 26L)) {
          return(board_pin_create(board = board, path = path, name = name, metadata = metadata, code = code, search_all = TRUE, ...))
        }

        pin_log("Failed to create pin with name '", name, "'.")
        stop("Failed to create pin: ", content$error)
      }

      guid <- content$guid
    }
    else {
      guid <- existing$guid

      if (!existing$content_category %in% c("pin", "")) {
        stop("Failed to create pin: Content named '", name, "' of type '", existing$content_category, "' already exists.")
      }

      # when versioning is turned off we also need to clean up previous bundles so we store the current versions
      if (!board_versions_enabled(board, TRUE)) {
        previous_versions <- board_pin_versions(board, name_qualified)
      }

      content <- rsconnect_api_post(
        board,
        paste0("/__api__/v1/experimental/content/", guid),
        list(
          app_mode = "static",
          content_category = "pin",
          name = name,
          description = board_metadata_to_text(metadata, metadata$description),
          access_type = access_type
        )
      )

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
      progress = http_utils_progress("up", size = file.info(normalizePath(bundle))$size)
    )

    if (!is.null(upload$error)) {
      stop("Failed to upload pin: ", upload$error)
    }

    result <- rsconnect_api_post(
      board,
      paste0("/__api__/v1/experimental/content/", guid, "/deploy"),
      list(
        bundle_id = upload$bundle_id
      )
    )

    if (!is.null(result$error)) {
      stop("Failed to activate pin: ", result$error)
    }

    # it might take a few seconds for the pin to register in rsc, see travis failures, wait 5s
    result <- rsconnect_wait_by_name(board, name_qualified)

    # when versioning is turned off we also need to clean up previous bundles
    if (!board_versions_enabled(board, TRUE) && !is.null(previous_versions)) {
      for (idx in 1:nrow(previous_versions)) {
        delete_version <- previous_versions[idx, ]

        delete_path <- paste0("/__api__/v1/experimental/bundles/", delete_version$version)
        pin_log("Deleting previous version ", delete_path)
        rsconnect_api_delete(board, delete_path)
      }
    }

    result
  }
}


#' @export
board_pin_get.pins_board_rsconnect <- function(board, name, version = NULL, ...) {
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
