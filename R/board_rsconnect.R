#' Register RStudio Connect Board
#'
#' @description
#' To use a RStudio Connect board, you need to first authenticate. The easiest
#' way to do so is by launching **Tools** - **Global Options** - **Publishing**
#' - **Connect**, and follow the instructions.
#'
#' If you use multiple RStudio connect servers or multiple user accounts,
#' you'll need to specify the `server` or `account` parameters when connecting
#' to the board.
#'
#' # Sharing
#'
#' You can share pins with others in RStudio Connect by changing the viewers
#' of the document to specific users or groups. This is accomplished by opening
#' the new published pin and then changing access under the settings tab.
#' After you've shared the pin, it will be automatically available to others.
#'
#' # Public
#'
#' You can also choose to share a pin publicly and avoid having to register
#' the RStudio Connect board to retrieve this pin.
#'
#' To create a public pin, first publish a pin and navigate to RStudio Connect;
#' then set the "Access" to "Anyone - no login required" -- The pin will become
#' public and accessible to anyone using their content URL. The remote resource
#' stored in RStudio Connect can then be cached locally with `pin()` as follows:
#'
#' ```r
#' pin("https://rstudio-connect-server/content/1234", name = "my-rsc-content")
#' ```
#'
#' To avoid having to change the "Access" manually, you can also set the
#' `access_type` to `acl`, `loggend_in` or `all` when creating a pin:
#'
#' ```r
#'  pin("https://rstudio-connect-server/content/1234", name = "my-rsc-content",
#'    access_type = "all"
#'  )
#' ```
#'
#' # Automation
#'
#' One significant advantage of RStudio Connect over other boards is its
#' ability to schedule R Markdown reports to automate the creation of pins.
#'
#' To support automation you need to use an [RStudio Connect API Key](https://docs.rstudio.com/connect/user/api-keys/) as your authentication method. Once you've got an
#' API key, you can connect to the board with:
#'
#' ```r
#' board <- board_rsconnect(
#'   server = "https://rstudio-connect-server",
#'   key = Sys.getenv("CONNECT_API_KEY"),
#' )
#' ```
#'
#' Note the use of an environment variable to ensure that the API key is
#' not stored in plain text in the document.
#'
#' # Customizing
#'
#' A pin is displayed in RStudio Connect with an auto-generated page showcasing
#' instructions for getting the pin and a preview of the dataset, this page can
#' be customized as follows:
#'
#' 1. Locate the file with `system.file("views/data/index.html", package = "pins")`
#' 1. Copy the file to a new location and make any changes to it.
#' 1. Set the file path as an option using `Sys.setenv(RSCONNECT_HTML_PATH = <your index>)`.
#' 1. Pin a dataset normally.
#'
#' @param name Optional name for this board, defaults to 'rsconnect'.
#' @param server Optional address to RStudio Connect server.
#' @param account Optional account name to use with RStudio Connect.
#' @param key The RStudio Connect API key.
#' @param output_files Should the output in an automated report create output files?
#' @param cache The local folder to use as a cache, defaults to `board_cache_path()`.
#' @param ... Additional parameters required to initialize a particular board.
#' @family boards
#' @examples
#' \dontrun{
#' # the following examples require an RStudio Connect API key
#'
#' # register from rstudio
#' board <- board_rsconnect()
#'
#' # register from rstudio with multiple servers
#' board <- board_rsconnect(server = "https://rstudio-connect-server")
#'
#' # register from rstudio with multiple account
#' board <- board_rsconnect(account = "account-name")
#'
#' # register automated report for rstudio connect
#' board <- board_rsconnect(
#'   key = Sys.getenv("CONNECT_API_KEY"),
#'   server = Sys.getenv("CONNECT_SERVER")
#' )
#' }
#'
#' @export
board_rsconnect <- function(name = "rsconnect",
                            server = NULL,
                            account = NULL,
                            key = NULL,
                            output_files = FALSE,
                            cache = board_cache_path(),
                            ...) {

  # key <- key %||% Sys.getenv("CONNECT_API_KEY", Sys.getenv("RSCONNECT_API"))
  if (identical(key, "")) {
    stop("Invalid API key, the API key is empty.")
  }

  # server <- server %||% Sys.getenv("CONNECT_SERVER", Sys.getenv("RSCONNECT_SERVER"))
  if (!is.null(key) && is.null(server)) {
    stop("Please specify the 'server' parameter when using API keys.")
  }
  if (!is.null(server)) {
    server <- gsub("/$", "", server)
  }

  board <- new_board("pins_board_rsconnect",
    name = name,
    cache = cache,
    server = server,
    account = account,
    key = key,
    output_files = output_files,
    pins_supported = TRUE,
    ...
  )

  if (!rsconnect_api_auth(board) && !identical(board$output_files, TRUE)) {
    board <- rsconnect_token_initialize(board)
  }
  if (!rsconnect_pins_supported(board)) {
    stop("pins not supported by this rsconnect", call. = FALSE)
  }

  if (is.null(board$account)) {
    board$account <- rsconnect_api_get(board, "/__api__/users/current/")$username
  }

  board
}

rsconnect_pins_supported <- function(board) {
  tryCatch(
    {
      version <- rsconnect_api_get(board, "/__api__/server_settings")$version
      package_version(version) > "1.7.7"
    },
    error = function(e) FALSE
  )
}

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
board_pin_find.pins_board_rsconnect <- function(board,
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

  if (identical(board$pins_supported, TRUE) && !identical(all_content, TRUE)) content_filter <- "filter=content_type:pin&"

  entries <- rsconnect_api_get(board, paste0("/__api__/applications/?count=", getOption("pins.search.count", 10000), "&", content_filter, utils::URLencode(filter)))$applications
  if (!all_content) entries <- Filter(function(e) e$content_category == "pin", entries)

  entries <- lapply(entries, function(e) {
    e$name <- paste(e$owner_username, e$name, sep = "/")
    e
  })

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

rsconnect_get_by_name <- function(board, name, all_content = FALSE) {
  only_name <- pin_content_name(name)

  details <- board_pin_find(board, text = only_name, name = name, all_content = all_content)
  details <- pin_results_extract_column(details, "content_category")
  details <- pin_results_extract_column(details, "url")
  details <- pin_results_extract_column(details, "guid")

  if (nrow(details) > 1) {
    owner_details <- details[details$owner_username == board$account, ]
    if (nrow(owner_details) == 1) {
      details <- owner_details
    }
  }

  if (nrow(details) > 1) {
    stop(
      "Multiple pins named '", name, "' in board '", board$name,
      "', choose from: ", paste0("'", paste0(details$name, collapse = "', '"), "'.")
    )
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

#' @export
board_pin_remove.pins_board_rsconnect <- function(board, name, ...) {
  details <- rsconnect_get_by_name(board, name)
  details <- pin_results_extract_column(details, "guid")

  invisible(rsconnect_api_delete(board, paste0("/__api__/v1/experimental/content/", details$guid)))
}

#' @export
board_browse.pins_board_rsconnect <- function(board, ...) {
  utils::browseURL(board$server)
}

#' @export
board_pin_versions.pins_board_rsconnect <- function(board, name, ...) {
  details <- rsconnect_get_by_name(board, name)
  if (nrow(details) == 0) stop("The pin '", name, "' is not available in the '", board$name, "' board.")

  details$guid

  bundles <- rsconnect_api_get(board, paste0("/__api__/v1/experimental/content/", details$guid, "/bundles/"))

  data.frame(
    version = sapply(bundles$results, function(e) e$id),
    created = sapply(bundles$results, function(e) e$created_time),
    size = sapply(bundles$results, function(e) e$size),
    stringsAsFactors = FALSE
  ) %>%
    format_tibble()
}


pin_results_extract_column <- function(df, column) {
  df[[column]] <- sapply(df$metadata, function(e) jsonlite::fromJSON(e)[[column]])
  df
}
