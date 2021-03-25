#' Use an RStudio Connect board
#'
#' @description
#' To use a RStudio Connect board, you need to first authenticate. The easiest
#' way to do so is by launching **Tools** - **Global Options** - **Publishing**
#' - **Connect**, and follow the instructions.
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
#' @inheritParams new_board
#' @param auth There are two approaches to auth: you can either use `"envvars"`
#'   `CONNECT_API_KEY` and `CONNECT_SERVER` or the rsconnect package. The
#'   default is `auto`, which will use the environment variables if both are
#'   available, and rsconnect if not.
#' @param server For `auth = "envvar"` the full url to the server.
#'   For `auth = 'rsconnect'` a host name used to disambiguate RSC accounts.
#' @param account A user name used to disambiguate multiple RSC accounts
#' @param key The RStudio Connect API key.
#' @param output_files `r lifecycle::badge("deprecated") No longer supported.
#' @family boards
#' @export
#' @examples
#' \dontrun{
#' board <- board_rsconnect()
#' # Share the mtcars with your team
#' board %>% pin_write(mtcars, "mtcars")
#'
#' # Download a shared dataset
#' board %>% pin_read(mtcars)
#' }
board_rsconnect <- function(
                            auth = c("auto", "envvar", "rsconnect"),
                            server = NULL,
                            account = NULL,
                            key = NULL,
                            output_files = FALSE,
                            cache = board_cache_path(name),
                            name = "rsconnect",
                            versions = TRUE,
                            ...) {

  auth <- check_auth(auth)
  if (auth == "envvar") {
    server <- server %||% Sys.getenv("CONNECT_SERVER")
    account <- NULL # see below
    server_name <- httr::parse_url(server)$hostname
    key <- key %||% Sys.getenv("CONNECT_API_KEY")
  } else {
    info <- rsc_account_find(server, account)
    account <- info$name
    server <- info$server
    server_name <- info$server_name
  }

  board <- new_board("pins_board_rsconnect",
    name = name,
    cache = cache,
    server = server,
    account = account,
    server_name = server_name,
    url = url,
    key = key,
    versions = versions,
    ...
  )

  if (rsc_version(board) < "1.7.7") {
    abort("Pins requires RSC 1.7.7 or later")
  }

  # Fill in account name if auth == "envvar"
  board$account <- board$account %||% rsc_GET(board, "users/current/")$username

  board
}

check_auth <- function(auth = c("auto", "envvar", "rsconnect")) {
  auth <- arg_match(auth)
  if (auth == "auto") {
    if (has_envvars(c("CONNECT_API_KEY", "CONNECT_SERVER"))) {
      "envvar"
    } else {
      "rsconnect"
    }
  } else {
    auth
  }
}

rsc_account_find <- function(server = NULL, name = NULL) {
  check_installed("rsconnect")

  accounts <- rsconnect::accounts()
  if (is.null(accounts)) {
    abort("No RStudio Connect accounts has been registered")
  }

  if (!is.null(server)) {
    accounts <- accounts[accounts$server == server, , drop = FALSE]
  } else {
    accounts <- accounts[accounts$server != "shinyapps.io", , drop = FALSE]
  }

  if (!is.null(name)) {
    accounts <- accounts[accounts$name == name, , drop = FALSE]
  }

  if (nrow(accounts) == 0) {
    abort("No matching RStudio Connect accounts found")
  } else if (nrow(accounts) > 1) (
    abort(c(
      "Multiple matching RStudio Connect found",
      i = "Please disambiguate with `server` and `account`"
    ))
  )

  url <- rsconnect::serverInfo(accounts$server)$url

  list(
    name = accounts$name,
    server_name = accounts$server,
    server = sub("/__api__$", "", url)
  )
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

  if (!identical(all_content, TRUE)) content_filter <- "filter=content_type:pin&"

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
    return(board_empty_results())
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

pin_split_owner <- function(name) {
  parts <- strsplit(name, "/")[[1]]
  list(
    owner = if (length(parts) > 1) paste(parts[1:length(parts) - 1], collapse = "/") else NULL,
    name = if (length(parts) > 0) parts[length(parts)] else NULL
  )
}

pin_content_name <- function(name) {
  if (is.character(name)) pin_split_owner(name)$name else name
}
