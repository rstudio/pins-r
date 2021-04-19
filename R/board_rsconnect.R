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


board_rsconnect_test <- function(...) {
  if (nrow(rsconnect::accounts()) > 1) {
    board_rsconnect(..., auth = "rsconnect")
  } else if (!has_envvars(c("CONNECT_API_KEY", "CONNECT_SERVER"))) {
    testthat::skip("No RSC env vars set up")
  } else {
    board_rsconnect(..., auth = "envvar")
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
                                               extended = FALSE,
                                               metadata = FALSE,
                                     ...) {

  params <- list(
    search = text,
    filter = "content_type:pin",
    count = 1000
  )
  json <- rsc_GET(board, "applications/", params)

  pins <- json$applications
  name <- map_chr(pins, ~ .x$name)
  user <- map_chr(pins, ~ .x$owner_username)

  wibble(
    name = paste0(user, "/", name),
    title = map_chr(pins, ~ .x$title %||% ""),
    description = map_chr(pins, ~ .x$description)
  )
}

#' @export
board_pin_remove.pins_board_rsconnect <- function(board, name, ...) {
  rsc_content_delete(board, name)
}

#' @export
board_browse.pins_board_rsconnect <- function(board, ...) {
  utils::browseURL(board$server)
}

#' @export
board_pin_versions.pins_board_rsconnect <- function(board, name, ...) {
  guid <- rsc_content_find(board, name)$guid
  rsc_content_versions(board, guid)
}

#' @export
board_pin_download.pins_board_rsconnect <- function(board, name, version = NULL, ...) {

  content <- rsc_content_find(board, name, quiet = FALSE)

  url <- content$content_url
  if (!is.null(version)) {
    stopifnot(is_string(version))
    url <- paste0(url, "_rev", version, "/")
  } else {
    version <- content$bundle_id
  }

  # Bundles (guid + bundle id) are immutable so only need to download once
  # Can't use bundle download endpoint because that requires collaborator
  # access. So download data.txt, then download each file that it lists.
  pin_path <- fs::path(board$cache, paste0(content$guid, "_", version))
  fs::dir_create(pin_path)
  rsc_download(board, url, pin_path, "data.txt")

  meta <- read_meta(pin_path)
  for (path in meta$path) {
    rsc_download(board, url, pin_path, path)
  }

  list(
    dir = pin_path,
    path = fs::path(pin_path, meta$path),
    meta = meta
  )
}

#' @export
board_pin_upload.pins_board_rsconnect <- function(
    board,
    name,
    path,
    metadata,
    versioned = NULL,
    x = NULL,
    ...,
    access_type = c("acl", "logged_in", "all"))
{
  # https://docs.rstudio.com/connect/1.8.0.4/cookbook/deploying/

  versioned <- versioned %||% board$versions

  # Make .tar.gz bundle containing data.txt + index.html + pin data
  bundle_dir <- rsc_bundle(board, name, path, metadata, x = x)
  bundle_file <- fs::file_temp(ext = "tar.gz")
  utils::tar(
    bundle_file, fs::dir_ls(bundle_dir),
    compression = "gzip",
    tar = "internal"
  )

  # Find/create content item
  content <- tryCatch(
    rsc_content_find(board, name),
    pins_pin_absent = function(e) {
      rsc_content_create(board, name, metadata, access_type = access_type)
    }
  )
  content_guid <- content$guid

  # Upload bundle
  # https://docs.rstudio.com/connect/api/#post-/v1/content/{guid}/bundles
  json <- rsc_POST(board, rsc_v1("content", content_guid, "bundles"),
    body = httr::upload_file(bundle_file)
  )
  bundle_id <- json$id

  # Deploy bundle
  # https://docs.rstudio.com/connect/api/#post-/v1/experimental/content/{guid}/deploy
  json <- rsc_POST(board, rsc_v1("content", content_guid, "deploy"),
    body = list(bundle_id = bundle_id),
  )
  task_id <- json$task_id

  # Poll until deployment complete
  # https://docs.rstudio.com/connect/api/#get-/v1/experimental/tasks/{id}
  json <- rsc_GET(board, rsc_v1("tasks", task_id), list(wait = 1))
  while (!json$finished) {
    json <- rsc_GET(
      board, rsc_v1("tasks", task_id),
      list(wait = 1, first = json$last)
    )
  }

  # Clean up old bundles if not versioned
  if (!versioned) {
    versions <- rsc_content_versions(board, content_guid)
    ids <- setdiff(versions$version, bundle_id)

    if (length(ids) == 0) {
      # First version
    } else if (length(ids) == 1) {
      rsc_DELETE(board, rsc_v1("content", content_guid, "bundles", ids))
    } else {
      abort(c(
        "Pin is versioned, but you have requested not to use versions",
        "To un-version this pin you will need to delete it"
      ))
    }
  }

  invisible()
}

# v0 ----------------------------------------------------------------------

#' @export
board_pin_get.pins_board_rsconnect <- function(board, name, version = NULL, ...,
                                               extract = NULL) {

  pin <- board_pin_download.pins_board_rsconnect(board, name, version = version, ...)
  pin$dir
}

#' @export
board_pin_create.pins_board_rsconnect <- function(board, path, name, metadata, code = NULL,
                                       search_all = FALSE,
                                       ...) {

  path <- fs::dir_ls(path)

  board_pin_upload.pins_board_rsconnect(
    board = board,
    name = name,
    path = path,
    metadata = metadata,
    ...
  )
}

# Bundle ------------------------------------------------------------------

# Content -----------------------------------------------------------------

rsc_content_find <- function(board, name, quiet = TRUE) {
  name <- rsc_parse_name(name)

  # TODO: look in cache if !is.null(owner$name)

  # https://docs.rstudio.com/connect/api/#get-/v1/content/{guid}/bundles/{id}/download
  json <- rsc_GET(board, "v1/content", list(name = name$name))
  if (length(json) == 0) {
    abort(
      paste0("Can't find pin with name '",  name$name, "'"),
      class = "pins_pin_absent"
    )
  }

  if (is.null(name$owner)) {
    if (length(json) > 1) {
      # TODO: Find user names and offer
      abort(paste0("Multiple pins with name '",  name$name, "'"))
    }
    owner <- rsc_user_name(board, json[[1]]$owner_guid)
    if (!quiet) {
      pins_inform(paste0("Downloading pin '", owner, "/", name$name, "'"))
    }
    json[[1]]
  } else {
    owner_guids <- map_chr(json, ~ .x$owner_guid)
    owner_names <- map_chr(owner_guids, rsc_user_name, board = board)
    if (!name$owner %in% owner_names) {
      abort(paste0("Can't find pin named '", name$name, "' with owner '", name$owner, "'"))
    }
    json[[name$owner %in% owner_names]]
  }
}

rsc_content_create <- function(board, name, metadata, access_type = "acl") {
  name <- rsc_parse_name(name)
  if (!grepl("^[-_A-Za-z0-9]+$", name$name)) {
    abort("RStudio connect requires alpanumeric names")
  }

  # TODO: What should happen if owner isn't NULL
  access_type <- arg_match0(access_type, c("acl", "logged_in", "all"))

  body <- list(
    name = name$name,
    title = name$name,
    access_type = access_type,
    description = metadata$description %||% ""
  )

  # https://docs.rstudio.com/connect/api/#post-/v1/content
  rsc_POST(board, rsc_v1("content"), body = body)
}

rsc_content_versions <- function(board, guid) {
  # https://docs.rstudio.com/connect/api/#get-/v1/content/{guid}/bundles
  json <- rsc_GET(board, rsc_v1("content", guid, "bundles"))

  wibble(
    version = map_chr(json, ~ .x$id),
    created = rsc_parse_time(map_chr(json, ~ .x$created_time)),
    active = map_lgl(json, ~ .x$active),
    size = map_num(json, ~ .x$size),
  )
}

rsc_content_delete <- function(board, name) {
  json <- rsc_content_find(board, name)
  rsc_DELETE(board, rsc_v1("content", json$guid))
}

rsc_parse_name <- function(x) {
  parts <- strsplit(x, "/", fixed = TRUE)[[1]]

  if (length(parts) == 1) {
    list(owner = NULL, name = parts[[1]])
  } else if (length(parts)) {
    list(owner = parts[[1]], name = paste0(parts[-1], collapse = "/"))
  }
}


rsc_user_name <- function(board, guid) {
  # https://docs.rstudio.com/connect/api/#get-/v1/users/{guid}
  rsc_GET(board, rsc_v1("users", guid))$username
}

# helpers -----------------------------------------------------------------

rsc_GET <- function(board, path, query = NULL, ...) {
  path <- paste0("/__api__/", path)
  auth <- rsc_auth(board, path, "GET", NULL)

  req <- httr::GET(board$server,
    path = path,
    query = query,
    auth,
    ...
  )
  rsc_check_status(req)
  httr::content(req)
}

rsc_download <- function(board, content_url, dest_path, name) {
  # RSC pin bundle are immutable so never need to re-download
  dest <- fs::path(dest_path, name)
  if (fs::file_exists(dest)) {
    return()
  }

  path <- paste0("/", httr::parse_url(content_url)$path, name)
  auth <- rsc_auth(board, path, "GET", NULL)

  req <- httr::GET(content_url,
    path = path,
    auth,
    httr::write_disk(dest)
  )
  rsc_check_status(req)
  invisible()
}

rsc_DELETE <- function(board, path, query = NULL, ...) {
  path <- paste0("/__api__/", path)
  auth <- rsc_auth(board, path, "DELETE", NULL)

  req <- httr::DELETE(board$server,
    path = path,
    query = query,
    auth,
    ...
  )
  rsc_check_status(req)
  invisible()
}

rsc_POST <- function(board, path, query = NULL, body, ...) {
  path <- paste0("/__api__/", path)

  # Turn body into a path so it can be passed to
  # rsconnect:::signatureHeaders() if needed
  if (is.null(body)) {
    body_path <- NULL
  } else if (inherits(body, "form_file")) {
    body_path <- body$path
  } else if (is_bare_list(body)) {
    body_path <- withr::local_tempfile()
    jsonlite::write_json(body, body_path, auto_unbox = TRUE)
    body <- httr::upload_file(body_path, "application/json")
  } else {
    abort("Unknown `body` type")
  }
  auth <- rsc_auth(board, path, "POST", body_path)

  req <- httr::POST(board$server,
    path = path,
    query = query,
    body = body,
    auth,
    ...
  )
  rsc_check_status(req)
  httr::content(req)
}

rsc_auth <- function(board, path, verb, body_path) {
  if (!is.null(board$key)) {
    httr::add_headers("Authorization" = paste("Key", board$key))
  } else {
    # https://github.com/rstudio/connect/wiki/token-authentication#request-signing-rsconnect
    info <- rsconnect::accountInfo(board$account, board$server_name)

    signatureHeaders <- utils::getFromNamespace("signatureHeaders", "rsconnect")
    headers <- signatureHeaders(info, verb, path, body_path)
    httr::add_headers(.headers = unlist(headers))
  }
}

rsc_check_status <- function(req) {
  if (httr::status_code(req) < 400) {

  } else {
    type <- httr::parse_media(httr::headers(req)$`content-type`)
    if (type$complete == "application/json") {
      json <- httr::content(req)
      abort(c(
        paste0("RStudio Connect API failed [", req$status_code, "]"),
        json$error
      ))
    } else {
      httr::stop_for_status(req)
    }
  }
}

rsc_version <- function(board) {
  package_version(rsc_GET(board, "server_settings")$version)
}

rsc_v1 <- function(...) {
  paste0(c("v1", ...), collapse = "/")
}

rsc_parse_time <- function(x) {
  y <- as.POSIXct(strptime(x, "%Y-%m-%dT%H:%M:%S", tz = "UTC"))
  attr(y, "tzone") <- ""
  y
}
