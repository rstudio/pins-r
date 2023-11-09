#' Use Posit Connect as board
#'
#' @description
#' To use a Posit Connect board, you need to first authenticate. The easiest
#' way to do so is by launching **Tools** - **Global Options** -
#' **Publishing** - **Connect**, and follow the instructions.
#'
#' You can share pins with others in Posit Connect by changing the viewers
#' of the document to specific users or groups. This is accomplished by opening
#' the new published pin and then changing access under the settings tab.
#' After you've shared the pin, it will be automatically available to others.
#'
#' # Public pins
#'
#' If your Posit Connect instance allows it, you can share a pin publicly by
#' setting the access type to `all`:
#'
#' ```r
#' board %>% pin_write(my_df, access_type = "all")
#' ```
#'
#' (You can also do this in Posit Connect by setting "Access" to
#' "Anyone - no login required")
#'
#' Now anyone can read your pin through [board_url()]:
#'
#' ```r
#' board <- board_url(c(
#'   numbers = "https://colorado.posit.co/rsc/great-numbers/"
#' ))
#' board %>% pin_read("numbers")
#' ```
#'
#' You can find the URL of a pin with [pin_browse()].
#'
#' @inheritParams new_board
#' @inheritParams board_url
#' @param auth There are three ways to authenticate:
#'  * `auth = "manual"` uses arguments `server` and `key`.
#'  * `auth = "envvar"` uses environment variables `CONNECT_API_KEY`
#'     and `CONNECT_SERVER`.
#'  * `auth = "rsconnect"` uses servers registered with the rsconnect
#'    package (filtered by `server` and `account`, if provided)
#'
#'  The default, `auth = "auto"`, automatically picks between the three options,
#'  using `"manual"` if `server` and `key` are provided, `"envvar"` if both
#'  environment variables are set, and `"rsconnect"` otherwise.
#' @param server For `auth = "manual"` or `auth = 'envvar'`, the full url to the server,
#'   like `http://server.posit.co/rsc` or `https://connect.posit.co/`.
#'   For `auth = 'rsconnect'` a host name used to disambiguate Connect accounts,
#'   like `server.posit.co` or `connect.posit.co`.
#' @param account A user name used to disambiguate multiple Connect accounts.
#' @param key The Posit Connect API key.
#' @param output_files `r lifecycle::badge("deprecated")` No longer supported.
#' @param use_cache_on_failure If the pin fails to download, is it OK to
#'   use the last cached version? Defaults to `is_interactive()` so you'll
#'   be robust to poor internet connectivity when exploring interactively,
#'   but you'll get clear errors when the code is deployed. Note that this
#'   argument controls whether you use the cache for reading pins, but you can't
#'   create a board object unless you can connect to your Connect server.
#' @family boards
#' @export
#' @examples
#' \dontrun{
#' board <- board_connect()
#' # Share the mtcars with your team
#' board %>% pin_write(mtcars, "mtcars")
#'
#' # Download a shared dataset
#' board %>% pin_read("timothy/mtcars")
#' }
board_connect <- function(auth = c("auto", "manual", "envvar", "rsconnect"),
                          server = NULL,
                          account = NULL,
                          key = NULL,
                          cache = NULL,
                          name = "posit-connect",
                          versioned = TRUE,
                          use_cache_on_failure = is_interactive()) {

  server <- rsc_server(auth, server, account, key)
  cache <- cache %||% board_cache_path(paste0("connect-", hash(server$url)))

  board <- new_board(
    "pins_board_connect",
    api = c(0, 1),
    name = name,
    cache = cache,
    url = server$url,
    account = server$account,         # for full name of pin
    server_name = server$server_name, # for board_connect(server = "...") in template
    auth = server$auth,
    versioned = versioned,
    use_cache_on_failure = use_cache_on_failure
  )

  version <- tryCatch(
    rsc_version(board),
    error = function(e) {
      if (length(fs::dir_ls(cache)) == 0) {
        # We've never successfully connected
        abort(c(
          glue("Failed to connect to Posit Connect instance at <{server$url}>"),
          conditionMessage(e)
        ))
      } else {
        "???"
      }
    }
  )
  pins_inform("Connecting to Posit Connect {version} at <{server$url}>")

  # Fill in account name if auth == "envvar"
  board$account <- board$account %||% rsc_GET(board, "users/current/")$username
  board
}

#' @rdname board_connect
#' @export
board_rsconnect <- function(auth = c("auto", "manual", "envvar", "rsconnect"),
                            server = NULL,
                            account = NULL,
                            key = NULL,
                            output_files = FALSE,
                            cache = NULL,
                            name = "posit-connect",
                            versioned = TRUE,
                            use_cache_on_failure = is_interactive()) {

  lifecycle::deprecate_stop("1.1.0", "board_rsconnect()", "board_connect()")

  board_connect(
    auth = auth,
    server = server,
    account = account,
    key = key,
    cache = cache,
    name = name,
    versioned = versioned,
    use_cache_on_failure = use_cache_on_failure
  )
}

#' @export
board_pin_remove.pins_board_connect <- function(board, name, ...) {
  rsc_content_delete(board, name)
}

#' @export
pin_delete.pins_board_connect <- function(board, names, ...) {
  for (name in names) {
    check_pin_exists(board, name)
    rsc_content_delete(board, name)
  }
  invisible(board)
}

#' @export
board_browse.pins_board_connect <- function(board, ...) {
  browse_url(board$url)
}

#' @export
pin_list.pins_board_connect <- function(board, ...) {
  params <- list(
    filter = "content_type:pin",
    count = 1000
  )
  json <- rsc_GET(board, "applications/", params)
  pins <- json$applications

  name <- map_chr(pins, ~ .x$name)
  user <- map_chr(pins, ~ .x$owner_username)
  paste0(user, "/", name)
}

#' @export
pin_exists.pins_board_connect <- function(board, name, ...) {
  tryCatch(
    {
      rsc_content_find(board, name)
      TRUE
    },
    pins_pin_missing = function(cnd) FALSE
  )
}

#' @export
board_pin_versions.pins_board_connect <- function(board, name, ...) {
  guid <- rsc_content_find(board, name)$guid
  rsc_content_versions(board, guid)
}
#' @export
pin_versions.pins_board_connect <- board_pin_versions.pins_board_connect

#' @export
pin_version_delete.pins_board_connect <- function(board, name, version, ...) {
  guid <- rsc_content_find(board, name, warn = FALSE)$guid
  rsc_DELETE(board, rsc_v1("content", guid, "bundles", version))
}

#' @export
pin_meta.pins_board_connect <- function(board, name, version = NULL, ...) {
  content <- rsc_content_find(board, name)

  if (is.null(version)) {
    bundle_id <- rsc_content_version(board, content$guid)
  } else {
    bundle_id <- version
  }
  url <- paste0(content$url, "_rev", bundle_id, "/")

  # Cache data.txt locally
  cache_path <- fs::path(board$cache, content$guid, bundle_id)
  fs::dir_create(cache_path)

  tryCatch(
    rsc_download(board, url, cache_path, "data.txt"),
    http_404 = function(e) {
      abort_pin_version_missing(bundle_id)
    }
  )

  meta <- read_meta(cache_path)
  local_meta(
    meta,
    name = name,
    dir = cache_path,
    url = url,
    version = bundle_id,
    content_id = content$guid
  )
}

#' @export
pin_fetch.pins_board_connect <- function(board, name, version = NULL, ...) {
  # Can't use bundle download endpoint because that requires collaborator
  # access. So download data.txt, then download each file that it lists.
  meta <- pin_meta(board, name, version = version)
  cache_touch(board, meta)

  for (file in meta$file) {
    rsc_download(board, meta$local$url, meta$local$dir, file)
  }

  meta
}

#' @export
pin_store.pins_board_connect <- function(
    board,
    name,
    paths,
    metadata,
    versioned = NULL,
    x = NULL,
    ...,
    access_type = NULL)
{
  # https://docs.posit.co/connect/1.8.0.4/cookbook/deploying/

  check_pin_name(rsc_parse_name(name)$name)

  versioned <- versioned %||% board$versioned
  if (!is.null(access_type)) {
    access_type <- arg_match0(access_type, c("acl", "logged_in", "all"))
  }

  # Find/create content item
  content_guid <- tryCatch(
    {
      guid <- rsc_content_find(board, name, warn = FALSE)$guid
      rsc_content_update(board, guid, metadata, access_type = access_type)
      guid
    },
    pins_pin_missing = function(e) {
      rsc_content_create(board, name, metadata, access_type = access_type)$guid
    }
  )

  # Make .tar.gz bundle containing data.txt + index.html + pin data
  bundle_dir <- rsc_bundle(board, name, paths, metadata, x = x)
  bundle_file <- fs::file_temp(ext = "tar.gz")

  # suppress warnings about "invalid uid value" / "invalid gid value"
  withr::with_dir(
    bundle_dir,
    suppressWarnings(utils::tar(
      bundle_file,
      compression = "gzip",
      tar = Sys.getenv("RSCONNECT_TAR", "internal")
    ))
  )

  # Upload bundle
  # https://docs.rstudio.com/connect/api/#post-/v1/content/{guid}/bundles
  json <- rsc_POST(
    board, rsc_v1("content", content_guid, "bundles"),
    body = httr::upload_file(bundle_file)
  )
  bundle_id <- json$id

  # Deploy bundle
  # https://docs.rstudio.com/connect/api/#post-/v1/experimental/content/{guid}/deploy
  json <- rsc_POST(
    board, rsc_v1("content", content_guid, "deploy"),
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
      pin_version_delete(board, name, ids)
    } else {
      abort_pin_versioned()
    }
  }

  name <- rsc_parse_name(name)
  paste0(name$owner %||% board$account, "/", name$name)
}

#' @export
pin_search.pins_board_connect <- function(board, search = NULL, ...) {
  params <- list(
    search = search,
    filter = "content_type:pin",
    count = 1000
  )
  json <- rsc_GET(board, "applications/", params)

  if (length(json$applications) == 0) {
    return(multi_meta(board, character()))
  }

  name <- map_chr(json$applications, ~ .x$name)
  user <- map_chr(json$applications, ~ .x$owner_username)
  multi_meta(board, paste0(user, "/", name))
}


#' @rdname board_deparse
#' @export
board_deparse.pins_board_connect <- function(board, ...) {
  expr(board_connect(auth = "envvar"))
}

#' @rdname required_pkgs.pins_board
#' @export
required_pkgs.pins_board_connect <- function(x, ...) {
  ellipsis::check_dots_empty()
  "rsconnect"
}

# v0 ----------------------------------------------------------------------

#' @export
board_pin_get.pins_board_connect <- function(board, name, version = NULL, ...,
                                             extract = NULL) {

  meta <- pin_fetch(board, name, version = version, ...)
  meta$local$dir
}

#' @export
board_pin_create.pins_board_connect <- function(board, path, name,
                                                metadata, code = NULL,
                                                search_all = FALSE,
                                                ...) {

  path <- fs::dir_ls(path)
  metadata$file <- fs::path_file(path)

  pin_store(
    board = board,
    name = name,
    paths = path,
    metadata = metadata,
    ...
  )
}

#' @export
board_pin_find.pins_board_connect <- function(board,
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

  tibble::tibble(
    name = paste0(user, "/", name),
    title = map_chr(pins, ~ .x$title %||% ""),
    description = map_chr(pins, ~ .x$description %||% "")
  )
}

# Content -----------------------------------------------------------------

rsc_content_find <- function(board, name, version = NULL, warn = TRUE) {

  name <- rsc_parse_name(name)

  # https://docs.rstudio.com/connect/api/#get-/v1/content
  json <- rsc_GET(board, "v1/content", list(name = name$name))
  if (length(json) == 0) {
    abort_pin_missing(name$name)
  }

  if (is.null(name$owner)) {
    if (length(json) > 1) {
      # TODO: Find user names and offer
      cli_abort(c(
        "Multiple pins with name {.val {name$name}}",
        i = "Use a fully specified name including user name like {.val {paste0('julia/', name$name)}}"
      ))
    }
    owner <- rsc_user_name(board, json[[1]]$owner_guid)
    name$full <- paste0(owner, "/", name$name)

    if (warn) {
      cli::cli_alert_warning("Use a fully specified name including user name: {.val {name$full}}, not {.val {name$name}}.")
    }
    selected <- json[[1]]
  } else {
    owner_guids <- map_chr(json, ~ .x$owner_guid)
    owner_names <- map_chr(owner_guids, rsc_user_name, board = board)
    if (!name$owner %in% owner_names) {
      abort(paste0("Can't find pin named '", name$name, "' with owner '", name$owner, "'"))
    }
    selected <- json[[name$owner %in% owner_names]]
  }

  content <- list(
    guid = selected$guid,
    url = selected$content_url
  )
  content
}

rsc_content_create <- function(board, name, metadata, access_type = "acl") {
  name <- rsc_parse_name(name)
  if (!grepl("^[-_A-Za-z0-9]+$", name$name)) {
    abort("Posit Connect requires alpanumeric names")
  }

  body <- list(
    name = name$name,
    access_type = access_type %||% "acl",
    title = metadata$title %||% name$name, # fallback for legacy board
    description = metadata$description %||% ""
  )

  # https://docs.rstudio.com/connect/api/#post-/v1/content
  rsc_POST(board, rsc_v1("content"), body = body)
}

rsc_content_update <- function(board, guid, metadata, access_type = NULL) {
  body <- compact(list(
    access_type = access_type,
    title = metadata$title,
    description = metadata$description %||% ""
  ))

  # https://docs.rstudio.com/connect/api/#patch-/v1/content/{guid}
  rsc_PATCH(board, rsc_v1("content", guid), body = body)
}

rsc_content_info <- function(board, guid) {
  # https://docs.rstudio.com/connect/api/#get-/v1/content/{guid}
  rsc_GET(board, rsc_v1("content", guid), body = body)
}

rsc_content_versions <- function(board, guid) {
  # https://docs.rstudio.com/connect/api/#get-/v1/content/{guid}/bundles
  json <- rsc_GET(board, rsc_v1("content", guid, "bundles"))

  tibble::tibble(
    version = map_chr(json, ~ .x$id),
    created = parse_8601(map_chr(json, ~ .x$created_time)),
    active = map_lgl(json, ~ .x$active),
    size = map_dbl(json, ~ .x$size),
  )
}

rsc_content_version <- function(board, guid) {
  if (!board$use_cache_on_failure) {
    return(rsc_content_version_live(board, guid))
  }

  tryCatch(
    rsc_content_version_live(board, guid),
    error = function(cnd) {
      rsc_content_version_cached(board, guid)
    }
  )
}

rsc_content_version_live <- function(board, guid) {
  rsc_GET(board, rsc_v1("content", guid))$bundle_id
}

rsc_content_version_cached <- function(board, guid) {
  bundles <- fs::dir_ls(fs::path(board$cache, guid))
  # Should really check that all files also exist, but using only
  # pin_meta() and not pin_read() should be relatively unusual
  meta <- fs::path(bundles, "data.txt")
  meta <- meta[fs::file_exists(meta)]

  if (length(meta) == 0) {
    abort("Failed to connect to Posit Connect")
  } else {
    cli::cli_alert_danger("Failed to connect to Posit Connect; using cached version")

    info <- fs::file_info(meta)
    meta <- meta[order(info$modification_time, decreasing = TRUE)]
    fs::path_file(fs::path_dir(meta[[1]]))
  }
}

rsc_content_delete <- function(board, name) {
  content <- rsc_content_find(board, name)
  rsc_DELETE(board, rsc_v1("content", content$guid))
  invisible(NULL)
}

rsc_parse_name <- function(x) {
  parts <- strsplit(x, "/", fixed = TRUE)[[1]]

  if (length(parts) == 1) {
    list(owner = NULL, name = parts[[1]], full = NULL)
  } else if (length(parts)) {
    list(owner = parts[[1]], name = paste0(parts[-1], collapse = "/"), full = x)
  }
}

rsc_user_name <- function(board, guid) {
  rsc_GET(board, rsc_v1("users", guid))$username
}

# helpers -----------------------------------------------------------------

rsc_path <- function(board, path) {
  board_path <- httr::parse_url(board$url)$path
  if (board_path != "" && !endsWith(board_path, "/")) {
    board_path <- paste0(board_path, "/")
  }
  paste0("/", board_path, "__api__/", path)
}

rsc_GET <- function(board, path, query = NULL, ...) {
  path <- rsc_path(board, path)
  auth <- rsc_auth(board, path, "GET", NULL)

  req <- httr::GET(
    board$url,
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

  temp <- fs::file_temp()
  req <- httr::GET(content_url, path = path, auth, httr::write_disk(temp))

  rsc_check_status(req)
  fs::file_copy(temp, dest) # only copy if request is successful
  fs::file_chmod(dest, "u=r")
  invisible()
}

rsc_DELETE <- function(board, path, query = NULL, ...) {
  path <- rsc_path(board, path)
  auth <- rsc_auth(board, path, "DELETE", NULL)

  req <- httr::DELETE(
    board$url,
    path = path,
    query = query,
    auth,
    ...
  )
  rsc_check_status(req)
  invisible()
}

rsc_POST <- function(board, path, query = NULL, body, ..., .method = "POST") {
  path <- rsc_path(board, path)

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
  auth <- rsc_auth(board, path, .method, body_path)

  req <- httr::VERB(
    .method,
    url = board$url,
    path = path,
    query = query,
    body = body,
    auth,
    ...
  )
  rsc_check_status(req)
  httr::content(req)
}

rsc_PATCH <- function(board, path, query = NULL, body, ...) {
  rsc_POST(board, path, query = query, body = body, ..., .method = "PATCH")
}

rsc_auth <- function(board, path, verb, body_path) {
  if (is.character(board$auth)) {
    httr::add_headers("Authorization" = paste("Key", board$auth))
  } else {
    # https://github.com/rstudio/connect/wiki/token-authentication#request-signing-rsconnect
    signatureHeaders <- utils::getFromNamespace("signatureHeaders", "rsconnect")
    headers <- signatureHeaders(board$auth, verb, path, body_path)
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
        paste0("Posit Connect API failed [", req$status_code, "]"),
        json$error
      ))
    } else {
      httr::stop_for_status(req)
    }
  }
}

rsc_version <- function(board) {
  rsc_GET(board, "server_settings")$version
}

rsc_v1 <- function(...) {
  paste0(c("v1", ...), collapse = "/")
}

# Testing setup -----------------------------------------------------------

board_connect_test <- function(...) {
  if (connect_has_colorado()) {
    board_connect_colorado(...)
  } else {
    board_connect_susan(...)
  }
}

# Use Colorado for local testing
connect_has_colorado <- function() {
  accounts <- rsconnect::accounts()
  "colorado.posit.co" %in% accounts$server
}

board_connect_colorado <- function(...) {
  if (!connect_has_colorado()) {
    testthat::skip("board_connect_colorado() only works with Posit's demo server")
  }
  board_connect(..., server = "colorado.posit.co", auth = "rsconnect", cache = fs::file_temp())
}

board_connect_susan <- function(...) {
  creds <- read_creds()
  board_connect(
    server = "http://localhost:3939",
    account = "susan",
    key = creds$susan_key
  )
}
board_connect_derek <- function(...) {
  creds <- read_creds()
  board_connect(
    server = "http://localhost:3939",
    account = "derek",
    key = creds$derek_key
  )
}
read_creds <- function() {
  path <- testthat::test_path("creds.rds")
  if (!file.exists(path)) {
    testthat::skip(glue("board_connect() tests requires `{path}`"))
  }
  readRDS(path)
}
add_another_user <- function(board, user_name, content_id) {

  ## get user GUID for new owner from user_name
  path <- glue("v1/users/")
  path <- rsc_path(board, path)
  auth <- rsc_auth(board, path, "GET")
  query <- glue("prefix={user_name}")
  resp <- httr::GET(board$url, path = path, query = query, auth)
  httr::stop_for_status(resp)
  res <- httr::content(resp)
  principal_guid <- res$results[[1]]$guid

  ## add user_name as owner for content at GUID
  body <- glue('{{
  "principal_guid": "{principal_guid}",
  "principal_type": "user",
  "role": "owner"
  }}')

  path <- glue("v1/content/{content_id}/permissions")
  path <- rsc_path(board, path)
  auth <- rsc_auth(board, path, "POST")
  resp <- httr::POST(board$url, path = path, body = body, auth)
  httr::stop_for_status(resp)
  invisible(resp)
}
