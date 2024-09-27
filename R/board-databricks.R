#' @export
board_databricks <- function(
    folder_url,
    host = NULL,
    prefix = NULL,
    versioned = TRUE,
    cache = NULL) {
  check_installed("httr2")
  cache <- cache %||% board_cache_path(paste0("databricks-", folder_url))
  new_board_v1(
    "pins_board_databricks",
    name = "databricks",
    folder_url = folder_url,
    host = db_get_host(host),
    prefix = prefix,
    cache = cache,
    versioned = versioned
  )
}

#' @export
pin_list.pins_board_databricks <- function(board, ...) {
  db_list_content(board)
}

#' @export
pin_exists.pins_board_databricks <- function(board, name, ...) {
  name %in% db_list_content(board)
}

#' @export
pin_meta.pins_board_databricks <- function(board, name, version = NULL, ...) {
  check_pin_exists(board, name)
  version <- check_pin_version(board, name, version)

  metadata_blob <- fs::path(name, version %||% "", "data.txt")
  if (!db_list_content(board, metadata_blob)) {
    abort_pin_version_missing(version)
  }
  path_version <- fs::path(board$cache, name, version %||% "")
  local_meta(
    read_meta(path_version),
    name = name,
    dir = path_version,
    version = version
  )
}

#' @export
pin_store.pins_board_databricks <- function(board, name, paths, metadata,
                                            versioned = NULL, x = NULL, ...) {
  check_dots_used()
  check_pin_name(name)
  version <- version_setup(
    board,
    name,
    version_name(metadata),
    versioned = versioned
  )
  version_dir <- fs::path(name, version)
  temp_file <- withr::local_tempfile()
  yaml::write_yaml(x = metadata, file = temp_file)
  db_upload_file(
    board = board,
    path = temp_file,
    name = name,
    file_name = "data.txt"
  )
  for (path in paths) {
    db_upload_file(
      board = board,
      path = path,
      name = name
    )
  }
  name
}

db_upload_file <- function(board, path, name = "", file_name = NULL) {
  file_name <- file_name %||% fs::path_file(path)
  full_path <- fs::path(
    "/api/2.0/fs/files",
    board$folder_url,
    board$prefix %||% "",
    name,
    file_name
  )
  out <- db_req_init(board, "PUT", full_path)
  out <- httr2::req_body_file(out, path)
  out <- httr2::req_perform(out)
  out
}

db_list_content <- function(board, path = NULL) {
  full_path <- fs::path(
    "/api/2.0/fs/directories",
    board$folder_url,
    board$prefix %||% "",
    path %||% ""
  )
  out <- db_req_init(board, "GET", full_path)
  out <- httr2::req_perform(out)
  out <- httr2::resp_body_json(out)
  out <- purrr::list_flatten(out)
  out <- purrr::keep(out, \(x) x$is_directory)
  out <- purrr::map_chr(out, \(x) x$name)
  as.character(out)
}

db_req_init <- function(board, method, path) {
  host_url <- httr2::url_parse(board$host)
  if (is.null(host_url$scheme)) host_url$scheme <- "https"
  out <- httr2::url_build(host_url)
  out <- httr2::request(out)
  out <- httr2::req_method(out, method)
  out <- httr2::req_auth_bearer_token(out, db_get_token())
  httr2::req_url_path_append(out, glue(path))
}

db_get_host <- function(host = NULL, fail = TRUE) {
  if (!is.null(host)) {
    return(set_names(host, "argument"))
  }
  env_host <- Sys.getenv("DATABRICKS_HOST", unset = NA)
  connect_host <- Sys.getenv("CONNECT_DATABRICKS_HOST", unset = NA)
  if (!is.na(env_host)) {
    host <- set_names(env_host, "environment")
  }
  if (!is.na(connect_host)) {
    host <- set_names(connect_host, "environment_connect")
  }
  if (is.null(host)) {
    if (fail) {
      cli_abort(c(
        paste0(
          "No Host URL was provided, and",
          "the environment variable 'DATABRICKS_HOST' is not set."
        ),
        "Please add your Host to 'DATABRICKS_HOST' inside your .Renviron file."
      ))
    } else {
      host <- ""
    }
  }
  host
}

db_get_token <- function(token = NULL, fail = FALSE) {
  if (!is.null(token)) {
    return(set_names(token, "argument"))
  }
  # Checks the Environment Variable
  if (is.null(token)) {
    env_token <- Sys.getenv("DATABRICKS_TOKEN", unset = NA)
    connect_token <- Sys.getenv("CONNECT_DATABRICKS_TOKEN", unset = NA)
    if (!is.na(env_token)) {
      token <- set_names(env_token, "environment")
    } else {
      if (!is.na(connect_token)) {
        token <- set_names(connect_token, "environment_connect")
      }
    }
  }
  # Checks for OAuth Databricks token inside the RStudio API
  if (is.null(token) && exists(".rs.api.getDatabricksToken")) {
    getDatabricksToken <- get(".rs.api.getDatabricksToken")
    token <- set_names(getDatabricksToken(db_get_host()), "oauth")
  }
  if (is.null(token)) {
    if (fail) {
      rlang::abort(c(
        paste0(
          "No authentication token was identified: \n",
          " - No 'DATABRICKS_TOKEN' environment variable found \n",
          " - No Databricks OAuth token found \n",
          " - Not passed as a function argument"
        ),
        "Please add your Token to 'DATABRICKS_TOKEN' inside your .Renviron file."
      ))
    } else {
      token <- ""
    }
  }
  token
}
