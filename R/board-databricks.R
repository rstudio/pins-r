#' @export
board_databricks <- function(
    folder_url,
    host = NULL,
    prefix = NULL,
    versioned = TRUE,
    cache = NULL) {
  check_installed("httr2")

  cache_path <- tolower(fs::path("databricks", folder_url, prefix %||% ""))
  cache_path <- paste0(strsplit(cache_path, "\\/")[[1]], collapse = "-")
  cache <- cache %||% board_cache_path(cache_path)
  new_board_v1(
    "pins_board_databricks",
    name = "databricks",
    folder_url = folder_url,
    host = as.character(db_get_host(host)),
    prefix = prefix,
    cache = cache,
    versioned = versioned
  )
}

board_databricks_test <- function(prefix = NULL) {
  skip_if(is.null(db_get_token()), message = "No Databricks credentials found")
  skip_if(is.null(db_get_host()), message = "No Databricks host defined")
  skip_if_missing_envvars(
    tests = "board_databricks()",
    envvars = c("PINS_DATABRICKS_FOLDER_URL")
  )
  board_databricks(
    folder_url = Sys.getenv("PINS_DATABRICKS_FOLDER_URL"),
    prefix = prefix,
    cache = tempfile()
    )
}

#' @export
pin_list.pins_board_databricks <- function(board, ...) {
  db_list_folders(board)
}

#' @export
pin_exists.pins_board_databricks <- function(board, name, ...) {
  name %in% db_list_folders(board)
}

#' @export
pin_meta.pins_board_databricks <- function(board, name, version = NULL, ...) {
  check_pin_exists(board, name)
  version <- check_pin_version(board, name, version)
  if (!version %in% db_list_folders(board, name)) {
    abort_pin_version_missing(version)
  }
  db_download_file(board, name, version, "data.txt")
  path_version <- fs::path(board$cache, name, version %||% "")
  local_meta(
    x = read_meta(path_version),
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
    board = board,
    name = name,
    new_version = version_name(metadata),
    versioned = versioned
  )
  version_dir <- fs::path(name, version)
  temp_file <- withr::local_tempfile()
  yaml::write_yaml(x = metadata, file = temp_file)
  db_upload_file(
    board = board,
    path = temp_file,
    name = version_dir,
    file_name = "data.txt"
  )
  for (path in paths) {
    db_upload_file(
      board = board,
      path = path,
      name = version_dir
    )
  }
  name
}

#' @export
pin_versions.pins_board_databricks <- function(board, name, ...) {
  paths <- db_list_folders(board, name)
  version_from_path(paths)
}

#' @export
pin_fetch.pins_board_databricks <- function(board, name, version = NULL, ...) {
  meta <- pin_meta(board, name, version = version)
  for (file in meta$file) {
    db_download_file(board, name, meta$local$version, file)
  }
  meta
}

#' @export
pin_delete.pins_board_databricks <- function(board, names, ...) {
  for (name in names) {
    check_pin_exists(board, name)
    db_delete_pin(board, name)
  }
  invisible(board)
}

#' @export
pin_version_delete.pins_board_databricks <- function(board, name, version, ...) {
  db_delete_pin(board, fs::path(name, version))
}

#' @export
board_deparse.pins_board_databricks <- function(board, ...) {
  expr(
    board_databricks(
      folder_url = !!board$folder_url,
      host = !!board$host,
      prefix = !!board$prefix,
      versioned = !!board$versioned,
      cache = !!board$cache
    )
  )
}

#' @export
required_pkgs.pins_board_databricks <- function(x, ...) {
  check_dots_empty()
  "httr2"
}

# Helpers -----------------------------------------------------------------

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

db_download_file <- function(board, name = "", version = "", file_name = "") {
  base_path <- fs::path(board$prefix %||% "", name, version)
  cache_path <- fs::path(board$cache, base_path)
  local_path <- fs::path(cache_path, file_name)
  if (fs::file_exists(local_path)) {
    return(invisible())
  }
  try(fs::dir_create(cache_path))
  full_path <- fs::path("/api/2.0/fs/files", board$folder_url, base_path, file_name)
  out <- db_req_init(board, "GET", full_path)
  out <- httr2::req_perform(out, path = local_path)
  fs::file_chmod(local_path, "u=r")
  invisible()
}

db_delete_pin <- function(board, name) {
  files <- db_list_file_paths(board, name)
  purrr::walk(files, db_delete_file, board)
  dir <- fs::path(name, db_list_folders(board, name))
  purrr::walk(dir, db_delete_folder, board)
  db_delete_folder(name, board)
  invisible()
}

db_delete_folder <- function(folder, board) {
  full_path <- fs::path(
    "/api/2.0/fs/directories",
    board$folder_url,
    board$prefix %||% "",
    folder
  )
  out <- db_req_init(board, "DELETE", full_path)
  out <- httr2::req_perform(out)
  out
}

db_delete_file <- function(path, board) {
  full_path <- fs::path("/api/2.0/fs/files", path)
  out <- db_req_init(board, "DELETE", full_path)
  out <- httr2::req_perform(out)
  out
}

db_list_file_paths <- function(board, name) {
  root_folder <- db_list_folders(board, name)
  root_files <- db_list_files(board, name, "")
  if (length(root_files) == 0) {
    root_files <- NULL
  }
  if (length(root_folder) == 0 && length(root_files) == 0) {
    return(root_folder)
  }
  out <- purrr::map(root_folder, \(x) db_list_files(board, name, x))
  if (length(out) > 0) {
    out <- purrr::reduce(out, c)
  } else {
    out <- NULL
  }
  c(out, root_files)
}

db_list_files <- function(board, name, folder = "") {
  out <- db_list_contents(board, fs::path(name, folder))
  out <- purrr::discard(out, \(x) x$is_directory)
  out <- purrr::map_chr(out, \(x) x$path)
  as.character(out)
}

db_list_folders <- function(board, path = NULL) {
  out <- db_list_contents(board, path)
  out <- purrr::keep(out, \(x) x$is_directory)
  out <- purrr::map_chr(out, \(x) x$name)
  as.character(out)
}

db_list_contents <- function(board, path = NULL) {
  full_path <- fs::path(
    "/api/2.0/fs/directories",
    board$folder_url,
    board$prefix %||% "",
    path %||% ""
  )
  out <- db_req_init(board, "GET", full_path)
  out <- try(httr2::req_perform(out), silent = TRUE)
  if(inherits(out, "try-error")) {
    cond <- attr(out, "condition")
    if(inherits(cond, "httr2_http_404")) {
      return(list())
    } else {
      return(cond)
    }
  }
  out <- httr2::resp_body_json(out)
  purrr::list_flatten(out)
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
