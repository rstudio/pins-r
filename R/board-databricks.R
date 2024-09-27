board_databricks <- function(
    catalog,
    schema,
    volume,
    host = NULL,
    cache = NULL
    ) {

  check_installed("httr2")

  cache_path <- paste0(catalog, schema, volume, collapse = "-")
  cache <- cache %||% board_cache_path(paste0("databricks-", cache_path))

  new_board_v1(
    "pins_board_databricks",
    name = "databricks",
    catalog = catalog,
    schema = schema,
    volume = volume,
    host = databricks_host(host),
    #prefix = prefix,
    cache = cache
    #versioned = versioned
  )
}

pin_list.pins_board_databricks <- function(board, ...) {
  db_req_init(
    board,
    "GET",
    "/api/2.0/fs/directories/Volumes/{board$catalog}/{board$schema}/{board$volume}"
    ) |>
    httr2::req_perform() |>
    httr2::resp_body_json() |>
    purrr::list_flatten() |>
    purrr::keep(\(x) x$is_directory) |>
    purrr::map_chr(\(x) x$name) |>
    as.character()
}

db_req_init <- function(board, method, path) {
  host_url <- httr2::url_parse(board$host)
  if (is.null(host_url$scheme)) host_url$scheme <- "https"
  host_url |>
    httr2::url_build() |>
    httr2::request() |>
    httr2::req_method(method) |>
    httr2::req_auth_bearer_token(databricks_token()) |>
    httr2::req_url_path_append(glue(path))
}

databricks_host <- function(host = NULL, fail = TRUE) {
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

databricks_token <- function(token = NULL, fail = FALSE) {
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
    token <- set_names(getDatabricksToken(databricks_host()), "oauth")
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
