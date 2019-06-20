kaggle_dependencies <- function() {

}

kaggle_auth_paths <- function() {
  normalizePath(
    "~/.kaggle/kaggle.json",
    mustWork = FALSE
  )
}

kaggle_authenticated <- function() {
  any(file.exists(kaggle_auth_paths()))
}

kaggle_auth_info <- function() {
  jsonlite::read_json(kaggle_auth_paths())
}

kaggle_auth <- function() {
  kaggle_keys <- kaggle_auth_info()

  httr::authenticate(
    kaggle_keys$username,
    kaggle_keys$key
  )
}

kaggle_qualify_name <- function(name) {
  qualified <- name
  if (!grepl("/", qualified)) qualified <- paste0(kaggle_auth_info()$username, "/", name)

  qualified
}

kaggle_upload_resource <- function(path) {
  path <- normalizePath(path)
  if (!file.exists(path)) stop("Invalid path: ", path)

  content_length <- file.info(path)$size
  modified <- as.integer(file.info(path)$mtime)

  url <- paste0("https://www.kaggle.com/api/v1/datasets/upload/file/", content_length, "/", modified)

  results <- httr::POST(url, body = list(fileName = basename(path)), config = kaggle_auth())

  if (httr::status_code(results) != 200) stop("Upload registration failed with status ", httr::status_code(results))

  parsed <- httr::content(results)

  if (!identical(parsed$error, NULL)) stop("Upload registration failed: ", parsed$error)

  upload_url <- parsed$createUrl
  token <- parsed$token

  results <- httr::PUT(upload_url, body = httr::upload_file(normalizePath(path)), config = kaggle_auth())

  if (httr::status_code(results) != 200) stop("Upload failed with status ", httr::status_code(results))

  parsed <- httr::content(results)

  if (!identical(parsed$error, NULL)) stop("Upload failed: ", parsed$error)

  token
}

kaggle_create_resource <- function(name, description, token) {
  url <- "https://www.kaggle.com/api/v1/datasets/create/new"

  body <- list(
    convertToCsv = jsonlite::unbox(FALSE),
    files = data.frame(
      token = token
    ),
    isPrivate = jsonlite::unbox(TRUE),
    licenseName = jsonlite::unbox("CC0-1.0"),
    ownerSlug = jsonlite::unbox(kaggle_auth_info()$username),
    slug = jsonlite::unbox(name),
    subtitle = jsonlite::unbox("none"),
    title = jsonlite::unbox(description),
    categories = list()
  )

  results <- httr::POST(url, body = body, config = kaggle_auth(), encode = "json")

  if (httr::status_code(results) != 200) stop("Resource creation failed with status ", httr::status_code(results))

  parsed <- httr::content(results)

  if (!identical(parsed$error, NULL)) stop("Resource creation failed: ", parsed$error)

  parsed$url
}

board_initialize.kaggle <- function(board, token = NULL, overwrite = FALSE, ...) {
  if (!is.null(token)) {
    exists <- file.exists(kaggle_auth_paths())
    if (exists && !overwrite) stop("File already exists, use 'overwrite = TRUE' to overwrite.")

    if (!exists || identical(overwrite, TRUE)) {
      token_path <- dirname(kaggle_auth_paths())
      if (!dir.exists(token_path)) dir.create(token_path, recursive = TRUE)

      if (is.list(token)) {
         jsonlite::write_json(token, kaggle_auth_paths(), auto_unbox = TRUE)
      }
      else {
        file.copy(token, kaggle_auth_paths(), overwrite = TRUE)
      }
    }
  }

  if (!kaggle_authenticated()) {
    stop("Authentication to Kaggle failed. Was a token file specified when registering this board? ")
  }

  board
}

board_pin_create.kaggle <- function(board, path, name, description, type, metadata) {
  if (is.null(description) || nchar(description) == 0) stop("Description used as kaggle title is required.")
  if (!file.exists(path)) stop("File does not exist: ", path)

  token <- kaggle_upload_resource(path)
  kaggle_create_resource(name, description, token)

  qualified_name <- paste0(kaggle_auth_info()$username, "/", name)
  pin_get(qualified_name, board$name)
}

board_pin_find.kaggle <- function(board, text, ...) {
  if (!kaggle_authenticated()) return(data.frame(name = c(), description = c(), type = c(), metadata = c()))

  # clear name searches
  text <- gsub("^[^/]+/", "", text)

  base_url <- "https://www.kaggle.com/api/v1/datasets/list?"
  if (identical(text, NULL) || length(text) == 0 || nchar(text) == 0) {
    # the api does not return all datasets so scope to user ones first
    params <- "group=my"
  }
  else {
    params <- paste0("search=", text)
  }

  url <- utils::URLencode(paste0(base_url, params))

  results <- httr::content(httr::GET(url, config = kaggle_auth()))

  results <- jsonlite::fromJSON(jsonlite::toJSON(results))

  if (identical(list(...)$extended, TRUE)) return(results)

  if (length(results) == 0) return(data.frame(name = c(), description = c(), type = c(), metadata = c()))

  data.frame(
    name = as.character(results$ref),
    description = as.character(results$title),
    type = "files",
    metadata = rep(as.character(jsonlite::toJSON(list(extension = "zip"), auto_unbox = TRUE)), length(results$ref)),
    stringsAsFactors = FALSE
  )
}

board_pin_get.kaggle <- function(board, name, details) {
  local_path <- file.path(board_local_storage("kaggle"), name)

  if (!dir.exists(local_path)) {
    url <- paste0("https://www.kaggle.com/api/v1/datasets/download/", name)
    temp_zip <- tempfile(fileext = ".zip")

    httr::content(httr::GET(url, config = kaggle_auth(), httr::write_disk(temp_zip)))

    dir.create(local_path, recursive = TRUE)
    unzip(temp_zip, exdir = local_path)
  }

  attr(local_path, "pin_type") <- "files"
  local_path
}

board_pin_remove.kaggle <- function(board, name) {
  qualified <- kaggle_qualify_name(name)
  stop("Please remove dataset from: https://www.kaggle.com/", qualified, "/settings")
}

board_info.kaggle <- function(board) {
  list(
  )
}
