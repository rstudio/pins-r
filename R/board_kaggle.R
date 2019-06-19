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

kaggle_auth <- function() {
  kaggle_keys <- jsonlite::read_json(kaggle_auth_paths())

  httr::authenticate(
    kaggle_keys$username,
    kaggle_keys$key
  )
}

kaggle_create_resource <- function(name, description, token) {
  body <- list(
    convertToCsv = jsonlite::unbox(FALSE),
    files = data.frame(
      token = token
    ),
    isPrivate = jsonlite::unbox(TRUE),
    licenseName = jsonlite::unbox("CC0-1.0"),
    ownerSlug = jsonlite::unbox("javierluraschi"),
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
  stop("Not yet implemented.")
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

}

board_info.kaggle <- function(board) {
  list(
  )
}
