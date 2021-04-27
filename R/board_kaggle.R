#' Use a Kaggle board
#'
#' To use a Kaggle board, you need to first download a token file from
#' <https://www.kaggle.com/me/account>.
#'
#' # Sharing
#'
#' When working in teams, you might want to share your pins with others. For
#' You can do by adding users or making the dataset public on Kaggle's website.
#'
#' Once you share with specific users, they can follow the same steps to
#' register a Kaggle board which allows them to download and upload pins
#'
#' @param token The Kaggle token as a path to the `kaggle.json` file, can
#'   be `NULL` if the `~/.kaggle/kaggle.json` file already exists.
#' @keywords internal
#' @examples
#' \dontrun{
#' # the following example requires a Kaggle API token
#' board <- board_kaggle(token = "path/to/kaggle.json")
#'
#' pin_find("crowdflower", board = board)
#'
#' # names starting with c/ are competitions
#' pin_get("c/crowdflower-weather-twitter", board = board)
#' }
#' @export
board_kaggle <- function(token = NULL, name = "kaggle", ...) {
  token <- token %||% "~/.kaggle/kaggle.json"
  if (!file.exists(token)) {
    stop("Kaggle token file '", token, "' does not exist.", call. = FALSE)
  }

  board <- new_board(
    "pins_board_kaggle",
    name = name,
    token = token,
    ...
  )

  board
}

kaggle_auth_info <- function(board) {
  jsonlite::read_json(board$token)
}

kaggle_auth <- function(board) {
  kaggle_keys <- kaggle_auth_info(board)

  httr::authenticate(
    kaggle_keys$username,
    kaggle_keys$key
  )
}

kaggle_qualify_name <- function(name, board) {
  qualified <- name
  if (!grepl("/", qualified)) qualified <- paste0(kaggle_auth_info(board)$username, "/", name)

  qualified
}

kaggle_upload_resource <- function(path, board) {
  path <- normalizePath(path)
  if (!file.exists(path)) stop("Invalid path: ", path)

  content_length <- file.info(path)$size
  modified <- as.integer(file.info(path)$mtime)

  url <- paste0("https://www.kaggle.com/api/v1/datasets/upload/file/", content_length, "/", modified)

  results <- httr::POST(url, body = list(fileName = basename(path)), kaggle_auth(board))

  if (httr::http_error(results)) stop("Upload registration failed with status ", httr::status_code(results))

  parsed <- httr::content(results, encoding = "UTF-8")

  if (!identical(parsed$error, NULL)) stop("Upload registration failed: ", parsed$error)

  upload_url <- parsed$createUrl
  token <- parsed$token

  results <- httr::PUT(upload_url,
    body = httr::upload_file(normalizePath(path)), kaggle_auth(board),
    http_utils_progress("up", size = file.info(normalizePath(path))$size)
  )

  if (httr::http_error(results)) stop("Upload failed with status ", httr::status_code(results))

  parsed <- httr::content(results, encoding = "UTF-8")

  if (!identical(parsed$error, NULL)) stop("Upload failed: ", parsed$error)

  token
}

kaggle_create_resource <- function(name, description, token, type, metadata, notes, board) {
  if (kaggle_resource_exists(board, name)) {
    owner <- kaggle_auth_info(board)$username
    url <- paste("https://www.kaggle.com/api/v1/datasets/create/version", owner, name, sep = "/")

    if (is.null(notes)) notes <- paste("Updated version")

    body <- list(
      convertToCsv = jsonlite::unbox(FALSE),
      files = data.frame(
        token = token
      ),
      versionNotes = jsonlite::unbox(notes),
      title = jsonlite::unbox(description),
      deleteOldVersions = jsonlite::unbox(identical(board$versions, FALSE))
    )
  }
  else {
    url <- "https://www.kaggle.com/api/v1/datasets/create/new"

    body <- list(
      convertToCsv = jsonlite::unbox(FALSE),
      files = data.frame(
        token = token
      ),
      isPrivate = jsonlite::unbox(TRUE),
      licenseName = jsonlite::unbox("CC0-1.0"),
      ownerSlug = jsonlite::unbox(kaggle_auth_info(board)$username),
      slug = jsonlite::unbox(name),
      title = jsonlite::unbox(description),
      categories = list()
    )
  }

  results <- httr::POST(url, body = body, kaggle_auth(board), encode = "json")

  if (httr::http_error(results)) stop("Resource creation failed with status ", httr::status_code(results))

  parsed <- httr::content(results, encoding = "UTF-8")

  if (!identical(parsed$error, NULL)) stop("Resource creation failed: ", parsed$error)

  parsed$url
}

kaggle_create_bundle <- function(path, type, description) {
  bundle_path <- tempfile()
  dir.create(bundle_path)
  on.exit(unlink(bundle_path, recursive = TRUE))

  if (dir.exists(path)) {
    file.copy(file.path(path, dir(path)), bundle_path, recursive = TRUE)
  }
  else {
    file.copy(path, bundle_path)
  }

  bundle_file <- tempfile(fileext = ".zip")
  withr::with_dir(
    bundle_path,
    zip::zipr(bundle_file, dir(bundle_path))
  )

  bundle_file
}

#' @export
board_pin_create.pins_board_kaggle <- function(board, path, name, metadata, notes = NULL, ...) {
  description <- metadata$description
  type <- metadata$type

  if (is.null(description) || nchar(description) == 0) description <- paste("A pin for the", gsub("-pin$", "", name), "dataset")
  if (!file.exists(path)) stop("File does not exist: ", path)

  if (identical(list(...)$use_zip, TRUE)) {
    temp_bundle <- kaggle_create_bundle(path, type, description)
    on.exit(unlink(temp_bundle))

    token <- kaggle_upload_resource(temp_bundle, board)
  }
  else {
    token <- list()

    if (dir.exists(path)) {
      upload_files <- file.path(path, dir(path))
    }
    else {
      upload_files <- path
    }

    for (upload_file in upload_files) {
      token[[length(token) + 1]] <- kaggle_upload_resource(upload_file, board)
    }

    token <- unlist(token)
  }

  kaggle_create_resource(name, description, token, type, metadata, notes, board)

  qualified_name <- paste0(kaggle_auth_info(board)$username, "/", name)

  board_wait_create(board, qualified_name)
}

board_pin_search_kaggle <- function(board, text = NULL, base_url = "https://www.kaggle.com/api/v1/datasets/list?") {
  if (identical(text, NULL) || length(text) == 0 || nchar(text) == 0) {
    params <- "group=my"
  }
  else {
    params <- paste0("search=", text)
  }

  url <- utils::URLencode(paste0(base_url, params))

  results <- httr::GET(url, kaggle_auth(board))
  if (httr::http_error(results)) stop("Finding pin failed with status ", httr::status_code(results))

  httr::content(results, encoding = "UTF-8")
}

#' @export
board_pin_find.pins_board_kaggle <- function(board, text, extended = FALSE, ...) {
  if (is.null(text)) text <- ""

  # clear name searches
  text <- gsub("^[^/]+/", "", text)

  # search private dataserts first since they won't search by default
  results <- board_pin_search_kaggle(board)
  reults <- Filter(function(e) grepl(text, e$ref), results)

  results <- c(results, board_pin_search_kaggle(board, text))

  if (nchar(text) > 0) {
    competitions <- board_pin_search_kaggle(board, text, base_url = "https://www.kaggle.com/api/v1/competitions/list?") %>%
      lapply(function(e) {
        e$ref <- paste0("c/", e$ref)
        e
      })
    results <- c(results, competitions)
  }

  results <- pin_entries_to_dataframe(results)

  if (identical(extended, TRUE)) {
    results$name <- results$ref
    results$description <- results$title
    results$ref <- NULL
    results$title <- NULL
    return(results)
  }

  if (length(results) == 0) {
    return(board_empty_results())
  }

  data.frame(
    name = as.character(results$ref),
    description = as.character(results$title),
    type = "files",
    metadata = rep(as.character(jsonlite::toJSON(list(extension = "zip"), auto_unbox = TRUE)), length(results$ref)),
    stringsAsFactors = FALSE
  ) %>%
    unique()
}

kaggle_competition_files <- function(board, name) {
  url <- paste0("https://www.kaggle.com/api/v1/competitions/data/list/", gsub("^c/", "", name))

  results <- httr::GET(url, kaggle_auth(board))
  if (httr::http_error(results)) stop("Finding pin failed with status ", httr::status_code(results))

  httr::content(results, encoding = "UTF-8")
}

#' @export
board_pin_get.pins_board_kaggle <- function(board, name, extract = NULL, version = NULL, ...) {
  if (grepl("^c/", name)) {
    name <- gsub("^c/", "", name)
    if (!grepl("/", name)) {
      files <- kaggle_competition_files(board, name)
    }
    url <- paste0("https://www.kaggle.com/api/v1/competitions/data/download-all/", name)
    download_name <- paste0(name, ".zip") # it's always a zip file

    etag <- max(sapply(files, function(e) e$creationDate))
    content_length <- sum(sapply(files, function(e) e$totalBytes))
  }
  else {
    if (!grepl("/", name)) name <- paste(kaggle_auth_info(board)$username, name, sep = "/")
    url <- paste0("https://www.kaggle.com/api/v1/datasets/download/", name)
    download_name <- NULL # use the default name (found with basename)

    extended <- pin_find(name = name, board = board, extended = TRUE)

    etag <- if (is.null(extended$lastUpdated)) "" else as.character(extended$lastUpdated)
    content_length <- if (is.null(extended$totalBytes)) 0 else as.integer(extended$totalBytes)
  }

  subpath <- name
  if (!is.null(version)) {
    url <- paste0(url, "?datasetVersionNumber=", version)
    subpath <- file.path(name, pin_versions_path_name(), version)
    etag <- NULL
  }

  local_path <- pin_download_files(url,
    name,
    board,
    config = kaggle_auth(board),
    custom_etag = etag,
    extract = !identical(extract, FALSE),
    content_length = content_length,
    subpath = subpath,
    download_name = download_name
  )

  local_path
}

#' @export
board_pin_remove.pins_board_kaggle <- function(board, name, ...) {
  qualified <- kaggle_qualify_name(name, board)
  stop("Please remove dataset from: https://www.kaggle.com/", qualified, "/settings")
}

#' @export
board_browse.pins_board_kaggle <- function(board, ...) {
  utils::browseURL("https://www.kaggle.com/datasets?tab=my")
}

#' @export
board_pin_versions.pins_board_kaggle <- function(board, name, ...) {
  if (!grepl("/", name)) name <- paste(kaggle_auth_info(board)$username, name, sep = "/")

  url <- paste0("https://www.kaggle.com/api/v1/datasets/view/", name)

  response <- httr::GET(url, kaggle_auth(board))

  if (httr::http_error(response)) stop("Failed to view dataset with status ", httr::status_code(response))

  parsed <- httr::content(response, encoding = "UTF-8")

  if (httr::http_error(response)) {
    stop("Failed to retrieve commits from ", board$repo, ": ", parsed$message)
  }

  tibble::tibble(
    version = sapply(parsed$versions, function(e) e$versionNumber),
    created = sapply(parsed$versions, function(e) e$creationDate),
    author = sapply(parsed$versions, function(e) e$creatorName),
    message = sapply(parsed$versions, function(e) e$versionNotes),
  )
}

kaggle_resource_exists <- function(board, name) {
  if (!grepl("/", name)) name <- paste(kaggle_auth_info(board)$username, name, sep = "/")

  url <- paste0("https://www.kaggle.com/api/v1/datasets/view/", name)

  response <- httr::GET(url, kaggle_auth(board))

  !httr::http_error(response)
}


board_wait_create <- function(board, name) {
  retrieved <- NULL
  retries <- 10
  while (retries > 0 && is.null(retrieved)) {
    retrieved <- suppressWarnings(tryCatch(
      {
        pin_get(name, board)
      },
      error = function(e) NULL
    ))

    Sys.sleep(1)
    retries <- retries - 1
  }
}
