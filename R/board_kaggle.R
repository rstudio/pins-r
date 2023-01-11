#' Use kaggle datasets/competitions as a board
#'
#' @description
#' These functions are no longer supported because of changes to the Kaggle
#' API and will be removed in a future version of pins. We recommend that you
#' use the [Kaggle CLI](https://www.kaggle.com/docs/api) instead.
#'
#' `board_kaggle_competition()` allows you to treat a Kaggle competition like
#' a read-only board, making it easy get the data on to your computer.
#' `board_kaggle_dataset()` lets you upload and download files to and from a
#' kaggle dataset. Data is only re-downloaded when it changes.
#'
#' These boards work best with `pin_download()` and `pin_upload()` since
#' `pin_read()` and `pin_write()` are not a good fit to the Kaggle model.
#'
#' @name board_kaggle
#' @keywords internal
#' @param username,key Typically you'll authenticate using the
#'   `"~/.kaggle/kaggle.json"` file downloaded from your account page
#'   (by clicking "Create New API Token". However, if necessary you can supply
#'   the `username` and `key` arguments here; this can be useful for testing.
#' @inheritParams new_board
NULL

# competitions ------------------------------------------------------------

#' @rdname board_kaggle
#' @export
#' @examples
#' \dontrun{
#' board <- board_kaggle_competitions()
#' board
#'
#' board %>% pin_meta("titanic")
#' paths <- board %>% pin_download("titanic")
#' paths
#' head(read.csv(paths[[1]]))
#' head(read.csv(paths[[2]]))
#' }
board_kaggle_competitions <- function(username = NULL, key = NULL, cache = NULL) {
  auth_info <- kaggle_authenticate(username, key)
  cache <- cache %||% board_cache_path("kaggle-competition")

  new_board_v1("pins_board_kaggle_competition",
    cache = cache,
    auth = httr::authenticate(auth_info$username, auth_info$key),
    username = auth_info$username
  )
}

board_kaggle_competitions_test <- function() {
  envvars <- c("PINS_KAGGLE_USERNAME", "PINS_KAGGLE_KEY")
  if (!has_envvars(envvars)) {
    testthat::skip(paste0("Kaggle tests require env vars ", paste0(envvars, collapse = ", ")))
  }

  board_kaggle_competitions(
    username = Sys.getenv(envvars[[1]]),
    key = Sys.getenv(envvars[[2]]),
    cache = tempfile()
  )
}


#' @rdname board_kaggle
#' @export
pin_search.pins_board_kaggle_competition <- function(
                                                 board,
                                                 search = NULL,
                                                 sort_by = c("grouped", "prize", "earliestDeadline", "latestDeadline", "numberOfTeams", "recentlyCreated"),
                                                 page = 1,
                                                 user = NULL,
                                                 ...) {
  sort_by <- arg_match(sort_by)
  json <- kaggle_get(board, "competitions/list",
    query = list(
      search = search,
      sortBy = sort_by,
      user = user,
      page = page
    )
  )
  tibble::tibble(
    name = map_chr(json, ~ .$ref),
    type = "file",
    description = map_chr(json, ~ .$title),
    created = parse_8601(map_chr(json, ~ .$enabledDate)),
    deadline = parse_8601(map_chr(json, ~ .$deadline)),
  )
}


#' @export
pin_exists.pins_board_kaggle_competition <- function(board, name, ...) {
  check_name(name)

  tryCatch(
    {
      kaggle_get(board, paste0("competitions/data/list/", name))
      TRUE
    },
    http_403 = function(e) FALSE
  )
}


#' @export
pin_delete.pins_board_kaggle_competition <- function(board, names, ...) {
  abort_board_read_only("board_kaggle_competitions")
}

#' @export
pin_store.pins_board_kaggle_competition <- function(board, name, paths, metadata,
                                              versioned = NULL, ...) {
  abort_board_read_only("board_kaggle_competitions")
}

#' @export
pin_list.pins_board_kaggle_competition <- function(board, ...) {
  NA
}

#' @export
pin_meta.pins_board_kaggle_competition <- function(board, name, ...) {
  check_name(name)

  resp <- kaggle_get(board, "competitions/list", query = list(search = name))
  match <- map_lgl(resp, ~ .x$ref == name)
  if (sum(match) == 0) {
    abort(glue("Can't find competition called '{name}'"))
  }
  competition <- resp[match][[1]]

  # Metadata can't be cached
  data <- kaggle_get(board, glue("competitions/data/list/{name}"))
  files <- map_chr(data, "[[", "ref")
  size <- map_int(data, "[[", "totalBytes")

  meta <- list(
    description = competition$title,
    file = files,
    file_size = sum(size),
    type = "file",
    created = parse_8601(competition$enabledDate),
    api_version = 1
  )
  local_meta(meta,
    name = name,
    version = NA_character_,
    dir = fs::path(board$cache, name),
    url = competition$url
  )
}


#' @export
pin_fetch.pins_board_kaggle_competition <- function(board, name, ...) {
  meta <- pin_meta(board, name)

  for (file in meta$file) {
    file_url <- utils::URLencode(file, reserved = TRUE)
    url <- kaggle_url("competitions", "data", "download", name, file_url)

    fs::dir_create(fs::path_dir(fs::path(meta$local$dir, file)))
    http_download(url, path_dir = meta$local$dir, path_file = file, board$auth, on_failure = kaggle_json)
  }

  meta
}

#' @rdname board_deparse
#' @export
board_deparse.pins_board_kaggle_competition <- function(board, ...) {
  'board_kaggle_competitions()'
}


# datasets ----------------------------------------------------------------

# https://github.com/Kaggle/kaggle-api/blob/master/KaggleSwagger.yaml

#' @rdname board_kaggle
#' @export
#' @examples
#' \dontrun{
#' board <- board_kaggle_dataset()
#'
#' board %>% pin_search("cats")
#' board %>% pin_exists("rturley/pet-breed-characteristics")
#' board %>% pin_meta("rturley/pet-breed-characteristics")
#' board %>% pin_versions("rturley/pet-breed-characteristics")
#'
#' board %>% pin_versions("imsparsh/animal-breed-cats-and-dogs")
#' }
board_kaggle_dataset <- function(username = NULL, key = NULL, cache = NULL) {
  auth_info <- kaggle_authenticate(username, key)
  cache <- cache %||% board_cache_path("kaggle")

  new_board_v1("pins_board_kaggle_dataset",
    cache = cache,
    auth = httr::authenticate(auth_info$username, auth_info$key),
    username = auth_info$username
  )
}

board_kaggle_dataset_test <- function() {
  envvars <- c("PINS_KAGGLE_USERNAME", "PINS_KAGGLE_KEY")
  if (!has_envvars(envvars)) {
    testthat::skip(paste0("Kaggle tests require env vars ", paste0(envvars, collapse = ", ")))
  }

  board_kaggle_dataset(
    username = Sys.getenv(envvars[[1]]),
    key = Sys.getenv(envvars[[2]]),
    cache = tempfile()
  )
}

#' @export
pin_list.pins_board_kaggle_dataset <- function(board, ...) {
  NA
}

#' @export
pin_delete.pins_board_kaggle_dataset <- function(board, names, ...) {
  check_pin_exists(board, names[[1]])

  meta <- pin_meta(board, names[[1]])

  abort(c(
    "Kaggle datasets can only be deleted from the web interface",
    i = glue("Click 'Delete Dataset' on <{meta$url}/settings>")
  ))
}

#' @rdname board_kaggle
#' @inheritParams pin_search
#' @param sort_by How to sort the results.
#' @param page Which page of results to retrieve.
#' @param user If non-`NULL` filter to specified user.
#' @export
pin_search.pins_board_kaggle_dataset <- function(
                                                 board,
                                                 search = NULL,
                                                 sort_by = c("hottest", "votes", "updated", "active"),
                                                 page = 1,
                                                 user = NULL,
                                                 ...) {
  sort_by <- arg_match(sort_by)
  json <- kaggle_get(board, "datasets/list",
    query = list(
      search = search,
      sortBy = sort_by,
      user = user,
      page = page
    )
  )

  tibble::tibble(
    name = map_chr(json, ~ .$ref),
    type = "file",
    description = map_chr(json, ~ .$title),
    created = parse_8601(map_chr(json, ~ .$lastUpdated)),
    file_size = fs::as_fs_bytes(map_dbl(json, ~ .$totalBytes)),
    license = map_chr(json, ~ .$licenseName)
  )
}

#' @export
pin_exists.pins_board_kaggle_dataset <- function(board, name, ...) {
  kaggle_check_name(name)

  tryCatch(
    {
      kaggle_get(board, paste0("datasets/list/", name))
      TRUE
    },
    http_403 = function(e) FALSE
  )
}

#' @export
pin_meta.pins_board_kaggle_dataset <- function(board, name, version = NULL, ...) {
  kaggle_check_name(name)
  view <- kaggle_get(board, paste0("datasets/view/", name))
  list <- kaggle_get(board, paste0("datasets/list/", name))

  meta <- list(
    description = view$title,
    license = view$licenseName,
    file = map_chr(list$datasetFiles, ~ .$name),
    file_size = fs::as_fs_bytes(view$totalBytes),
    type = "file",
    created = parse_8601(view$lastUpdated),
    api_version = 1
  )
  local_meta(meta,
    name = name,
    version = view$currentVersionNumber,
    dir = fs::path(board$cache, name),
    url = view$url
  )
}

#' @export
pin_fetch.pins_board_kaggle_dataset <- function(board, name, version = NULL, ...) {
  meta <- pin_meta(board, name)

  for (file in meta$file) {
    url <- glue("https://www.kaggle.com/api/v1/datasets/download/{name}/{file}")
    if (!is.null(version)) {
      url <- paste0(url, "?datasetVersionNumber=", version)
    }
    fs::dir_create(fs::path_dir(fs::path(meta$local$dir, file)))
    http_download(url, meta$local$dir, file, board$auth)
  }

  meta
}

#' @export
pin_versions.pins_board_kaggle_dataset <- function(board, name, ...) {
  kaggle_check_name(name)
  view <- kaggle_get(board, paste0("datasets/view/", name))
  versions <- view$versions

  tibble::tibble(
    version = map_int(versions, ~ .$versionNumber),
    created = parse_8601(map_chr(versions, ~ .$creationDate)),
    notes = map_chr(versions, ~ .$versionNotes),
    status = map_chr(versions, ~ .$status)
  )

}

#' @export
#' @rdname board_kaggle
#' @inheritParams pin_store
#' @param private Should the dataset be private (`TRUE`, the default)
#'   or public (`FALSE`)?
#' @param license How should the data be licensed?
pin_store.pins_board_kaggle_dataset <- function(board, name, paths, metadata,
                                    versioned = NULL, ...,
                                    private = TRUE,
                                    license = "CC0-1.0") {

  check_name(name)
  versioned <- versioned %||% TRUE

  tokens <- map_chr(paths, kaggle_upload_file, board = board)

  if (!pin_exists(board, paste0(board$username, "/", name))) {
    url <- kaggle_url("datasets/create/new")

    body <- list(
      ownerSlug = board$username,
      slug = name,
      files = data.frame(token = tokens),
      convertToCsv = FALSE,
      isPrivate = private,
      licenseName = license,
      title = metadata$description,
      categories = NULL
    )
  } else {
    url <- kaggle_url("datasets/create/version", board$username, name)
    notes <- metadata$notes %||% "Uploaded by pins package"
    body <- list(
      convertToCsv = FALSE,
      files = data.frame(token = tokens),
      versionNotes = notes,
      deleteOldVersions = !versioned
    )
  }

  resp <- httr::POST(url, board$auth,
    body = body,
    encode = "json"
  )
  kaggle_json(resp, "store pin")

  paste0(board$username, "/", name)
}

#' @rdname board_deparse
#' @export
board_deparse.pins_board_kaggle_dataset <- function(board, ...) {
  'board_kaggle_dataset()'
}

kaggle_upload_file <- function(board, path) {
  content_length <- file.info(path)$size
  modified <- as.integer(file.info(path)$mtime)

  url <- kaggle_url("datasets/upload/file", content_length, modified)
  resp <- httr::POST(url,
    body = list(fileName = basename(path)),
    board$auth,
    encode = "form"
  )
  json <- kaggle_json(resp, "Upload registration failed")
  token <- json$token

  resp <- httr::PUT(json$createUrl,
    body = httr::upload_file(path),
    board$auth
  )
  json <- kaggle_json(resp, "Upload failed")

  token
}

# Helpers -----------------------------------------------------------------

kaggle_authenticate <- function(username = NULL, key = NULL) {
  if (is.null(username) != is.null(key)) {
    abort("Must either supply both of `username` and `key` or neither")
  }

  if (!is.null(username)) {
    list(username = username, key = key)
  } else {
    path <- "~/.kaggle/kaggle.json"
    if (!fs::file_exists(path)) {
      abort(glue("Can't find {path}; you must supply `username` and `key"))
    }
    jsonlite::read_json(path)
  }
}

kaggle_json <- function(resp, task = "retrieve data") {
  json <- httr::content(resp, encoding = "UTF-8")

  if (httr::http_error(resp)) {
    if (!is.null(json$message)) {
      abort(c(paste0("Failed to ", task), json$message))
    } else {
      httr::stop_for_status(resp, task)
    }
  }
  if (!is.null(json$hasError) && json$hasError) {
    abort(c(paste0("Failed to ", task), json$error))
  }

  json
}

kaggle_get <- function(board, path, ...) {
  url <- paste0("https://www.kaggle.com/api/v1/", path)
  resp <- httr::GET(url, board$auth, ...)
  httr::stop_for_status(resp)

  httr::content(resp, as = "parsed")
}

kaggle_url <- function(...) {
  file.path("https://www.kaggle.com/api/v1", ...)
}

kaggle_check_name <- function(name) {
  if (!is_string(name)) {
    abort("`name` must be a single string")
  }

  pieces <- strsplit(name, "/", fixed = TRUE)[[1]]
  if (length(pieces) != 2) {
    abort("`name` must be of form {owner}/{dataset}")
  }

  name
}
