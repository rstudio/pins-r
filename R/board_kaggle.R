#' Retrieve data from kaggle competition
#'
#' @description
#' * Data is only re-downloaded when it changes.
#' * Auth with `"~/.kaggle/kaggle.json"`
#' * Will almost always need to use `pin_download()` rather than `pin_read()`
#'
#' @param username,key Your kaggle username and key, if you choose not to use
#'   `"~/.kaggle/kaggle.json"`.
#' @inheritParams new_board
#' @export
#' @examples
#' board <- board_kaggle_competitions()
#' board
#'
#' board %>% pin_meta("titanic")
#' paths <- board %>% pin_download("titanic")
#' paths
#' head(read.csv(paths[[1]]))
#' head(read.csv(paths[[2]]))
board_kaggle_competitions <- function(username = NULL, key = NULL, cache = NULL) {
  auth <- kaggle_authenticate(username, key)
  cache <- cache %||% board_cache_path("kaggle-competition")

  new_board_v1("pins_board_kaggle_competition",
    cache = cache,
    auth = auth
  )
}


# competitions ------------------------------------------------------------

#' @export
pin_delete.pins_board_kaggle_competition <- function(board, names, ...) {
  abort("board_kaggle_competitions() is read only")
}

#' @export
pin_store.pins_board_kaggle_competition <- function(board, name, paths, metadata,
                                              versioned = NULL, ...) {
  abort("board_kaggle_competitions() is read only")
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
    url = competition$url,
    description = competition$title,
    file = files,
    file_size = sum(size),
    type = "file",
    created = parse_8601(competition$enabledDate),
    api_version = 1
  )
  local_meta(meta,
    version = NA_character_,
    dir = fs::path(board$cache, name)
  )
}

#' @export
pin_browse.pins_board_kaggle_competition <- function(board, name, ..., cache = FALSE) {
  meta <- pin_meta(board, name)

  if (cache) {
    browse_url(meta$local$dir)
  } else {
    browse_url(meta$url)
  }
}

#' @export
pin_fetch.pins_board_kaggle_competition <- function(board, name, ...) {
  meta <- pin_meta(board, name)

  for (file in meta$file) {
    url <- glue("https://www.kaggle.com/api/v1/competitions/data/download/{name}/{file}")
    fs::dir_create(fs::path_dir(fs::path(meta$local$dir, file)))
    http_download(url, meta$local$dir, file, board$auth)
  }

  meta
}


# datasets ----------------------------------------------------------------

# https://github.com/Kaggle/kaggle-api/blob/master/KaggleSwagger.yaml

#' @examples
#' board <- board_kaggle_dataset()
#' board %>% pin_search("cats")
#' board %>% pin_exists("rturley/pet-breed-characteristics")
#' board %>% pin_meta("rturley/pet-breed-characteristics")
#' board %>% pin_versions("rturley/pet-breed-characteristics")
#'
#' board %>% pin_versions("imsparsh/animal-breed-cats-and-dogs")

board_kaggle_dataset <- function(username = NULL, key = NULL, cache = NULL) {
  auth <- kaggle_authenticate(username, key)
  cache <- cache %||% board_cache_path("kaggle")

  new_board_v1("pins_board_kaggle_dataset",
    cache = cache,
    auth = auth
  )
}

#' @export
pin_list.pins_board_kaggle_dataset <- function(board, ...) {
  NA
}


#' @rdname board_kaggle_dataset
#' @export
pin_search.pins_board_kaggle_dataset <- function(
                                                 board,
                                                 pattern = NULL,
                                                 sort_by = c("hottest", "votes", "updated", "active"),
                                                 page = 1,
                                                 user = NULL,
                                                 ...) {
  sort_by <- arg_match(sort_by)
  json <- kaggle_get(board, "datasets/list",
    query = list(
      search = pattern,
      sortBy = sort_by,
      user = user,
      page = page
    )
  )

  tibble::tibble(
    name = map_chr(json, ~ .$ref),
    version = map_int(json, ~ .$currentVersionNumber),
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
    http_404 = function(e) FALSE
  )
}

#' @export
pin_meta.pins_board_kaggle_dataset <- function(board, name, version = NULL, ...) {
  kaggle_check_name(name)
  view <- kaggle_get(board, paste0("datasets/view/", name))
  list <- kaggle_get(board, paste0("datasets/list/", name))

  meta <- list(
    url = view$url,
    description = view$title,
    license = view$licenseName,
    file = map_chr(list$datasetFiles, ~ .$name),
    file_size = fs::as_fs_bytes(view$totalBytes),
    type = "file",
    created = parse_8601(view$lastUpdated),
    api_version = 1
  )
  local_meta(meta,
    version = view$currentVersionNumber,
    dir = fs::path(board$cache, name)
  )
}

#' @export
pin_fetch.pins_board_kaggle_dataset <- function(board, name, version = NULL, ...) {
  meta <- pin_meta(board, name)

  for (file in meta$file) {
    url <- glue("https://www.kaggle.com/api/v1/datasets/download/{name}/{file}")
    if (!is.null(version)) {
      url <- paste0("?datasetVersionNumber=", version)
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


# Helpers -----------------------------------------------------------------

kaggle_authenticate <- function(username = NULL, key = NULL) {
  if (is.null(username) != is.null(key)) {
    abort("Must either supply both of `username` and `key` or neither")
  }

  if (!is.null(username)) {
    httr::authenticate(username, key)
  } else {
    path <- "~/.kaggle/kaggle.json"
    if (!fs::file_exists(path)) {
      abort(glue("Can't find {path}; you must supply `username` and `key"))
    }
    json <- jsonlite::read_json(path)
    httr::authenticate(json$username, json$key)
  }
}

kaggle_get <- function(board, path, ...) {
  url <- paste0("https://www.kaggle.com/api/v1/", path)
  resp <- httr::GET(url, board$auth, ...)
  httr::stop_for_status(resp)

  httr::content(resp, as = "parsed")
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
#
#   list(
#     ref = name,
#     owner = pieces[[1]],
#     dataset = pieces[[2]]
#   )
}
