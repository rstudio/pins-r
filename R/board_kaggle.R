kaggle_authneticate <- function(username = NULL, key = NULL) {
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

board_kaggle_competitions <- function(username = NULL, key = NULL, cache = NULL) {
  auth <- kaggle_authneticate(username, key)
  cache <- cache %||% board_cache_path("kaggle-competition")

  new_board_v1("pins_board_kaggle_competition",
    cache = cache,
    auth = auth
  )
}

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
  resp <- kaggle_get(board, "competitions/list", query = list(search = name))
  match <- map_lgl(resp, ~ .x$ref == name)
  if (sum(match) == 0) {
    abort(glue("Can't find competition called '{name}'"))
  }
  competition <- resp[match][[1]]

  # Can this be cached?
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


# Helpers -----------------------------------------------------------------

kaggle_get <- function(board, path, ...) {
  url <- paste0("https://www.kaggle.com/api/v1/", path)
  resp <- httr::GET(url, board$auth, ...)
  httr::stop_for_status(resp)

  httr::content(resp, as = "parsed")
}

