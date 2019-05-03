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

board_initialize.kaggle <- function(board, token = NULL, overwrite = FALSE, ...) {
  if (!is.null(token)) {
    exists <- file.exists(kaggle_auth_paths())
    if (exists && !overwrite) stop("File already exists, use 'overwrite = TRUE' to overwrite.")

    if (!exists || identical(overwrite, TRUE)) {
      token_path <- dirname(kaggle_auth_paths())
      if (!dir.exists(token_path)) dir.create(token_path, recursive = TRUE)
      file.copy(token, kaggle_auth_paths(), overwrite = TRUE)
    }
  }
}

pin_create.kaggle <- function(board, x, name, description, type, metadata) {

}

pin_find.kaggle <- function(board, text) {
  if (!kaggle_authenticated())
    stop("Missing Kaggle token file under, configure by running: register_board(\"kaggle\", )")

  # clear name searches
  text <- gsub("^[^/]+/", "", text)

  url <- paste0("https://www.kaggle.com/api/v1/datasets/list?search=", text)
  results <- httr::content(httr::GET(url, config = kaggle_auth()))

  results <- as.data.frame(do.call("rbind", results))

  if (nrow(results) == 0) return(data.frame(name = c(), description = c(), type = c(), metadata = c()))

  data.frame(
    name = as.character(results$ref),
    description = as.character(results$title),
    type = "files",
    metadata = "{}"
  )
}

pin_retrieve.kaggle <- function(board, name) {
  local_path <- file.path(pins:::pins_local_path("kaggle"), name)

  if (!dir.exists(local_path)) {
    url <- paste0("https://www.kaggle.com/api/v1/datasets/download/", name)
    temp_zip <- tempfile(fileext = ".zip")

    httr::content(httr::GET(url, config = auth, httr::write_disk(temp_zip)))

    dir.create(local_path, recursive = TRUE)
    unzip(temp_zip, exdir = local_path)
  }

  data.frame(
    path = dir(local_path, full.names = TRUE, recursive = TRUE),
    stringsAsFactors = FALSE
  )
}

pin_remove.kaggle <- function(board, name) {

}

board_info.kaggle <- function(board) {
  if (kaggle_authenticated()) {
    install_html <- ""
  }
  else {
    install_html <- paste(
      "To search Kaggle datasets, <b>Create New API Token</b> from",
      "<a href=\"https://www.kaggle.com/me/account\">kaggle.com/me/account</a>",
      ", download and run register_board(\"<path-to-kaggle.json>\")"
    )
  }

  list(
    install_html = install_html
  )
}
