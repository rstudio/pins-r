github_dependencies <- function() {

}

github_authenticated <- function(board) {
  if (!is.null(board$token))
    TRUE
  else
    nchar(Sys.getenv("GITHUB_PAT")) > 0
}

github_auth <- function(board) {
  if (!is.null(board$token))
    board$token
  else
    Sys.getenv("GITHUB_PAT")
}

github_headers <- function(board) {
  httr::add_headers(Authorization = paste("token", github_auth(board)))
}

board_initialize.github <- function(board, token = NULL, repo = NULL, path = "pins", overwrite = FALSE, ...) {
  if (!github_authenticated(board)) {
    if (is.null(token)) {
      stop("GitHub Personal Access Token must be specified with 'token' parameter to initialize board. ",
           "You can create a token at https://github.com/settings/tokens.")
    }
  }

  if (is.null(repo)) {
    stop("GitHub repository must be specified as 'owner/repo' with 'repo' parameter.")
  }

  board$token <- token
  board$repo <- repo
  board$path <- path

  board
}

github_upload_file <- function(board, path) {

}

board_pin_create.github <- function(board, path, name, description, type, metadata, ...) {
  if (is.null(description) || nchar(description) == 0) description <- paste("A pin for the", name, "dataset")
  if (!file.exists(path)) stop("File does not exist: ", path)

  bundle_path <- tempfile()
  dir.create(bundle_path)
  on.exit(unlink(bundle_path, recursive = TRUE))

  file.copy(path, bundle_path, recursive = TRUE)

  pin_manifest_create(bundle_path, type, description, metadata, dir(bundle_path, recursive = TRUE))

  for (file in dir(bundle_path, recursive = TRUE)) {
    commit <- if (is.null(list(...)$commit)) paste("update", name) else list(...)$commit
    file_url <- github_url(board, "/contents/", board$path, "/", name, "/", file)

    pin_log("uploading ", file_url)

    sha <- NULL
    response <- httr::GET(file_url, github_headers(board))
    if (httr::status_code(response) == 200) {
      sha <- httr::content(response)$sha
    }

    base64 <- base64enc::base64encode(file.path(bundle_path, file))
    response <- httr::PUT(file_url,
                          body = list(
                            message = commit,
                            content = base64,
                            sha = sha
                          ),
                          github_headers(board), encode = "json")
    upload <- httr::content(response)

    if (httr::status_code(response) < 200 || httr::status_code(response) >= 300) {
      stop("Failed to upload ", file, " to ", board$repo, ": ", upload$message)
    }
  }
}

board_pin_find.github <- function(board, text, ...) {
  result <- httr::GET(github_url(board, "/contents/", board$path),
                      github_headers(board))

  if (httr::status_code(result) != 200) {
    data.frame(
      name = character(),
      description = character(),
      type = character(),
      metadata = character(),
      stringsAsFactors = FALSE
    )
  }
  else {
    folders <- httr::content(result) %>%
      Filter(function(e) identical(e$type, "dir"), .) %>%
      sapply(function(e) e$name)

    data.frame(
      name = folders,
      description = rep("", length(folders)),
      type = rep("files", length(folders)),
      metadata = rep("", length(folders)),
      stringsAsFactors = FALSE
    )
  }
}

github_url <- function(board, ...) {
  paste0("https://api.github.com/repos/", board$repo, paste0(..., collapse = ""))
}

board_pin_get.github <- function(board, name) {
  base_url <- github_url(board, "/contents/", board$path, "/", name)
  result <- httr::GET(base_url, github_headers(board))

  index <- httr::content(result)

  if (httr::status_code(result) != 200)
    stop("Failed to retrieve ", name, " from ", board$repo, ": ", index$message)

  # need to handle case where users passes a full URL to the specific file to download
  if (!is.null(names(index))) {
    index <- list(index)
    base_url <- basename(base_url)
  }

  type <- "files"
  result <- httr::GET(file.path(base_url, "pin.json"), github_headers(board))
  if (httr::status_code(result) == 200) {
    manifest <- httr::content(result)$content %>%
      base64enc::base64decode() %>%
      rawToChar() %>%
      jsonlite::fromJSON()

    type <- manifest$type
  }

  temp_path <- tempfile()
  dir.create(temp_path)

  for (file in index) {
    pin_log("retrieving ", file$download_url)

    httr::GET(file$download_url, httr::write_disk(file.path(temp_path, basename(file$download_url))),
              github_headers(board))
  }

  attr(temp_path, "pin_type") <- type
  temp_path
}

board_pin_remove.github <- function(board, name, ...) {
  base_url <- github_url(board, "/contents/", board$path, "/", name)
  result <- httr::GET(base_url, github_headers(board))

  index <- httr::content(result)

  if (httr::status_code(result) != 200)
    stop("Failed to retrieve ", name, " from ", board$repo, ": ", index$message)

  # need to handle case where users passes a full URL to the specific file to download
  if (!is.null(names(index))) {
    index <- list(index)
    base_url <- basename(base_url)
  }

  for (file in index) {
    pin_log("deleting ", file$name)

    commit <- if (is.null(list(...)$commit)) paste("delete", file$name) else list(...)$commit

    response <- httr::DELETE(file.path(base_url, file$name), body = list(
      message = commit,
      sha = file$sha
    ), github_headers(board), encode = "json")

    deletion <- httr::content(response)

    if (httr::status_code(response) != 200)
      stop("Failed to delete ", name, " from ", board$repo, ": ", deletion$message)
  }
}

board_info.github <- function(board) {
  list(
  )
}

board_persist.github <- function(board) {
  list(
    board = board$board,
    name = board$name,
    token = board$token,
    repo = board$repo,
    path = board$path
  )
}
