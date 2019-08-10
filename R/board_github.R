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

board_initialize.github <- function(board, token = NULL, repo = NULL, path = "", branch = "master", overwrite = FALSE, ...) {
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
  board$branch <- branch

  board
}

board_pin_create.github <- function(board, path, name, ...) {
  description <- list(...)$description

  if (is.null(description) || nchar(description) == 0) description <- paste("A pin for the", name, "dataset")
  if (!file.exists(path)) stop("File does not exist: ", path)

  bundle_path <- tempfile()
  dir.create(bundle_path)
  on.exit(unlink(bundle_path, recursive = TRUE))

  if (dir.exists(path)) {
    file.copy(file.path(path, dir(path)), bundle_path, recursive = TRUE)
  }
  else {
    file.copy(path, bundle_path)
  }

  for (file in dir(bundle_path, recursive = TRUE)) {
    commit <- if (is.null(list(...)$commit)) paste("update", name) else list(...)$commit
    file_url <- github_url(board, "/contents/", board$path, "/", name, "/", file)

    pin_log("uploading ", file_url)

    sha <- NULL
    response <- httr::GET(paste0(file_url, "?ref=", board$branch), github_headers(board))
    if (httr::status_code(response) == 200) {
      sha <- httr::content(response)$sha
    }

    base64 <- base64enc::base64encode(file.path(bundle_path, file))
    response <- httr::PUT(file_url,
                          body = list(
                            message = commit,
                            content = base64,
                            sha = sha,
                            branch = board$branch
                          ),
                          github_headers(board), encode = "json")
    upload <- httr::content(response)

    if (httr::status_code(response) < 200 || httr::status_code(response) >= 300) {
      stop("Failed to upload ", file, " to ", board$repo, ": ", upload$message)
    }
  }
}

board_pin_find.github <- function(board, text, ...) {
  result <- httr::GET(github_url(board, "/contents/", board$path, "?ref=", board$branch),
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
    folders <-  Filter(function(e) identical(e$type, "dir"), httr::content(result)) %>%
      sapply(function(e) e$name)

    result <- data.frame(
      name = folders,
      description = rep("", length(folders)),
      type = rep("", length(folders)),
      metadata = rep("", length(folders)),
      stringsAsFactors = FALSE
    )

    if (is.character(text)) {
      folders <- folders[grepl(text, folders)]
    }

    if (length(folders) == 1) {
      # retrieve additional details if searching for only one item
      result_single <- httr::GET(github_url(board, "/contents/", board$path, "/", folders, "/", "data.txt", "?ref=", board$branch),
                          github_headers(board))

      if (httr::status_code(result_single) == 200) {
        local_path <- pin_download(httr::content(result_single)$download_url,
                                   folders,
                                   "github",
                                   headers = github_headers(board),
                                   remove_query = TRUE)
        manifest <- pin_manifest_get(local_path)

        result$metadata <- as.character(jsonlite::toJSON(manifest))
      }
    }

    result
  }
}

github_url <- function(board, ...) {
  paste0("https://api.github.com/repos/", board$repo, paste0(..., collapse = ""))
}

github_download_files <- function(index, temp_path, board) {
  for (file in index) {
    pin_log("retrieving ", file$download_url)

    if (identical(file$type, "dir")) {
      sub_index <- httr::GET(file$url, github_headers(board)) %>% httr::content()
      github_download_files(sub_index, file.path(temp_path, file$name), board)
    }
    else {
      if (!dir.exists(temp_path)) dir.create(temp_path, recursive = TRUE)
      httr::GET(file$download_url, httr::write_disk(file.path(temp_path, basename(file$download_url))),
                github_headers(board))
    }
  }
}

board_pin_get.github <- function(board, name) {
  base_url <- github_url(board, "/contents/", board$path, "/", name)
  result <- httr::GET(paste0(base_url, "?ref=", board$branch), github_headers(board))

  index <- httr::content(result)

  if (httr::status_code(result) != 200)
    stop("Failed to retrieve ", name, " from ", board$repo, ": ", index$message)

  # need to handle case where users passes a full URL to the specific file to download
  if (!is.null(names(index))) {
    index <- list(index)
    base_url <- basename(base_url)
  }

  temp_path <- tempfile()
  dir.create(temp_path)

  github_download_files(index, temp_path, board)

  temp_path
}

board_pin_remove.github <- function(board, name, ...) {
  base_url <- github_url(board, "/contents/", board$path, "/", name)
  result <- httr::GET(paste0(base_url, "?ref=", board$branch), github_headers(board))

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

    response <- httr::DELETE(paste0(file.path(base_url, file$name), "?ref=", board$branch), body = list(
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
    branch = board$branch,
    path = board$path
  )
}

board_browse.github <- function(board) {
  utils::browseURL(paste0("https://github.com/", board$repo, "/tree/",board$branch, "/", board$path))
}
