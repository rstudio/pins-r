
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
  board$path <- if (!is.null(path) && nchar(path) > 0) paste0(path, "/") else ""
  board$branch <- branch

  board
}

github_update_index <- function(board, path, commit, operation, name = NULL, description = NULL, type = NULL, metadata = NULL) {
  index_url <- github_url(board, "/contents/", board$path, "data.txt")
  response <- httr::GET(index_url, github_headers(board))

  sha <- NULL
  index <- list()
  if (!httr::http_error(response)) {
    sha <- httr::content(response)$sha
    content <- httr::content(response)

    # API reeturns contents when size < 1mb
    if (!is.null(content$content)) {
      index <- yaml::yaml.load(rawToChar(base64enc::base64decode(content$content)))
    }
    else {
      response <- httr::GET(content$download_url, github_headers(board))
      if (!httr::http_error(response)) {
        index <- yaml::yaml.load(httr::content(response))
      }
    }
  }

  index_matches <- sapply(index, function(e) identical(e$path, path))
  index_pos <- if (length(index_matches) > 0) which(index_matches) else length(index) + 1
  if (length(index_pos) == 0) index_pos <- length(index) + 1

  if (identical(operation, "create")) {
    metadata$columns <- NULL

    index[[index_pos]] <- c(
      list(path = path),
      if (!is.null(name)) list(name = name) else NULL,
      if (!is.null(type)) list(type = type) else NULL,
      if (!is.null(description)) list(description = description) else NULL,
      metadata
    )
  }
  else if (identical(operation, "remove")) {
    if (index_pos <= length(index)) index[[index_pos]] <- NULL
  }
  else {
    stop("Operation ", operation, " is unsupported")
  }

  index_file <- tempfile(fileext = "yml")
  yaml::write_yaml(index, index_file)

  file_url <- github_url(board, "/contents/", board$path, "data.txt")

  base64 <- base64enc::base64encode(index_file)
  response <- httr::PUT(file_url,
                        body = list(
                          message = commit,
                          content = base64,
                          sha = sha,
                          branch = board$branch
                        ),
                        github_headers(board), encode = "json")

  if (httr::http_error(response)) {
    stop("Failed to update data.txt file: ", httr::content(response))
  }
}

board_pin_create.github <- function(board, path, name, ...) {
  metadata <- if (is.null(list(...)$metadata)) "" else list(...)$metadata
  type <- if (is.null(list(...)$type)) "" else list(...)$type
  description <- if (is.null(list(...)$description)) "" else list(...)$description
  update_index <- !identical(list(...)$index, FALSE)
  description <- list(...)$description

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
    file_url <- github_url(board, "/contents/", board$path, name, "/", file)

    pin_log("uploading ", file_url)

    sha <- NULL
    response <- httr::GET(file_url, github_headers(board))
    if (!httr::http_error(response)) {
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
                          github_headers(board), encode = "json",
                          http_utils_progress("up"))
    upload <- httr::content(response)

    if (httr::http_error(response)) {
      stop("Failed to upload ", file, " to ", board$repo, ": ", upload$message)
    }
  }

  if (update_index) {
    index_path <- paste0(board$path, name)

    if (identical(type, "table")) {
      index_path <- file.path(index_path, dir(path, "\\.csv"))
    }

    github_update_index(board, index_path, commit, operation = "create",
                        description = description, name = name, type = type, metadata = metadata)
  }

}

board_pin_find.github <- function(board, text, ...) {

  result <- httr::GET(github_url(board, "/contents/", board$path, "/data.txt"),
                      github_headers(board))

  if (!httr::http_error(result)) {
    content <- httr::content(result)
    if (is.null(content$content)) {
      result <- httr::GET(content$download_url, github_headers(board))
      content <- httr::content(result)
    }
    else {
      content <- rawToChar(base64enc::base64decode(content$content))
    }

    result <- yaml::yaml.load(content) %>%
      pin_results_from_rows()
  }
  else {

    result <- httr::GET(github_url(board, "/contents/", board$path),
                        github_headers(board))

    if (httr::http_error(result)) {
      result <- data.frame(
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
        type = rep("files", length(folders)),
        metadata = rep("", length(folders)),
        stringsAsFactors = FALSE
      )

      result
    }
  }

  if (is.character(text)) {
    result <- result[grepl(text, result$name),]
  }

  if (nrow(result) == 1) {
    # retrieve additional details if searching for only one item
    result_single <- httr::GET(github_url(board, "/contents/", board$path, result$name, "/", "data.txt"),
                               github_headers(board))

    if (!httr::http_error(result_single)) {
      local_path <- pin_download(httr::content(result_single)$download_url,
                                 result$name,
                                 "github",
                                 headers = github_headers(board),
                                 remove_query = TRUE)
      manifest <- pin_manifest_get(local_path)

      result$metadata <- as.character(jsonlite::toJSON(manifest, auto_unbox = TRUE))
    }
  }

  result
}

github_url <- function(board, ...) {
  url <- paste0("https://api.github.com/repos/", board$repo, paste0(..., collapse = ""))
  if (!is.null(board$branch) && nchar(board$branch) > 0)
    url <- paste0(url, "?ref=", board$branch)

  url
}

github_content_url <- function(board, ...) {
  args <- list(...)

  use_branch <- !identical(args$branch, FALSE)
  args$branch <- NULL

  url <- paste0("https://api.github.com/repos/", board$repo, "/contents/", paste0(args, collapse = ""))
  if (!is.null(board$branch) && nchar(board$branch) > 0 && use_branch)
    url <- paste0(url, "?ref=", board$branch)

  url
}

github_raw_url <- function(board, ...) {
  paste0("https://raw.githubusercontent.com/", board$repo, "/", board$branch, "/", paste0(..., collapse = ""))
}

github_download_files <- function(index, temp_path, board) {
  for (file in index) {
    pin_log("retrieving ", file$download_url)

    if (is.list(file) && identical(file$type, "dir")) {
      sub_index <- httr::GET(file$url, github_headers(board), http_utils_progress()) %>% httr::content()
      github_download_files(sub_index, file.path(temp_path, file$name), board)
    }
    else {
      if (!dir.exists(temp_path)) dir.create(temp_path, recursive = TRUE)
      httr::GET(file$download_url, httr::write_disk(file.path(temp_path, basename(file$download_url))),
                http_utils_progress(), github_headers(board))
    }
  }
}

board_pin_get.github <- function(board, name) {
  base_url <- github_raw_url(board, board$path, name, "/data.txt")
  local_path <- pin_download(base_url, name, board$board, headers = github_headers(board))

  if (file.exists(file.path(local_path, "data.txt"))) {
    index <- pin_manifest_get(local_path)

    for (file in index$path) {
      file_url <- github_raw_url(board, board$path, name, "/", file)
      pin_download(file_url, name, board$board, headers = github_headers(board))
    }

    local_path
  }
  else {
    base_url <- github_raw_url(board, board$path, name)
    result <- httr::GET(base_url, github_headers(board))

    index <- httr::content(result)

    if (httr::http_error(result))
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
}

board_pin_remove.github <- function(board, name, ...) {
  update_index <- !identical(list(...)$index, FALSE)

  base_url <- github_content_url(board, name, branch = FALSE)
  result <- httr::GET(paste0(base_url, "?ref=", board$branch), github_headers(board))

  index <- httr::content(result)

  if (httr::http_error(result))
    stop("Failed to retrieve ", name, " from ", board$repo, ": ", index$message)

  for (file in index) {
    pin_log("deleting ", file$name)

    commit <- if (is.null(list(...)$commit)) paste("delete", file$name) else list(...)$commit

    response <- httr::DELETE(file.path(base_url, file$name), body = list(
      message = commit,
      sha = file$sha,
      branch = board$branch
    ), github_headers(board), encode = "json")

    deletion <- httr::content(response)

    if (httr::http_error(response))
      stop("Failed to delete ", name, " from ", board$repo, ": ", deletion$message)
  }

  if (update_index) github_update_index(board, paste0(board$path, name), commit, operation = "remove")
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
