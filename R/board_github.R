
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
      stop("GitHub Personal Access Token must be specified with the 'token' parameter or with the 'GITHUB_PAT' ",
           "environment variable. You can create a token at https://github.com/settings/tokens.")
    }
  }

  if (is.null(repo)) {
    stop("GitHub repository must be specified as 'owner/repo' with 'repo' parameter.")
  }

  board$token <- token
  board$repo <- repo
  board$path <- if (!is.null(path) && nchar(path) > 0) paste0(path, "/") else ""
  board$branch <- branch

  # check repo exists
  check_exists <- httr::GET(github_url(board, branch = NULL), github_headers(board))
  if (httr::http_error(check_exists)) {
    stop("The repo '", board$repo, "' does not exist or can't be accessed: ", httr::content(check_exists)$message)
  }

  branches <- tryCatch(github_branches(board), error = function(e) e)
  if ("error" %in% class(branches) && grepl("^Git Repository is empty", branches$message)) {
    github_update_index(board, "", "initialize repo", operation = "initialize", branch = "master")
    branches <- github_branches(board)
  }

  if (!identical(branch, "master") && !branch %in% branches) {
    github_branches_create(board, branch, "master")
  }

  board
}

github_update_index <- function(board, path, commit, operation, name = NULL, metadata = NULL, branch = board$branch) {
  index_url <- github_url(board, branch = branch, "/contents/", board$path, "data.txt")
  response <- httr::GET(index_url, github_headers(board))

  sha <- NULL
  index <- list()
  if (!httr::http_error(response)) {
    sha <- httr::content(response)$sha
    content <- httr::content(response)

    # API returns contents when size < 1mb
    if (!is.null(content$content)) {
      index <- board_manifest_load(rawToChar(base64enc::base64decode(content$content)))
    }
    else {
      response <- httr::GET(content$download_url, github_headers(board))
      if (!httr::http_error(response)) {
        index <- board_manifest_load(httr::content(response))
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
      metadata
    )
  }
  else if (identical(operation, "remove")) {
    if (index_pos <= length(index)) index[[index_pos]] <- NULL
  }
  else if (identical(operation, "initialize")) {
    index <- list()
  }
  else {
    stop("Operation ", operation, " is unsupported")
  }

  index_file <- tempfile(fileext = "yml")
  board_manifest_create(index, index_file)

  file_url <- github_url(board, branch = branch, "/contents/", board$path, "data.txt")

  base64 <- base64enc::base64encode(index_file)
  response <- httr::PUT(file_url,
                        body = list(
                          message = commit,
                          content = base64,
                          sha = sha,
                          branch = branch
                        ),
                        github_headers(board), encode = "json")

  if (httr::http_error(response)) {
    stop("Failed to update data.txt file: ", httr::content(response))
  }
}

github_create_release <- function(board, name) {
  index_url <- github_url(board, branch = NULL, "/commits/", board$branch)
  response <- httr::GET(index_url, github_headers(board))
  version <- "initial"
  if (!httr::http_error(response)) version <- substr(httr::content(response)$sha, 1, 7)

  release_url <- github_url(board, branch = NULL, "/releases")

  release <- list(
    tag_name = paste(name, version, sep = "-"),
    target_commitish = board$branch,
    name = paste(name, version),
    body = paste0("Storage for resource '", name, "' which is too large to be stored as a GitHub file.")
  )

  response <- httr::POST(
    release_url,
    body = release,
    github_headers(board), encode = "json")

  if (httr::http_error(response)) stop("Failed to create release '", release$tag_name, "' for '", name, "': ", httr::content(response)$message)

  httr::content(response)
}

github_upload_release <- function(board, release, name, file, file_path) {
  asset_url <- paste0(gsub("\\{.*\\}", "", release$upload_url))

  response <- httr::POST(
    asset_url,
    query = list(name = file),
    body = httr::upload_file(normalizePath(file_path),
                             http_utils_progress("up", size = file.info(normalizePath(file_path))$size)),
    github_headers(board),
    http_utils_progress("up"))

  if (httr::http_error(response)) stop("Failed to upload asset '", file, "': ", httr::content(response)$message)

  httr::content(response)$browser_download_url
}

github_upload_content <- function(board, name, file, file_path, commit, sha, branch) {
  file_url <- github_url(board, branch = branch, "/contents/", board$path, name, "/", file)
  pin_log("uploading ", file_url)

  base64 <- base64enc::base64encode(file_path)
  response <- httr::PUT(file_url,
                        body = list(
                          message = commit,
                          content = base64,
                          sha = sha,
                          branch = branch
                        ),
                        github_headers(board), encode = "json",
                        http_utils_progress("up"))
  upload <- httr::content(response)

  if (httr::http_error(response)) {
    pin_log("Failed to upload ", file, " response: ", upload)
    stop("Failed to upload ", file, " to ", board$repo, ": ", upload$message)
  }
}

board_pin_create.github <- function(board, path, name, metadata, ...) {
  update_index <- !identical(list(...)$index, FALSE)
  description <- list(...)$description
  branch <- if (is.null(list(...)$branch)) board$branch else list(...)$branch
  release_storage <- identical(list(...)$release_storage, TRUE)

  if (!file.exists(path)) stop("File does not exist: ", path)

  if (!identical(branch, board$branch)) {
    if (!branch %in% github_branches(board)) {
      github_branches_create(board, branch, board$branch)
    }
  }

  bundle_path <- tempfile()
  dir.create(bundle_path)
  on.exit(unlink(bundle_path, recursive = TRUE))

  if (dir.exists(path)) {
    file.copy(file.path(path, dir(path)), bundle_path, recursive = TRUE)
  }
  else {
    file.copy(path, bundle_path)
  }

  dir_shas <- NULL
  dir_url <- github_url(board, branch = branch, "/contents/", board$path, name)
  dir_response <- httr::GET(dir_url, github_headers(board))
  if (!httr::http_error(dir_response)) {
    dir_shas <- httr::content(dir_response)
  }

  release <- NULL
  release_map <- list()
  upload_files <- dir(bundle_path, recursive = TRUE)

  # data.txt must be last to fix release paths
  upload_files <- c(Filter(function(e) e != "data.txt", upload_files), ifelse("data.txt" %in% upload_files, "data.txt", NULL))

  for (file in upload_files) {
    commit <- if (is.null(list(...)$commit)) paste("update", name) else list(...)$commit
    named_sha <- Filter(function(e) identical(e$path, paste0(board$path, file.path(name, file))), dir_shas)
    sha <- if (length(named_sha) > 0) named_sha[[1]]$sha else NULL

    file_path <- file.path(bundle_path, file)

    if (identical(file, "data.txt")) {
      datatxt <- yaml::read_yaml(file_path, eval.expr = FALSE)
      datatxt$path <- sapply(datatxt$path, function(e) { if(e %in% names(release_map)) release_map[[e]] else e })
      yaml::write_yaml(datatxt, file_path)
    }

    if ((file.info(file_path)$size > getOption("pins.github.release", 25) * 10^6 || release_storage) && !identical(file, "data.txt")) {
      if (is.null(release)) release <- github_create_release(board, name)
      download_url <- github_upload_release(board, release, name, file, file_path)
      release_map[[file]] <- download_url
    }
    else {
      github_upload_content(board, name, file, file_path, commit, sha, branch)
    }
  }

  if (update_index) {
    index_path <- paste0(board$path, name)

    github_update_index(board, index_path, commit, operation = "create",
                        name = name, metadata = metadata, branch = branch)
  }

}

board_pin_find.github <- function(board, text, ...) {
  branch <- if (is.null(list(...)$branch)) board$branch else list(...)$branch

  result <- httr::GET(github_url(board, "/contents/", board$path, "data.txt", branch = branch),
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

    result <- board_manifest_load(content) %>%
      pin_results_from_rows()
  }
  else {
    pin_log("Failed to find 'data.txt' file in repo '", board$repo, "', path '", board$path, "' and branch '", branch, "'.")
    result <- pin_find_empty()
  }

  if (is.character(text)) {
    result <- result[grepl(text, result$name) | grepl(text, result$description),]
  }

  if (nrow(result) == 1) {
    # retrieve additional details if searching for only one item
    result_single <- httr::GET(github_url(board, branch = branch, "/contents/", board$path, result$name, "/", "data.txt"),
                               github_headers(board))

    if (!httr::http_error(result_single)) {
      local_path <- pin_download(httr::content(result_single)$download_url,
                                 result$name,
                                 board$name,
                                 headers = github_headers(board),
                                 remove_query = TRUE)
      manifest <- pin_manifest_get(local_path)

      result$metadata <- as.character(jsonlite::toJSON(manifest, auto_unbox = TRUE))
    }
  }

  result
}

github_url <- function(board, branch = board$branch, ...) {
  args <- list(...)

  url <- paste0("https://api.github.com/repos/", board$repo, paste0(args, collapse = ""))
  if (!is.null(branch))
    url <- paste0(url, "?ref=", branch)

  url
}

github_branches <- function(board) {
  response <- httr::GET(github_url(board, "/git", "/refs", branch = NULL), github_headers(board))
  if (httr::http_error(response)) stop(httr::content(response))

  httr::content(response) %>%
    sapply(function(e) gsub("refs/heads/", "", e$ref))
}

github_branch <- function(board, branch) {
  reference <- paste0("refs/heads/", branch)

  response <- httr::GET(github_url(board, "/git", "/refs", branch = NULL), github_headers(board))

  if (httr::http_error(response)) {
    stop("Failed to retrieve branches ", as.character(httr::content(response)))
  }

  branch_object <- Filter(function(e) identical(e$ref, reference), httr::content(response))

  if (length(branch_object) != 1) stop("Failed to retrieve branch ", branch)

  branch_object[[1]]
}

github_branches_create <- function(board, new_branch, base_branch) {
  reference <- paste0("refs/heads/", new_branch)
  sha <- github_branch(board, base_branch)$object$sha

  response <- httr::POST(
    github_url(board, "/git", "/refs", branch = NULL),
    body = list(
      ref = reference,
      sha = sha
    ),
    github_headers(board), encode = "json")

  if (httr::http_error(response)) {
    stop("Failed to create branch ", new_branch, " ", as.character(httr::content(response)))
  }
}

github_content_url <- function(board, branch = board$branch, ...) {
  args <- list(...)

  url <- paste0("https://api.github.com/repos/", board$repo, "/contents/", paste0(board$path, paste0(args, collapse = "")))
  if (!is.null(branch))
    url <- paste0(url, "?ref=", branch)

  url
}

github_raw_url <- function(board, branch = board$branch, ...) {
  paste0("https://raw.githubusercontent.com/", board$repo, "/", branch, "/", paste0(..., collapse = ""))
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

board_pin_get.github <- function(board, name, extract = NULL, ...) {
  branch <- if (is.null(list(...)$branch)) board$branch else list(...)$branch

  base_url <- github_raw_url(board, branch = branch, board$path, name, "/data.txt")
  local_path <- pin_download(base_url, name, board$name, headers = github_headers(board))

  if (file.exists(file.path(local_path, "data.txt"))) {
    index_path <- pin_manifest_download(local_path)

    for (file in index_path) {
      file_url <- file
      headers <- github_headers(board)

      if (grepl("^http://|^https://", file)) {
        # retrieving releases fails if auth headers are specified
        headers <- NULL
      }
      else {
        file_url <- github_raw_url(board, branch = branch, board$path, name, "/", file)
      }
      pin_download(file_url, name, board$name, headers = headers, extract = identical(extract, TRUE))
    }

    local_path
  }
  else {
    base_url <- github_raw_url(board, branch = branch, board$path, name)
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

  base_url <- github_content_url(board, name, branch = NULL)
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

board_browse.github <- function(board) {
  utils::browseURL(paste0("https://github.com/", board$repo, "/tree/",board$branch, "/", board$path))
}
