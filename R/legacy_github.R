#' GitHub board (legacy API)
#'
#' `r lifecycle::badge('deprecated')`
#'
#' To use a GitHub board, you'll need to set up authentication, following
#' the instructions at <https://happygitwithr.com/https-pat.html#https-pat>.
#'
#' # Large Files
#'
#' A GitHub repo only supports files under 25MB in size (100MB in theory but
#' there is additional overhead when using the GitHub API). To store large
#' files, GitHub
#' [recommends](https://help.github.com/en/articles/distributing-large-binaries)
#' storing them using GitHub Releases which support up to 2GB files, which
#' is what pins uses. You don't need to do anything extra as this will happen
#' behind the scenes, but don't be surprised if pins creates releases in your
#' repo.
#'
#' @inheritParams new_board
#' @param repo The GitHub repository formatted as 'owner/repo'.
#' @param branch The branch to use to commit pins. Default, `NULL`, will
#'   use `main` or `master` if present.
#' @param token GitHub personal access token.
#'   Uses [gitcreds](https://gitcreds.r-lib.org) if not set.
#' @param path The subdirectory in the repo where the pins will be stored.
#' @param host The URL of the GitHub API. You'll need to customise
#'   this to use GitHub enterprise, e.g. `"https://yourhostname/api/v3"`.
#' @examples
#' \dontrun{
#' # the following example requires a GitHub API key
#' board <- legacy_github("owner/repo")
#' }
#' @export
#' @keywords internal
legacy_github <- function(repo,
                          branch = NULL,
                          token = NULL,
                          path = "",
                          host = "https://api.github.com",
                          name = "github",
                          cache = NULL,
                          ...) {

  cache <- cache %||% board_cache_path(name)

  board <- new_board_v0("pins_board_github",
    name = name,
    token = token,
    repo = repo,
    path = if (!is.null(path) && nchar(path) > 0) paste0(path, "/") else "",
    host = host,
    cache = cache,
    ...
  )

  # check repo exists
  check_exists <- httr::GET(github_url(board, branch = NULL), github_headers(board))
  if (httr::http_error(check_exists)) {
    stop("The repo '", board$repo, "' does not exist or can't be accessed: ", httr::content(check_exists, encoding = "UTF-8")$message)
  }

  branches <- tryCatch(github_branches(board), error = function(e) e)
  if ("error" %in% class(branches) && grepl("^Git Repository is empty", branches$message)) {
    github_update_index(board, "", "initialize repo", operation = "initialize", branch = "main")
    branches <- github_branches(board)
  }

  if (is.null(branch)) {
    if ("main" %in% branches) {
      branch <- "main"
    } else if ("master" %in% branches) {
      branch <- "master"
    } else {
      abort("No 'master' or 'main' branch present; please use `branch` argument")
    }
  } else if (is_string(branch)) {
    if (!branch %in% branches) {
      abort(c(
        "`branch` must be an existing branch.",
        glue("Can't find branch called '{branch}'")
      ))
    }
  } else {
    abort("`branch` must be a string or NULL")
  }
  board$branch <- branch

  board
}


#' @rdname legacy_github
#' @export
board_register_github <- function(name = "github",
                                  repo = NULL,
                                  branch = NULL,
                                  token = NULL,
                                  path = "",
                                  host = "https://api.github.com",
                                  cache = NULL,
                                  ...) {
  lifecycle::deprecate_soft(
    "1.4.0",
    "board_register_github()",
    details = 'Learn more at <https://pins.rstudio.com/articles/pins-update.html>'
  )

  board <- legacy_github(
    name = name,
    repo = repo,
    branch = branch,
    token = token,
    path = path,
    host = host,
    cache = cache,
    ...
  )
  board_register2(board)
}

github_auth <- function(board) {
  if (!is.null(board$token)) {
    board$token
  } else {
    check_installed("gitcreds")
    gitcreds::gitcreds_get()$password
  }
}

github_headers <- function(board) {
  httr::add_headers(Authorization = paste("token", github_auth(board)))
}

github_update_temp_index <- function(board, path, commit, operation, name = NULL, metadata = NULL, branch = board$branch) {
  index_url <- github_url(board, branch = branch, "/contents/", board$path, "data.txt")
  response <- httr::GET(index_url, github_headers(board))

  sha <- NULL
  index <- list()
  if (!httr::http_error(response)) {
    sha <- httr::content(response, encoding = "UTF-8")$sha
    content <- httr::content(response, encoding = "UTF-8")

    # API returns contents when size < 1mb
    if (!is.null(content$content)) {
      index <- board_manifest_load(rawToChar(jsonlite::base64_dec(content$content)))
    }
    else {
      response <- httr::GET(content$download_url, github_headers(board))
      if (!httr::http_error(response)) {
        index <- board_manifest_load(httr::content(response, encoding = "UTF-8"))
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

  list(
    index_file = index_file,
    sha = sha
  )
}

github_update_index <- function(board, path, commit, operation, name = NULL, metadata = NULL, branch = board$branch) {
  index <- github_update_temp_index(board, path, commit, operation, name = NULL, metadata = NULL, branch = board$branch)
  index_file <- index$index_file
  sha <- index$sha

  file_url <- github_url(board, branch = branch, "/contents/", board$path, "data.txt")

  base64 <- base64enc_file(index_file)
  response <- httr::PUT(file_url,
    body = list(
      message = commit,
      content = base64,
      sha = sha,
      branch = branch
    ),
    github_headers(board), encode = "json"
  )

  if (httr::http_error(response)) {
    stop("Failed to update data.txt file: ", httr::content(response, encoding = "UTF-8"))
  }
}

github_delete_release <- function(board, release_tag) {
  response <- httr::GET(github_url(board, branch = NULL, paste0("/releases/tags/", release_tag)), github_headers(board))
  if (!httr::http_error(response)) {
    release_id <- httr::content(response, encoding = "UTF-8")$id
    response <- httr::DELETE(github_url(board, branch = NULL, paste0("/releases/", release_id)), github_headers(board))
    if (httr::http_error(response)) {
      pin_log("Failed to delete release ", release_id)
    }

    response <- httr::DELETE(github_url(board, branch = NULL, paste0("/git/refs/tags/", release_tag)), github_headers(board))
    if (httr::http_error(response)) {
      pin_log("Failed to delete tag ", release_tag)
    }
  }
}

github_create_release <- function(board, name) {
  index_url <- github_url(board, branch = NULL, "/commits/", board$branch)
  response <- httr::GET(index_url, github_headers(board))
  version <- "initial"
  if (!httr::http_error(response)) version <- substr(httr::content(response, encoding = "UTF-8")$sha, 1, 7)

  release_url <- github_url(board, branch = NULL, "/releases")

  release_tag <- release_name <- name
  if (board_versions_enabled(board, TRUE)) {
    release_tag <- paste(name, version, sep = "-")
    release_name <- paste(name, version)
  }
  else {
    # attempt to delete existing release
    github_delete_release(board, release_tag)
  }

  release <- list(
    tag_name = release_tag,
    target_commitish = board$branch,
    name = release_name,
    body = paste0("Storage for resource '", name, "' which is too large to be stored as a GitHub file.")
  )

  response <- httr::POST(
    release_url,
    body = release,
    github_headers(board), encode = "json"
  )

  if (httr::http_error(response)) stop("Failed to create release '", release$tag_name, "' for '", name, "': ", httr::content(response, encoding = "UTF-8")$message)

  httr::content(response, encoding = "UTF-8")
}

github_upload_release <- function(board, release, name, file, file_path) {
  asset_url <- paste0(gsub("\\{.*\\}", "", release$upload_url))

  response <- httr::POST(
    asset_url,
    query = list(name = file),
    body = httr::upload_file(normalizePath(file_path)),
    github_headers(board),
    http_utils_progress("up", size = file.info(normalizePath(file_path))$size)
  )

  if (httr::http_error(response)) stop("Failed to upload asset '", file, "': ", httr::content(response, encoding = "UTF-8")$message)

  httr::content(response, encoding = "UTF-8")$url
}

github_upload_content <- function(board, name, file, file_path, commit, sha, branch) {
  file_url <- github_url(board, branch = branch, "/contents/", board$path, name, "/", file)
  pin_log("uploading ", file_url)

  base64 <- base64enc_file(file_path)
  response <- httr::PUT(file_url,
    body = list(
      message = commit,
      content = base64,
      sha = sha,
      branch = branch
    ),
    github_headers(board), encode = "json",
    http_utils_progress("up", size = file.info(normalizePath(file_path))$size)
  )
  upload <- httr::content(response, encoding = "UTF-8")

  if (httr::http_error(response)) {
    pin_log("Failed to upload ", file, " response: ", upload)
    stop("Failed to upload ", file, " to ", board$repo, ": ", upload$message)
  }
}

github_upload_blob <- function(board, file, file_path, commit) {
  blob_url <- github_url(board, branch = NULL, "/git/blobs")
  pin_log("uploading ", file)

  base64 <- base64enc_file(file_path)
  response <- httr::POST(blob_url,
    body = list(
      content = base64,
      encoding = "base64"
    ),
    github_headers(board), encode = "json",
    http_utils_progress("up", size = file.info(normalizePath(file_path))$size)
  )
  upload <- httr::content(response, encoding = "UTF-8")

  if (httr::http_error(response)) {
    pin_log("Failed to upload ", file, " response: ", upload)
    stop("Failed to upload ", file, " to ", board$repo, ": ", upload$message)
  }

  upload
}

github_create_tree <- function(board, tree_files, base_sha) {
  tree_url <- github_url(board, branch = NULL, "/git/trees")
  pin_log("creating tree")

  response <- httr::POST(tree_url,
    body = list(
      base_tree = jsonlite::unbox(base_sha),
      tree = tree_files
    ),
    github_headers(board), encode = "json",
    http_utils_progress("up")
  )
  upload <- httr::content(response, encoding = "UTF-8")

  if (httr::http_error(response)) {
    pin_log("Failed to create tree, response: ", upload)
    stop("Failed to create tree in ", board$repo, ": ", upload$message)
  }

  upload
}

github_create_commit <- function(board, tree_sha, base_sha, commit) {
  commit_url <- github_url(board, branch = NULL, "/git/commits")
  pin_log("creating commit")

  response <- httr::POST(commit_url,
    body = list(
      message = jsonlite::unbox(commit),
      tree = jsonlite::unbox(tree_sha),
      parents = list(
        jsonlite::unbox(base_sha)
      )
    ),
    github_headers(board), encode = "json",
    http_utils_progress("up")
  )
  upload <- httr::content(response, encoding = "UTF-8")

  if (httr::http_error(response)) {
    pin_log("Failed to create commit, response: ", upload)
    stop("Failed to create commit in ", board$repo, ": ", upload$message)
  }

  upload
}

github_update_head <- function(board, branch, commit_sha) {
  ref_url <- github_url(board, branch = NULL, paste0("/git/refs/heads/", branch))
  pin_log("updating head")

  response <- httr::VERB("PATCH",
    ref_url,
    body = list(
      sha = jsonlite::unbox(commit_sha),
      force = jsonlite::unbox(FALSE)
    ),
    github_headers(board), encode = "json",
    http_utils_progress("up")
  )
  upload <- httr::content(response, encoding = "UTF-8")

  if (httr::http_error(response)) {
    pin_log("Failed to update branch, reponse: ", upload)
    stop("Failed to update branch ", branch, ": ", upload$message)
  }

  upload
}

github_refs_head <- function(board, branch) {
  ref_url <- github_url(board, branch = NULL, paste0("/git/ref/heads/", branch))
  response <- httr::GET(ref_url, github_headers(board))

  reference <- httr::content(response, encoding = "UTF-8")

  if (httr::http_error(response)) {
    pin_log("Failed to retrieve branch, reponse: ", response)
    stop("Failed to retrieve branch ", branch, ": ", reference$message)
  }

  reference
}

github_files_commit <- function(board, upload_files, branch, commit) {
  tree_files <- list()
  for (file in names(upload_files)) {
    result <- github_upload_blob(board, file, upload_files[[file]], commit)

    tree_files[[file]] <- list(
      path = jsonlite::unbox(file),
      mode = jsonlite::unbox("100644"),
      type = jsonlite::unbox("blob"),
      sha = jsonlite::unbox(result$sha)
    )
  }

  head_sha <- github_refs_head(board, branch)
  tree_result <- github_create_tree(board, unname(tree_files), head_sha$object$sha)
  commit_result <- github_create_commit(board, tree_result$sha, head_sha$object$sha, commit)

  github_update_head(board, branch, commit_result$sha)
}

#' @export
board_pin_create.pins_board_github <- function(board, path, name, metadata, ...) {
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

  release <- NULL
  release_map <- list()
  upload_files <- dir(bundle_path, recursive = TRUE)

  # first upload large files
  for (file in upload_files) {
    file_path <- file.path(bundle_path, file)

    if ((file.info(file_path)$size > getOption("pins.github.release", 25) * 10^6 || release_storage) && !identical(file, "data.txt")) {
      if (is.null(release)) release <- github_create_release(board, name)
      download_url <- github_upload_release(board, release, name, file, file_path)
      release_map[[file]] <- download_url
    }
  }

  # remove uploaded files
  upload_files <- Filter(function(e) !e %in% names(release_map), upload_files)

  # update data.txt with large files
  if ("data.txt" %in% upload_files) {
    file_path <- file.path(bundle_path, "data.txt")

    datatxt <- suppressWarnings(yaml::read_yaml(file_path, eval.expr = FALSE))

    datatxt$filenames <- as.list(datatxt$path)

    datatxt$path <- sapply(datatxt$path, function(e) {
      if (e %in% names(release_map)) release_map[[e]] else e
    })

    names(datatxt$filenames) <- datatxt$path

    write_yaml(datatxt, file_path)
  }

  # create upload definition of remote-path/local-path
  upload_defs <- file.path(bundle_path, upload_files)
  names(upload_defs) <- paste0(board$path, name, "/", upload_files)

  # update local index
  if (update_index) {
    index_path <- name

    index <- github_update_temp_index(board, index_path, commit,
      operation = "create",
      name = name, metadata = metadata, branch = branch
    )

    remote_index <- paste0(board$path, "data.txt")
    upload_defs[[remote_index]] <- index$index_file
  }

  # add remaining files in a single commit
  commit <- if (is.null(list(...)$commit)) paste("update", name) else list(...)$commit
  github_files_commit(board, upload_defs, branch, commit)
}

#' @export
board_pin_find.pins_board_github <- function(board, text, ...) {
  branch <- if (is.null(list(...)$branch)) board$branch else list(...)$branch

  result <- httr::GET(
    github_url(board, "/contents/", board$path, "data.txt", branch = branch),
    github_headers(board)
  )

  if (!httr::http_error(result)) {
    content <- httr::content(result, encoding = "UTF-8")
    if (is.null(content$content)) {
      result <- httr::GET(content$download_url, github_headers(board))
      content <- httr::content(result, encoding = "UTF-8")
    }
    else {
      content <- rawToChar(jsonlite::base64_dec(content$content))
    }

    result <- board_manifest_load(content) %>%
      pin_results_from_rows()
  }
  else {
    pin_log("Failed to find 'data.txt' file in repo '", board$repo, "', path '", board$path, "' and branch '", branch, "'.")
    result <- pin_find_empty()
  }

  if (is.character(text)) {
    result <- result[grepl(text, result$name) | grepl(text, result$description), ]
  }

  if (nrow(result) == 1) {
    # retrieve additional details if searching for only one item
    result_single <- httr::GET(
      github_url(board, branch = branch, "/contents/", board$path, result$name, "/", "data.txt"),
      github_headers(board)
    )

    if (!httr::http_error(result_single)) {
      local_path <- pin_download_files(httr::content(result_single, encoding = "UTF-8")$download_url,
        result$name,
        board,
        headers = github_headers(board),
        remove_query = TRUE
      )
      manifest <- pin_manifest_get(local_path)

      result$metadata <- as.character(jsonlite::toJSON(manifest, auto_unbox = TRUE))
    }
  }

  result
}

github_url <- function(board, branch = board$branch, ...) {
  args <- list(...)

  url <- paste0(board$host, "/repos/", board$repo, paste0(args, collapse = ""))
  if (!is.null(branch)) {
    url <- paste0(url, "?ref=", branch)
  }

  url
}

github_branches <- function(board) {
  response <- httr::GET(github_url(board, "/git", "/refs", branch = NULL), github_headers(board))
  if (httr::http_error(response)) stop(httr::content(response, encoding = "UTF-8"))

  httr::content(response, encoding = "UTF-8") %>%
    sapply(function(e) gsub("refs/heads/", "", e$ref))
}

github_branch <- function(board, branch) {
  reference <- paste0("refs/heads/", branch)

  response <- httr::GET(github_url(board, "/git", "/refs", branch = NULL), github_headers(board))

  if (httr::http_error(response)) {
    stop("Failed to retrieve branches ", as.character(httr::content(response, encoding = "UTF-8")))
  }

  branch_object <- Filter(function(e) identical(e$ref, reference), httr::content(response, encoding = "UTF-8"))

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
    github_headers(board), encode = "json"
  )

  if (httr::http_error(response)) {
    stop("Failed to create branch ", new_branch, " ", as.character(httr::content(response, encoding = "UTF-8")))
  }
}

github_content_url <- function(board, branch = board$branch, ...) {
  args <- list(...)

  url <- paste0(board$host, "/repos/", board$repo, "/contents/", paste0(board$path, paste0(args, collapse = "")))
  if (!is.null(branch)) {
    url <- paste0(url, "?ref=", branch)
  }

  url
}

github_raw_url <- function(board, branch = board$branch, ...) {
  paste0("https://raw.githubusercontent.com/", board$repo, "/", branch, "/", paste0(..., collapse = ""))
}

github_download_files <- function(index, temp_path, board) {
  for (file in index) {
    pin_log("retrieving ", file$download_url)

    if (is.list(file) && identical(file$type, "dir")) {
      sub_index <- httr::GET(file$url, github_headers(board), http_utils_progress()) %>% httr::content(encoding = "UTF-8")
      github_download_files(sub_index, file.path(temp_path, file$name), board)
    }
    else {
      if (!dir.exists(temp_path)) dir.create(temp_path, recursive = TRUE)
      httr::GET(
        file$download_url, httr::write_disk(file.path(temp_path, basename(file$download_url))),
        http_utils_progress(), github_headers(board)
      )
    }
  }
}

#' @export
board_pin_get.pins_board_github <- function(board, name, extract = NULL, version = NULL, ...) {
  branch <- if (is.null(list(...)$branch)) board$branch else list(...)$branch
  subpath <- name

  if (!is.null(version)) {
    branch <- version
    subpath <- file.path(name, pin_versions_path_name(), version)
  }

  base_url <- github_raw_url(board, branch = branch, board$path, name, "/data.txt")
  local_path <- pin_download_files(base_url, name, board, headers = github_headers(board), subpath = subpath)

  if (file.exists(file.path(local_path, "data.txt"))) {
    index_path <- pin_manifest_download(local_path, namemap = TRUE)

    for (file_idx in seq_along(index_path)) {
      file <- index_path[file_idx]
      file_url <- file
      headers <- github_headers(board)
      file_name <- if (!identical(names(index_path), NULL) && nchar(names(index_path)[file_idx]) > 0) names(index_path)[file_idx] else NULL

      if (grepl("^http://|^https://", file)) {
        # manually move authorization to url due to https://github.com/octokit/rest.js/issues/967
        file_url <- paste0(file_url, "?access_token=", github_auth(board))
        headers <- httr::add_headers(Accept = "application/octet-stream")
      }
      else {
        file_url <- github_raw_url(board, branch = branch, board$path, name, "/", file)
      }

      pin_download_files(file_url,
        name,
        board,
        headers = headers,
        extract = identical(extract, TRUE),
        subpath = subpath,
        download_name = file_name
      )
    }

    local_path
  }
  else {
    base_url <- github_raw_url(board, branch = branch, board$path, name)
    result <- httr::GET(base_url, github_headers(board))

    index <- httr::content(result, encoding = "UTF-8")

    if (httr::http_error(result)) {
      stop("Failed to retrieve ", name, " from ", board$repo, ": ", index$message)
    }

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

#' @export
board_pin_remove.pins_board_github <- function(board, name, ...) {
  update_index <- !identical(list(...)$index, FALSE)

  base_url <- github_content_url(board, name, branch = NULL)
  result <- httr::GET(paste0(base_url, "?ref=", board$branch), github_headers(board))

  index <- httr::content(result, encoding = "UTF-8")

  if (httr::http_error(result)) {
    stop("Failed to retrieve ", name, " from ", board$repo, ": ", index$message)
  }

  for (file in index) {
    pin_log("deleting ", file$name)

    commit <- if (is.null(list(...)$commit)) paste("delete", file$name) else list(...)$commit

    response <- httr::DELETE(file.path(base_url, file$name), body = list(
      message = commit,
      sha = file$sha,
      branch = board$branch
    ), github_headers(board), encode = "json")

    deletion <- httr::content(response, encoding = "UTF-8")

    if (httr::http_error(response)) {
      stop("Failed to delete ", name, " from ", board$repo, ": ", deletion$message)
    }
  }

  if (!board_versions_enabled(board, TRUE)) {
    github_delete_release(board, name)
  }

  if (update_index) github_update_index(board, paste0(board$path, name), commit, operation = "remove")
}

#' @export
board_browse.pins_board_github <- function(board, ...) {
  utils::browseURL(paste0("https://github.com/", board$repo, "/tree/", board$branch, "/", board$path))
}

#' @export
board_pin_versions.pins_board_github <- function(board, name, ...) {
  branch <- if (is.null(list(...)$branch)) board$branch else list(...)$branch

  path <- paste0(board$path, name)
  response <- httr::GET(
    github_url(board, "/commits",
      branch = NULL,
      sha = "?per_page=100&sha=", branch,
      "&path=", path
    ),
    github_headers(board)
  )

  commits <- httr::content(response, encoding = "UTF-8")

  if (httr::http_error(response)) {
    stop("Failed to retrieve commits from ", board$repo, ": ", commits$message)
  }

  tibble::tibble(
    version = sapply(commits, function(e) e$sha),
    created = sapply(commits, function(e) e$commit$author$date),
    author = sapply(commits, function(e) e$author$login),
    message = sapply(commits, function(e) e$commit$message)
  )
}

base64enc_file <- function(path) {
  r <- readBin(path, raw(), file.size(path))
  jsonlite::base64_enc(r)
}
