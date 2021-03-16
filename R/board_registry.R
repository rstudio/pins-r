#' Board registry
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' Prior to pins 1.0.0, boards were managed using a named registry, with
#' [pin()] and [pin_get()] defaulting to a local board. Now, you must
#' explicitly supply a `board` that you've created using [board_local()],
#' [board_rsconnect()] etc.
#'
#'
#'
#' @keywords internal
#' @examples
#' # previously
#' board_register_local("myboard", cache = tempfile())
#' pin(mtcars, board = "myboard")
#' pin_get("mtcars", board = "myboard")
#'
#' # now
#' board <- board_local(cache = tempfile())
#' pin(mtcars, board = board)
#' pin_get("mtcars", board = board)
#' @export
board_register <- function(board,
                           name = NULL,
                           cache = board_cache_path(),
                           versions = NULL,
                           ...) {
  if (is_url(board)) {
    board <- board_datatxt(
      name = name,
      url = board,
      cache = cache,
      versions = versions,
      ...
    )
  } else {
    fun <- paste0("board_register_", board)
    if (!exists(fun, mode = "function")) {
      stop("Don't know how to create board of type '", board, "'", call. = FALSE)
    }
    fun <- match.fun(fun)
    board <- fun(name = name %||% board, cache = cache, versions = versions, ...)
  }

  board_register2(board)

  invisible(board$name)
}

#' @rdname board_register
#' @export
board_register_azure <- function(name = "azure",
                                 container = Sys.getenv("AZURE_STORAGE_CONTAINER"),
                                 account = Sys.getenv("AZURE_STORAGE_ACCOUNT"),
                                 key = Sys.getenv("AZURE_STORAGE_KEY"),
                                 cache = board_cache_path(),
                                 path = NULL,
                                 ...) {
  board <- board_azure(
    name = name,
    container = container,
    account = account,
    key = key,
    cache = cache,
    path = path,
    ...
  )
  board_register2(board)
}

#' @rdname board_register
#' @export
board_register_datatxt <- function(url,
                                   name = NULL,
                                   headers = NULL,
                                   cache = board_cache_path(),
                                   ...) {
  board <- board_datatxt(
    name = name,
    url = url,
    headers = headers,
    cache = cache,
    ...
  )
  board_register2(board)
}

#' @rdname board_register
#' @export
board_register_dospace <- function(name = "dospace",
                                   space = Sys.getenv("DO_SPACE"),
                                   key = Sys.getenv("DO_ACCESS_KEY_ID"),
                                   secret = Sys.getenv("DO_SECRET_ACCESS_KEY"),
                                   datacenter = Sys.getenv("DO_DATACENTER"),
                                   cache = board_cache_path(),
                                   host = "digitaloceanspaces.com",
                                   path = NULL,
                                   ...) {
  board <- board_dospace(
    name = name,
    space = space,
    key = key,
    secret = secret,
    datacenter = datacenter,
    cache = cache,
    host = host,
    path = path,
    ...
  )
  board_register2(board)
}


#' @rdname board_register
#' @export
board_register_gcloud <- function(name = "gcloud",
                                  bucket = Sys.getenv("GCLOUD_STORAGE_BUCKET"),
                                  token = NULL,
                                  cache = board_cache_path(),
                                  path = NULL,
                                  ...) {
  board <- board_gcloud(
    name = name,
    bucket = bucket,
    token = token,
    cache = cache,
    path = path,
    ...
  )
  board_register2(board)
}

#' @rdname board_register
#' @export
board_register_github <- function(name = "github",
                                  repo = NULL,
                                  branch = NULL,
                                  token = NULL,
                                  path = "",
                                  host = "https://api.github.com",
                                  cache = board_cache_path(),
                                  ...) {
  board <- board_github(
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

#' @rdname board_register
#' @export
board_register_local <- function(name = "local",
                                 cache = board_cache_path(),
                                 ...) {
  board <- board_local(name = name, cache = cache, ...)
  board_register2(board)
}

#' @rdname board_register
#' @export
board_register_kaggle <- function(name = "kaggle",
                                  token = NULL,
                                  cache = board_cache_path(),
                                  ...) {
  board <- board_kaggle("kaggle",
    name = name,
    token = token,
    cache = cache,
    ...
  )
  board_register2(board)
}

#' @rdname board_register
#' @export
board_register_rsconnect <- function(name = "rsconnect",
                                     server = NULL,
                                     account = NULL,
                                     key = NULL,
                                     output_files = FALSE,
                                     cache = board_cache_path(),
                                     ...) {
  board <- board_rsconnect(
    name = name,
    server = server,
    account = account,
    key = key,
    output_files = output_files,
    cache = cache,
    ...
  )
  board_register2(board)
}

#' @rdname board_register
#' @export
board_register_s3 <- function(name = "s3",
                              bucket = Sys.getenv("AWS_BUCKET"),
                              key = Sys.getenv("AWS_ACCESS_KEY_ID"),
                              secret = Sys.getenv("AWS_SECRET_ACCESS_KEY"),
                              cache = board_cache_path(),
                              host = "s3.amazonaws.com",
                              region = NULL,
                              path = NULL,
                              ...) {
  board_s3(
    name = name,
    bucket = bucket,
    key = key,
    secret = secret,
    cache = cache,
    region = region,
    path = path,
    ...
  )
}


board_register2 <- function(board, connect = TRUE) {
  board_registry_set(board$name, board)
  if (connect && !is_testing()) {
    # ui_viewer_register(board, "")
  }
  invisible(board)
}

# need to find the correct wrapper to support board_register_()
board_register_code <- function(board, name) {
  parent_idx <- 1
  parent_call <- NULL
  function_name <- NULL

  while (parent_idx < length(sys.parents())) {
    parent_func <- sys.function(sys.parent(parent_idx))
    parent_call <- sys.call(sys.parent(parent_idx))
    if (!is.function(parent_func) || !is.call(parent_call)) break

    this_parent_call <- tryCatch(match.call(definition = parent_func, call = parent_call), error = function(e) NULL)

    if (is.null(this_parent_call)) break
    if (length(this_parent_call) < 1) break

    this_function_name <- deparse(this_parent_call[[1]])

    if (!grepl("(^|::)board_register", this_function_name)) break

    parent_call <- this_parent_call
    function_name <- this_function_name
    parent_idx <- parent_idx + 1
  }

  header <- if (grepl("^pins::", function_name)) "" else "library(pins)\n"
  if (is.null(parent_call)) {
    paste0(header, "board_register(\"", board, "\", name = \"", name, "\")")
  }
  else {
    main_call <- paste(deparse(parent_call, width.cutoff = 500), collapse = " ")
    paste0(header, main_call)
  }
}

#' @rdname board_register
#' @export
board_deregister <- function(name, ...) {
  if (!name %in% board_registry_list()) stop("Board '", name, "' is not registered.")

  board <- board_get(name)

  if (!identical(list(...)$disconnect, FALSE)) board_disconnect(name)
  board_registry_set(name, NULL)

  invisible(NULL)
}

#' @rdname board_register
#' @export
board_default <- function() {
  board_registry_get(getOption("pins.board", "local"))
}

#' @rdname board_register
#' @export
board_list <- function() {
  board_registry_list()
}


board_registry_ensure <- function(register = TRUE) {
  if (identical(.globals$boards_registered, NULL)) {
    .globals$boards_registered <- list()
  }
}

board_registry_list <- function() {
  board_registry_ensure()

  names(.globals$boards_registered)
}

board_registry_get <- function(name) {
  board_registry_ensure()

  .globals$boards_registered[[name]]
}

board_registry_set <- function(name, board) {
  board_registry_ensure(FALSE)

  .globals$boards_registered[[name]] <- board
}
