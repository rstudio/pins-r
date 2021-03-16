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
#' @param board The name of the board to register.
#' @param name An optional name to identify this board, defaults to the board name.
#' @param cache Cache path. Every board requires a local cache to avoid
#'   downloading files multiple times. The default stores in a standard
#'   cache location for your operating system, but you can override if needed.
#' @param versions Should this board be registered with support for versions?
#' @param ... Additional parameters required to initialize a particular board.
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
