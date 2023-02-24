#' Board registry (legacy API)
#'
#' @description
#' The legacy pins API uses a board registry, where you first register a board
#' then refer to it by name in calls to pin functions.
#'
#' @examplesIf rlang::is_installed("filelock")
#' # legacy API
#' board_register_local("myboard", cache = tempfile())
#' pin(mtcars, board = "myboard")
#' pin_get("mtcars", board = "myboard")
#'
#' # modern API (not available for all boards)
#' board <- board_temp()
#' board %>% pin_write(mtcars)
#' board %>% pin_read("mtcars")
#' @keywords internal
#' @export
board_register <- function(board,
                           name = NULL,
                           cache = NULL,
                           versions = NULL,
                           ...) {
  if (is_url(board)) {
    board <- legacy_datatxt(
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
board_register_rsconnect <- function(name = "rsconnect",
                                     server = NULL,
                                     account = NULL,
                                     key = NULL,
                                     output_files = FALSE,
                                     cache = NULL,
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

board_register2 <- function(board) {
  board_registry_set(board$name, board)
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

#' @rdname board_register
#' @export
board_get <- function(name) {
  if (is.null(name)) {
    board_registry_get("local")
  } else if (is.board(name)) {
    name
  } else if (is.character(name) && length(name) == 1) {
    if (is_url(name)) {
      board <- legacy_datatxt(url = name)
      board_register2(board)
      board
    } else if (name == "packages") {
      board_packages()
    } else if (name %in% board_list()) {
      board_registry_get(name)
    } else {
      stop("Board '", name, "' not a board, available boards: ", paste(board_list(), collapse = ", "))
    }
  } else {
    stop("Invalid board specification", call. = FALSE)
  }
}


board_registry_list <- function() {
  names(.globals$boards_registered)
}

board_registry_get <- function(name) {
  .globals$boards_registered[[name]]
}

board_registry_set <- function(name, board) {
  .globals$boards_registered[[name]] <- board
}

local_register <- function(board, env = parent.frame()) {
  name <- board$name

  board_registry_set(name, board)
  withr::defer(board_registry_set(name, NULL), envir = env)

  invisible(board)
}
