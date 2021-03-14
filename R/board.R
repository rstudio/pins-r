new_board <- function(board, name, cache, versions = FALSE, ...) {

  if (is.null(cache)) stop("Please specify the 'cache' parameter.")

  fs::dir_create(fs::path(cache, name))

  board <- structure(
    list(
      board = board,
      name = name,
      cache = cache,
      versions = versions,
      ...
    ),
    class = c(board, "pins_board")
  )

  board
}

is.board <- function(x) inherits(x, "pins_board")

#' Connect to Board
#'
#' Connects to a board to activate RStudio's connection pane, when available.
#'
#' @param board The name of the board to activate.
#' @param code The code being used to registere this board.
#' @param ... Additional parameters required to initialize a particular board.
#'
#' @keywords internal
#' @export
board_connect <- function(board, code, ...) {
  board <- board_get(board)

  ui_viewer_register(board, code)

  invisible(board)
}

#' Disconnect to Board
#'
#' Disconnects board from RStudio's connection pane, when available.
#'
#' @param name The name of the board to deactivate.
#' @param ... Additional parameters required to disconnect from a particular board.
#'
#' @keywords internal
#' @export
board_disconnect <- function(name, ...) {
  board <- board_get(name)

  ui_viewer_closed(board)

  invisible(board)
}

#' List Boards
#'
#' Retrieves all available boards.
#'
#' @export
board_list <- function() {
  board_registry_list()
}

#' Get Board
#'
#' Retrieves information about a particular board.
#'
#' @param name The name of the board to use
#'
#' @export
board_get <- function(name) {
  if (is.null(name)) {
    board_default()
  } else if (is.board(name)) {
    name
  } else if (is.character(name) && length(name) == 1) {
    if (is_url(name)) {
      # TODO: remove magic registration
      board <- board_datatxt(url = name)
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

#' Register Board
#'
#' Registers a board, useful to find resources with `pin_find()` or pin to
#' additional boards with `pin()`.
#'
#' @param board The name of the board to register.
#' @param name An optional name to identify this board, defaults to the board name.
#' @param cache The local folder to use as a cache, defaults to `board_cache_path()`.
#' @param versions Should this board be registered with support for versions?
#' @param ... Additional parameters required to initialize a particular board.
#'
#' @details
#'
#' A board requires a local cache to avoid downloading files multiple times. It is
#' recommended to not specify the `cache` parameter since it defaults to a well
#' known `rappdirs`. However, you are welcome to specify any other
#' location for this cache or even a temp folder with `tempfile()`. Notice that,
#' when using a temp folder, pins will be cleared when your R session restarts. The
#' cache parameter can be also set with the `pins.path` option.
#'
#' If `versions` is set to `NULL` (the default), it will fall back on the
#' board-type-specific default. For instance, local boards do not use versions by default,
#' but GitHub boards do.
#'
#' @examples
#' # create a new local board
#' board_register("local", "other_board", cache = tempfile())
#'
#' # create a Website board
#' board_register("datatxt",
#'                name = "txtexample",
#'                url = "https://datatxt.org/data.txt",
#'                cache = tempfile())
#'
#' @seealso [board_register_local()], [board_register_github()],
#'   [board_register_kaggle()], [board_register_rsconnect()] and
#'   [board_register_datatxt()].
#'
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
    if (!is.function(parent_func) || !is.call(parent_call)) break;

    this_parent_call <- tryCatch(match.call(definition = parent_func, call = parent_call), error = function(e) NULL)

    if (is.null(this_parent_call)) break;
    if (length(this_parent_call) < 1) break;

    this_function_name <- deparse(this_parent_call[[1]])

    if (!grepl("(^|::)board_register", this_function_name)) break;

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

#' Deregister Board
#'
#' Deregisters a board, useful to disable boards no longer in use.
#'
#' @param name An optional name to identify this board, defaults to the board name.
#' @param ... Additional parameters required to deregister a particular board.
#'
#' @examples
#'
#' # create a new local board
#' board_register("local", "other_board", cache = tempfile())
#'
#' # pin iris to new board
#' pin(iris, board = "other_board")
#'
#' # deregister new board
#' board_deregister("other_board")
#'
#' @export
board_deregister <- function(name, ...) {
  if (!name %in% board_registry_list()) stop("Board '", name, "' is not registered.")

  board <- board_get(name)

  if (!identical(list(...)$disconnect, FALSE)) board_disconnect(name)
  board_registry_set(name, NULL)

  invisible(NULL)
}

#' Default Board
#'
#' Retrieves the default board, which defaults to `"local"` but can also be
#' configured with the `pins.board` option.
#'
#' @examples
#'
#' library(pins)
#'
#' # create temp board
#' board_register_local("temp", cache = tempfile())
#'
#' # configure default board
#' options(pins.board = "temp")
#'
#' # retrieve default board
#' board_default()
#'
#' # revert default board
#' options(pins.board = NULL)
#' @export
board_default <- function() {
  board_registry_get(getOption("pins.board", "local"))
}
