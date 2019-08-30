new_board <- function(board, name, cache, ...) {

  if (is.null(cache)) stop("Please specify the 'cache' parameter, usually set to '~/.pins'.")

  board <- structure(list(
      board = board,
      name = name,
      cache = cache
    ),
    class = board)

  board <- board_initialize(board, ...)

  board
}

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
  if (is.null(name)) name <- board_default()

  if (!name %in% board_list())
    stop("Board '", name, "' not a board, available boards: ", paste(board_list(), collapse = ", "))

  board_registry_get(name)
}

#' Register Board
#'
#' Registers a board, useful to find resources with \code{pin_find()} or pin to
#' additional boards with \code{pin()}.
#'
#' @param board The name of the board to register.
#' @param name An optional name to identify this board, defaults to the board name.
#' @param cache The local folder to use as a cache, defaults to \code{"~/.pins"}.
#' @param ... Additional parameters required to initialize a particular board.
#'
#' @details
#'
#' A board requires a local cache to avoid downloading files multiple times. It is
#' recommended to not specify the \code{cache} parameter since it defaults to a well
#' known \code{"~/.pins"} location. However, you are welcome to specify any other
#' location for this cache or even a temp folder with \code{tempfile()}. Notice that,
#' when using a temp folder, pins will be cleared when your R session restarts. The
#' cache parameter can be also set with the \code{pins.path} option.
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
#' @seealso \code{\link{board_register_local}}, \code{\link{board_register_github}},
#'   \code{\link{board_register_kaggle}}, \code{\link{board_register_rsconnect}} and
#'   \code{\link{board_register_datatxt}}.
#'
#' @export
board_register <- function(board, name = board, cache = "~/.pins", ...) {
  params <- list(...)
  board <- new_board(board, name, cache = cache, ...)

  board_registry_set(name, board)

  register_call <- board_register_code(board$name, name)

  if (!identical(params$connect, FALSE)) board_connect(name, register_call)

  invisible(name)
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
#' board_register("local", "other_board")
#'
#' # pin iris to new board
#' pin(iris, board = "other_board")
#'
#' # deregister new board
#' board_deregister("other_board")
#'
#' @export
board_deregister <- function(name, ...) {
  if (!name %in% board_list()) stop("Board '", name, "' is not registered.")

  board <- board_get(name)

  if (!identical(list(...)$disconnect, FALSE)) board_disconnect(name)
  board_registry_set(name, NULL)

  invisible(NULL)
}

#' Default Board
#'
#' Retrieves the default board, which defaults to \code{"temp"} but can also be
#' configured with the \code{pins.board} option.
#'
#' @examples
#'
#' # configure default board
#' options(pind.board = "temp")
#'
#' # retrieve default board
#' board_default()
#'
#' @export
board_default <- function() {
  getOption("pins.board", "temp")
}
