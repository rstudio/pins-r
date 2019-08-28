new_board <- function(board, name, ...) {
  board <- structure(list(
      board = board,
      name = name
    ),
    class = board)

  board <- board_initialize(board, ...)

  board
}

#' Connect to Board
#'
#' Connects to a board to activate RStudio's connection pane, when available.
#'
#' @param name The name of the board to activate.
#' @param ... Additional parameters required to initialize a particular board.
#'
#' @examples
#'
#' # define the storage location for all boards
#' options(pins.path = tempdir())
#'
#' @keywords internal
#' @export
board_connect <- function(name, ...) {
  board <- board_get(name)

  ui_viewer_register(board)

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
board_get <- function(name = NULL) {
  if (is.null(name)) name <- getOption("pins.board", "local")

  if (!name %in% board_list())
    stop("Board '", name, "' not a board, available boards: ", paste(board_list(), collapse = ", "))

  board_registry_get(name)
}

#' Register Board
#'
#' Registers a board, useful to add sources to \code{pin_find()} or pin to remote
#' boards with \code{pin()}.
#'
#' @param board The name of the board to register.
#' @param name An optional name to identify this board, defaults to the board name.
#' @param ... Additional parameters required to initialize a particular board.
#'
#' @examples
#' # create a new local board
#' board_register("local", "other_board")
#'
#' # create a Website board
#' board_register("datatxt", name = "txtexample", url = "https://datatxt.org/data.txt")
#'
#' @seealso \code{\link{board_register_github}}, \code{\link{board_register_kaggle}},
#'   \code{\link{board_register_rsconnect}} and \code{\link{board_register_datatxt}}.
#'
#' @export
board_register <- function(board, name = board, ...) {
  params <- list(...)
  board <- new_board(board, name, ...)

  board_registry_set(name, board)

  if (!identical(params$connect, FALSE)) board_connect(name)

  invisible(board)
}

#' Deregister Board
#'
#' Deregisters a board, useful to disable boards no longer in use. This operation
#' removes all locally cached pins.
#'
#' @param name An optional name to identify this board, defaults to the board name.
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
board_deregister <- function(name) {
  if (!name %in% board_list()) stop("Board '", name, "' is not registered.")

  board <- board_get(name)
  storage <- board_local_storage(board$name)

  board_disconnect(name)
  board_registry_set(name, NULL)
  unlink(storage, recursive = TRUE)

  invisible(NULL)
}
