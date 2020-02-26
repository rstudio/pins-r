new_board <- function(board, name, cache, versions, ...) {

  if (is.null(cache)) stop("Please specify the 'cache' parameter.")

  board <- structure(list(
      board = board,
      name = name,
      cache = cache,
      versions = versions
    ),
    class = board)

  board <- board_initialize(board, cache = cache, ...)

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
  defaults <- c("local", "packages", board_default())
  unique(c(board_registry_list(), defaults))
}

board_infer <- function(x, name = NULL, board = NULL, register_call = NULL, connect = NULL, url = NULL) {
  inferred <- list(
    name = name,
    board = if (is.null(board)) name else board,
    connect = if (is.null(connect)) !identical(name, "packages") else connect,
    url = url,
    register_call = register_call
  )

  # if boards starts with http:// or https:// assume this is a website board
  if (grepl("^http://|^https://", x)) {
    inferred$url <- x
    inferred$board <- "datatxt"

    # use only subdomain as friendly name which is also used as cache folder
    if (is.null(name) || identical(x, name)) {
      inferred$name <- gsub("https?://|\\..*", "", inferred$url)
    }

    inferred$register_call <- paste0("pins::board_register(board = \"datatxt\", name = \"",
                                     inferred$name,
                                     "\", url = \"",
                                     inferred$url,
                                     "\")")
  }

  if (is.null(inferred$name)) inferred$name <- x
  if (is.null(inferred$board)) inferred$board <- x

  inferred
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

  register_call <- paste0("pins::board_register(board = \"", name, "\")")

  if (!name %in% board_registry_list()) {
    board_inferred <- board_infer(name)

    if (!is.null(board_inferred$register_call)) {
      register_call <- board_inferred$register_call
    }

    # attempt to automatically register board
    name <- board_inferred$name
    tryCatch(board_register(board_inferred$board,
                            name = board_inferred$name,
                            connect = board_inferred$connect,
                            register_call = register_call,
                            url = board_inferred$url),
             error = function(e) NULL)

    if (!name %in% board_registry_list()) {
      stop("Board '", name, "' not a board, available boards: ", paste(board_list(), collapse = ", "))
    }
  }

  board_registry_get(name)
}

#' Register Board
#'
#' Registers a board, useful to find resources with \code{pin_find()} or pin to
#' additional boards with \code{pin()}.
#'
#' @param board The name of the board to register.
#' @param name An optional name to identify this board, defaults to the board name.
#' @param cache The local folder to use as a cache, defaults to \code{board_cache_path()}.
#' @param versions Should this board be registered with support for versions?
#' @param ... Additional parameters required to initialize a particular board.
#'
#' @details
#'
#' A board requires a local cache to avoid downloading files multiple times. It is
#' recommended to not specify the \code{cache} parameter since it defaults to a well
#' known \code{rappdirs}. However, you are welcome to specify any other
#' location for this cache or even a temp folder with \code{tempfile()}. Notice that,
#' when using a temp folder, pins will be cleared when your R session restarts. The
#' cache parameter can be also set with the \code{pins.path} option.
#'
#' The \code{versions} parameter default to different values depending on which board
#' is choosed. For instance, local boards do not use versions by default, but GitHub
#' boards do.
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
board_register <- function(board,
                           name = board,
                           cache = board_cache_path(),
                           versions = NULL,
                           ...) {
  params <- list(...)

  inferred <- board_infer(board,
                          board = board,
                          name = name,
                          register_call = params$register_call,
                          connect = params$connect,
                          url = params$url)
  params$url <- NULL

  new_params <- c(
    list(inferred$board, inferred$name, cache = cache, versions = versions),
    params,
    url = inferred$url
  )

  board <- do.call("new_board", new_params)

  board_registry_set(inferred$name, board)

  if (is.null(inferred$register_call)) inferred$register_call <- board_register_code(board$name, inferred$name)

  if (!identical(inferred$connect, FALSE)) board_connect(board$name, inferred$register_call)

  invisible(inferred$name)
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
#' Retrieves the default board, which defaults to \code{"local"} but can also be
#' configured with the \code{pins.board} option.
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
#' @export
board_default <- function() {
  getOption("pins.board", "local")
}
