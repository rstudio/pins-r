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

#' @export
print.pins_board <- function(x, ...) {
  cat(paste0(crayon::bold("Pin board"), " <", class(x)[[1]], ">\n"))
  cat(paste0(board_desc(x), "\n", collapse = ""))
  pins <- pin_find(board = x)$name

  n <- length(pins)
  if (n > 20) {
    pins <- c(pins[1:19], "...")
  }
  contents <- paste0(
    "With ", n, " pins: ",
    paste0("'", pins, "'", collapse = ", ")
  )

  cat(strwrap(contents, exdent = 2), sep = "\n")

  invisible()
}

is.board <- function(x) inherits(x, "pins_board")

#' Custom Boards
#'
#' Family of functions meant to be used to implement custom boards extensions,
#' not to be used by users.
#'
#' @param board The board to extend, retrieved with `board_get()`.
#' @param path The path to store as a pin.
#' @param name The name of the pin.
#' @param metadata A list of metadata associated with this pin.
#' @param text The text patteren to find a pin.
#' @param ... Additional parameteres.
#'
#' @rdname custom-boards
#' @export
board_pin_create <- function(board, path, name, metadata, ...) {
  UseMethod("board_pin_create")
}

#' @export
#' @rdname custom-boards
board_initialize <- function(board, ...) {
  stop("`board_initialize()` is no longer used", call. = FALSE)
}

#' @export
#' @rdname custom-boards
board_browse <- function(board, ...) {
  UseMethod("board_browse")
}

#' @export
#' @rdname custom-boards
board_desc <- function(board, ...) {
  UseMethod("board_desc")
}
#' @export
board_desc.default <- function(board, ...) {
  character()
}

#' @export
#' @rdname custom-boards
board_pin_get <- function(board, name, ...) {
  UseMethod("board_pin_get")
}

#' @export
#' @rdname custom-boards
board_pin_remove <- function(board, name, ...) {
  UseMethod("board_pin_remove")
}

#' @export
#' @rdname custom-boards
board_pin_find <- function(board, text, ...) {
  UseMethod("board_pin_find")
}

#' @export
#' @rdname custom-boards
board_pin_versions <- function(board, name, ...) {
  UseMethod("board_pin_versions")
}
#' @export
board_pin_versions.default <- function(board, name, ...) {
  data.frame(version = character(0), stringsAsFactors = FALSE)
}

#' Custom Boards Utilities
#'
#' A set of utilities used when implementing custom boards.
#'
#' @export
#' @rdname custom-boards-utils
#' @keywords internal
board_local_storage <- function(...) {
  stop("board_local_storage() is deprecated", call. = FALSE)
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

#' Retrieve Default Cache Path
#'
#' Retrieves the default path used to cache boards and pins. Makes
#' use of the `rappdirs` package to use cache folders
#' defined by each OS.
#'
#' @examples
#' # retrieve default cache path
#' board_cache_path()
#' @export
board_cache_path <- function() {
  # if a configuration is present this could mean we are running in a production environment without user caches
  if (nchar(Sys.getenv("R_CONFIG_ACTIVE")) > 0 && nchar(Sys.getenv("PINS_USE_CACHE")) == 0) {
    tempfile()
  } else {
    getOption("pins.path", rappdirs::user_cache_dir("pins"))
  }
}

# helpers -----------------------------------------------------------------

board_empty_results <- function() {
  data.frame(name = c(), description = c(), rows = c(), cols = c(), class = c())
}

