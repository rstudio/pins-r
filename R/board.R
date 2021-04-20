#' Create a new board
#'
#' @param board The name of the board to register.
#' @param name An optional name used identify the board. This is no longer
#'   generally needed since you should be passing around an explicit
#'   board object.
#' @param cache Cache path. Every board requires a local cache to avoid
#'   downloading files multiple times. The default stores in a standard
#'   cache location for your operating system, but you can override if needed.
#' @param versions Should this board be registered with support for versions?
#' @param ... Additional parameters required to initialize a particular board.
#' @keywords internal
new_board <- function(board, name, cache = NULL, versions = FALSE, ...) {
  cache <- cache %||% board_cache_path(name)
  fs::dir_create(cache)

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
  cat(paste0(cli::style_bold("Pin board"), " <", class(x)[[1]], ">\n"))
  cat(paste0(board_desc(x), "\n", collapse = ""))
  pins <- pin_find(board = x)$name

  n <- length(pins)
  if (n == 0) {
    contents <- "With no pins."
  } else {
    if (n > 20) {
      pins <- c(pins[1:19], "...")
    }
    contents <- paste0(
      "With ", n, " pins: ",
      paste0("'", pins, "'", collapse = ", ")
    )
  }


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
#' @keywords internal
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
  wibble(version = character(0))
}

#' @export
#' @rdname custom-boards
board_pin_download <- function(board, name, ...) {
  UseMethod("board_pin_download")
}
#' @export
#' @rdname custom-boards
board_pin_upload <- function(board, name, path, metadata, versioned = NULL, x = NULL, ...) {
  UseMethod("board_pin_upload")
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

#' Retrieve Default Cache Path
#'
#' Retrieves the default path used to cache boards and pins. Makes
#' use of the `rappdirs` package to use cache folders
#' defined by each OS.
#'
#' @param name Board name
#' @keywords internal
#' @examples
#' # retrieve default cache path
#' board_cache_path("local")
#' @export
board_cache_path <- function(name) {
  # R_CONFIG_ACTIVE suggests we're in a production environment
  if (has_envvars("R_CONFIG_ACTIVE") || has_envvars("PINS_USE_CACHE")) {
    path <- tempfile()
  } else {
    path <- rappdirs::user_cache_dir("pins")
  }
  fs::path(path, name)
}

# helpers -----------------------------------------------------------------

board_empty_results <- function() {
  data.frame(
    name = character(),
    description = character(),
    rows = character(),
    cols = character(),
    class = character()
  )
}

