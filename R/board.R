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
new_board <- function(board, name, api, cache = NULL, versions = FALSE, ...) {
  cache <- cache %||% board_cache_path(name)
  if (!is.na(cache)) {
    fs::dir_create(cache)
  }

  board <- structure(
    list(
      board = board,
      api = api,
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

  if (1 %in% x$api) {
    pins <- pin_list(x)
  } else {
    pins <- pin_find(board = x)$name
  }

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

