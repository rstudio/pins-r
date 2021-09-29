#' Create a new board
#'
#' @param board The name of the board to register.
#' @param name An optional name used identify the board. This is no longer
#'   generally needed since you should be passing around an explicit
#'   board object.
#' @param cache Cache path. Every board requires a local cache to avoid
#'   downloading files multiple times. The default stores in a standard
#'   cache location for your operating system, but you can override if needed.
#' @param versions,versioned Should this board be registered with support for versions?
#' @param ... Additional parameters required to initialize a particular board.
#' @keywords internal
new_board <- function(board, api, cache, ...) {
  if (!is.na(cache)) {
    fs::dir_create(cache)
  }

  board <- structure(
    list(
      board = board,
      api = api,
      cache = cache,
      ...
    ),
    class = c(board, "pins_board")
  )

  board
}

#' @rdname new_board
new_board_v0 <- function(board, name, cache = NULL, versions = FALSE, ...) {
  cache <- cache %||% board_cache_path(name)

  new_board(
    board = board,
    api = 0L,
    name = name,
    cache = cache,
    versions = versions,
    ...
  )
}

#' @rdname new_board
new_board_v1 <- function(board, cache, versioned = FALSE, ...) {
  new_board(
    board = board,
    api = 1L,
    cache = cache,
    versioned = versioned,
    ...
  )
}


#' @export
print.pins_board <- function(x, ...) {
  cat(paste0(cli::style_bold("Pin board"), " <", class(x)[[1]], ">\n"))

  desc <- board_desc(x)
  if (length(desc) > 0) {
    cat(paste0(desc, "\n", collapse = ""))
  }
  cat("Cache size: ", format(cache_size(x)), "\n", sep = "")

  if (1 %in% x$api) {
    pins <- pin_list(x)
  } else {
    pins <- pin_find(board = x)$name
  }

  # Some boards (e.g. kaggle_competitions have an infeasibly large number
  # and there's no point in listing them all)
  if (!identical(pins, NA)) {
    n <- length(pins)
    if (n > 0) {
      if (n > 20) {
        pins <- c(pins[1:19], "...")
      }
      contents <- paste0(
        "Pins [", n, "]: ",
        paste0("'", pins, "'", collapse = ", ")
      )
      cat(strwrap(contents, exdent = 2), sep = "\n")
    }
  }

  invisible(x)
}

cache_size <- function(board) {
  if (is.na(board$cache)) {
    0
  } else {
    dir_size(board$cache)
  }
}

is.board <- function(x) inherits(x, "pins_board")

#' Local storage path
#'
#' Deprecated: please use [board_cache_path()] instead.
#'
#' @export
#' @rdname custom-boards-utils
#' @keywords internal
board_local_storage <- function(...) {
  lifecycle::deprecate_stop("1.0.0", "board_local_storage()", "board_cache_path()")
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
    path <- cache_dir()
  }
  fs::path(path, name)
}

#' Deparse a board object
#'
#' Returns the R code that would recreate the board when re-run on another
#' computer. Goal is to capture the location of the board, but not the
#' authorisation, since (a) that would leak credentials and (b) in
#' most deployment scenarios board auth will be read from env vars.
#'
#' @returns A call.
#' @keywords internal
#' @examples
#' \dontrun{
#' board <- board_rsconnect()
#' # Generate code to access this board from elsewhere
#' board_deparse(board)
#' }
#' @export
#' @inheritParams pin_read
board_deparse <- function(board, ...) {
  ellipsis::check_dots_used()
  UseMethod("board_deparse")
}

#' @export
board_deparse.pins_board <- function(board, ...) {
  abort("This board doesn't support deparsing")
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

