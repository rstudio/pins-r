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
#' @param versioned Should this board be registered with support for versions?
#' @param ... Additional parameters required to initialize a particular board.
#' @keywords internal
#' @export
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
format.pins_board <- function(x, ...) {
  first_class <- class(x)[[1]]
  desc <- board_desc(x)
  cli_format_method({
    cli_text("Pin board {.cls {first_class}}")
    if (length(desc) > 0) {
      cli_text("{desc}")
    }
    cli_text("Cache size: {format(cache_size(x))}")
  })
}

#' @export
print.pins_board <- function(x, ...) {
  cat(format(x), sep = "\n")
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

#' Retrieve default cache path
#'
#' Retrieves the default path used to cache boards and pins. Makes
#' use of [rappdirs::user_cache_dir()] for cache folders defined by each OS.
#' Remember that you can set the cache location for an individual board object
#' via the `cache` argument.
#'
#' @param name Board name
#' @details
#' There are several environment variables available to control the location of
#' the default pins cache:
#'
#' - Use `PINS_CACHE_DIR` to set the cache path for _only_ pins functions
#' - Use `R_USER_CACHE_DIR` to set the cache path for all functions that use rappdirs
#'
#' On system like AWS Lambda that is read only (for example, only `/tmp` is
#' writeable), set either of these to [base::tempdir()]. You may also need to
#' set environment variables like `HOME` and/or `R_USER_DATA_DIR` to the
#' session temporary directory.
#'
#' @examples
#' # retrieve default cache path
#' board_cache_path("local")
#'
#' # set with env vars:
#' withr::with_envvar(
#'   c("PINS_CACHE_DIR" = "/path/to/cache"),
#'   board_cache_path("local")
#' )
#' withr::with_envvar(
#'   c("R_USER_CACHE_DIR" = "/path/to/cache"),
#'   board_cache_path("local")
#' )
#'
#' @export
board_cache_path <- function(name) {
  # R_CONFIG_ACTIVE suggests we're in a production environment
  if (has_envvars("R_CONFIG_ACTIVE") || has_envvars("PINS_USE_CACHE")) {
    path <- fs::dir_create(tempdir(), "pins")
  } else if (has_envvars("PINS_CACHE_DIR") ) {
    path <- Sys.getenv("PINS_CACHE_DIR")
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
#' board <- board_connect()
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

#' Write board manifest file to board's root directory
#'
#' @description
#' A board manifest file records all the pins, along with their
#' versions, stored on a board.
#' This can be useful for a board built using, for example,
#' [board_folder()] or [board_s3()], then served as a website,
#' such that others can consume using [board_url()].
#' The manifest file is _not_ versioned like a pin is, and this function
#' will overwrite any existing `_pins.yaml` file on your board. It is
#' your responsibility as the user to keep the manifest up to date.
#'
#' Some examples are provided in `vignette("using-board-url")`.
#'
#' @details This function is not supported for read-only boards.
#' It is called for the side-effect of writing a manifest file,
#' `_pins.yaml`, to the root directory of the  `board`. (This will
#' not work in the unlikely event that you attempt to create a pin
#' called `"_pins.yaml"`.)
#'
#' The behavior of the legacy API (for example, [pin_find()]) is unspecified
#' once you have written a board manifest file to a board's root directory.
#' We recommend you only use `write_board_manifest()` with modern boards.
#'
#' @param board A pin board that is *not* read-only.
#' @inheritParams pin_read
#'
#' @return The board, invisibly
#' @export
#'
#' @examples
#' board <- board_temp()
#' pin_write(board, mtcars, "mtcars-csv", type = "csv")
#' pin_write(board, mtcars, "mtcars-json", type = "json")
#'
#' write_board_manifest(board)
#'
#' # see the manifest's format:
#' fs::path(board$path, "_pins.yaml") %>% readLines() %>% cat(sep = "\n")
#'
#' # if you write another pin, the manifest file is out of date:
#' pin_write(board, 1:10, "nice-numbers", type = "json")
#'
#' # you decide when to update the manifest:
#' write_board_manifest(board)
#'
write_board_manifest <- function(board, ...) {
  manifest <- make_manifest(board)
  write_board_manifest_yaml(board, manifest, ...)
  pins_inform("Manifest file written to root folder of board, as `{manifest_pin_yaml_filename}`")
  invisible(board)
}

manifest_pin_yaml_filename <- "_pins.yaml"

make_manifest <- function(board) {
  # given board, return named list:
  #   - names are pin names
  #   - values are relative paths to version directories

  pin_names <- pin_list(board)

  result <- map(
    pin_names,
    ~fs::path(.x, pin_versions(board, name = .x)$version) %>%
      end_with_slash() %>% # versions usually don't include slash
      as.list()
  )
  names(result) <- pin_names

  result
}

#' Write a manifest YAML file for a board
#'
#' This is a low-level function that powers [write_board_manifest()]. It is
#' needed primarily for folks developing new board types, and should not
#' generally be called directly.
#'
#' @return `write_board_manifest_yaml()` is called for its side-effect of
#' writing a manifest YAML file.
#' @export
#' @keywords internal
#' @inheritParams write_board_manifest
#' @param manifest Contents to be written to the manifest file, as a list.
#'
write_board_manifest_yaml <- function(board, manifest, ...) {
  ellipsis::check_dots_used()
  UseMethod("write_board_manifest_yaml")
}

#' @export
write_board_manifest_yaml.default <- function(board, manifest, ...) {
  abort(glue("Manifest not supported for {class(board)[[1]]}"))
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

