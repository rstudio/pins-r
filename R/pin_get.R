#' Retrieve a pin (legacy API)
#'
#' `r lifecycle::badge('deprecated')`
#'
#' Retrieves a pin by name from the local or given board.
#'
#' @param name The name of the pin.
#' @param board The board where this pin will be retrieved from.
#' @param cache Should the pin cache be used? Defaults to `TRUE`.
#' @param extract Should compressed files be extracted? Each board defines the
#'   default behavior.
#' @param version The version of the dataset to retrieve, defaults to latest one.
#' @param files Should only the file names be returned?
#' @param signature Optional signature to validate this pin, use `pin_info()`
#'   to compute signature.
#' @param ... Additional parameters.
#'
#' @details
#'
#' `pin_get()` retrieves a pin by name and, by default, from the local board.
#' You can use the `board` parameter to specify which board to retrieve a pin from.
#' If a board is not specified, it will use `pin_find()` to find the pin across
#' all boards and retrieve the one that matches by name.
#'
#' @examplesIf rlang::is_installed("filelock")
#' # define temporary board
#' board <- legacy_temp()
#' pin(mtcars, board = board)
#'
#' # retrieve the mtcars pin
#' pin_get("mtcars", board = board)
#' @export
#' @keywords internal
pin_get <- function(name,
                    board = NULL,
                    cache = TRUE,
                    extract = NULL,
                    version = NULL,
                    files = FALSE,
                    signature = NULL,
                    ...) {
  lifecycle::deprecate_soft("1.4.0", "pin_get()", "pin_read()")
  board <- board_get(board)

  if (!cache) {
    pin_register_reset_cache(board, name)
  }
  result <- board_pin_get(board, name, extract = extract, version = version, ...)

  meta <- read_meta(result)
  meta$type <- meta$type %||% "files"

  # Why doesn't this use meta$files? Maybe because of zips?
  result_files <- result[!grepl(paste0("^", pin_versions_path_name()), result)]
  result_files <- dir(result_files, full.names = TRUE)
  if (meta$type == "files" && length(result_files) > 1) {
    result_files <- result_files[!grepl("/data.txt$", result_files)]
  }

  if (!is.null(signature)) {
    pin_signature <- pin_version_signature(result_files)
    if (!identical(signature, pin_signature)) {
      stop("Pin signature '", pin_signature, "' does not match given signature.")
    }
  }

  if (files) {
    result_files
  } else {
    pin_load(structure(result, class = meta$type))
  }
}
