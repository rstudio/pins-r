#' Retrieve pin metadata (legacy API)
#'
#' `r lifecycle::badge('deprecated')`
#'
#' Retrieve metadata for pins in legacy boards.
#'
#' @param name The exact name of the pin to match when searching.
#' @param board The board name used to find the pin.
#' @param extended Should additional board-specific information be shown?
#' @param metadata Should additional pin-specific information be shown?
#' @param signature Should a signature to identify this pin be shown?
#' @param ... Additional parameters.
#'
#' @examplesIf rlang::is_installed("filelock")
#' # old API
#' board_register_local(cache = tempfile())
#' pin(mtcars)
#' pin_info("mtcars", "local")
#'
#' # new API
#' board <- board_temp()
#' board %>% pin_write(mtcars)
#' board %>% pin_meta("mtcars")
#' @export
#' @keywords internal
pin_info <- function(name,
                     board = NULL,
                     extended = TRUE,
                     metadata = TRUE,
                     signature = FALSE,
                     ...) {
  lifecycle::deprecate_soft("1.4.0", "pin_info()", "pin_meta()")

  if (is.board(board) && !0 %in% board$api) {
    this_not_that("pin_meta", "pin_info")
  }

  entry <- pin_find(
    name = name,
    board = board,
    extended = extended,
    metadata = metadata
  )
  if (nrow(entry) == 0) {
    abort(paste0("Pin '", name, "' was not found."))
  }
  if (nrow(entry) > 1) {
    boards <- paste0(entry$board, collapse = ",")
    abort(paste0("Pin '", name, "' was found in multiple boards: ", boards))
  }
  if (is.null(board)) {
    board <- board_get(entry$board)
  }

  metadata <- list()
  if ("metadata" %in% colnames(entry) && nchar(entry$metadata) > 0) {
    metadata <- jsonlite::fromJSON(entry$metadata, simplifyDataFrame = FALSE)
  }

  if (signature) {
    files <- pin_get(name, board = board, files = TRUE)
    entry[["signature"]] <- pin_version_signature(files)
  }

  entry_ext <- as.list(entry)
  entry_ext$metadata <- NULL
  entry_ext <- Filter(function(e) !is.list(e) || length(e) != 1 || !is.list(e[[1]]) || length(e[[1]]) > 0, entry_ext)
  for (name in names(metadata)) {
    entry_ext[[name]] <- metadata[[name]]
  }

  structure(entry_ext, class = "pin_info")
}

#' @keywords internal
#' @export
print.pin_info <- function(x, ...) {
  info <- x

  cat(cli::col_silver(paste0("# Source: ", info$board, "<", info$name, "> [", info$type, "]\n")))
  if (nchar(info$description) > 0) cat(cli::col_silver(paste0("# Description: ", info$description, "\n")))
  if (!is.null(info$signature)) cat(cli::col_silver(paste0("# Signature: ", info$signature, "\n")))

  info$board <- info$name <- info$type <- info$description <- info$signature <- NULL

  if (length(names(info)) > 0) {
    cat(cli::col_silver(paste0("# Properties:", "\n")))

    for (i in names(info)) {
      entry <- info[[i]]
      if ((is.list(entry) && length(entry) == 0) ||
        (is.character(entry) && identical(nchar(entry), 0L)) ||
        identical(i, "path")) {
        info[[i]] <- NULL
      }
    }

    yaml_str <- yaml::as.yaml(info) %>%
      strsplit("\n") %>%
      sapply(function(e) paste("#  ", e)) %>%
      paste0(collapse = "\n")
    cat(cli::col_silver(yaml_str))
  }
}
