
#' Pin Info
#'
#' Retrieve information for a given pin.
#'
#' @param name The exact name of the pin to match when searching.
#' @param board The board name used to find the pin.
#' @param extended Should additional board-specific information be shown?
#' @param metadata Should additional pin-specific information be shown?
#' @param signature Should a signature to identify this pin be shown?
#' @param ... Additional parameters.
#'
#' @examples
#' # define board and cache a dataset
#' board <- board_local(tempfile())
#' pin(mtcars, board = board)
#'
#' # Get info
#' pin_info("mtcars", board = board)
#' @export
pin_info <- function(name,
                     board = NULL,
                     extended = TRUE,
                     metadata = TRUE,
                     signature = FALSE,
                     ...) {
  entry <- pin_get_one(name, board, extended, metadata)

  board <- entry$board

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

pin_get_one <- function(name, board, extended, metadata) {
  # first ensure there is always one pin since metadata with multiple entries can fail
  entry <- pin_find(name = name, board = board, metadata = FALSE, extended = FALSE)

  if (nrow(entry) == 0) stop("Pin '", name, "' was not found.")
  if (nrow(entry) > 1) stop("Pin '", name, "' was found in multiple boards: ", paste(entry$board, collapse = ","), ".")

  board <- entry$board
  entry <- pin_find(name = name, board = board, metadata = metadata, extended = extended)

  entry
}

#' @keywords internal
#' @export
print.pin_info <- function(x, ...) {
  info <- x

  cat(crayon::silver(paste0("# Source: ", info$board, "<", info$name, "> [", info$type, "]\n")))
  if (nchar(info$description) > 0) cat(crayon::silver(paste0("# Description: ", info$description, "\n")))
  if (!is.null(info$signature)) cat(crayon::silver(paste0("# Signature: ", info$signature, "\n")))

  info$board <- info$name <- info$type <- info$description <- info$signature <- NULL

  if (length(names(info)) > 0) {
    cat(crayon::silver(paste0("# Properties:", "\n")))

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
    cat(crayon::silver(yaml_str))
  }
}
