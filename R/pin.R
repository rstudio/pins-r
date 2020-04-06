#' Create Pin Name
#'
#' Creates a pin name from an character expression generated with \code{deparse(substitute(x))}.
#'
#' @param x The expression to generate  the pin name from.
#' @param board The board to which this name is generating for.
#'
#' @export
#' @keywords internal
pin_default_name <- function(x, board) {
  name <- basename(x)

  error <- "Can't auto-generate pin name from object, please specify the 'name' parameter."
  if (length(name) != 1) stop(error)

  sanitized <- gsub("[^a-zA-Z0-9-]", "-", name)
  sanitized <- gsub("^-*|-*$", "", sanitized)
  sanitized <- gsub("-+", "-", sanitized)

  if (nchar(sanitized) == 0) stop(error)

  # kaggle boards require five or more character names
  if (identical(board, "kaggle") && nchar(sanitized) < 5) sanitized <- paste(sanitized, "pin", sep = "-")

  sanitized
}

#' Pin Resource
#'
#' Pins the given resource locally or to the given board.
#'
#' @param x An object, local file or remote URL to pin.
#' @param name The name for the dataset or object.
#' @param description Optional description for this pin.
#' @param board The board where this pin will be placed.
#' @param ... Additional parameters.
#'
#' @details
#'
#' \code{pin()} allows you to cache remote resources and intermediate results with ease. When
#' caching remote resources, usually URLs, it will check for HTTP caching headers to avoid
#' re-downloading when the remote result has not changed.
#'
#' This makes it ideal to support reproducible research by requiring manual instruction to
#' download resources before running your R script.
#'
#' In addition, \code{pin()} still works when working offline or when the remote resource
#' becomes unavailable; when this happens, a warning will be triggered but your code will
#' continue to work.
#'
#' @examples
#' library(pins)
#'
#' # define local board
#' board_register_local(cache = tempfile())
#'
#' # cache the mtcars dataset
#' pin(mtcars)
#'
#' # cache computation over mtcars
#' mtcars[mtcars$mpg > 30,] %>%
#'   pin(name = "mtefficient")
#'
#' # retrieve cached pin
#' pin_get("mtefficient")
#'
#' # url to remote resource
#' resource <- file.path("https://raw.githubusercontent.com/facebook/prophet",
#'                       "master/examples/example_retail_sales.csv")
#'
#' # cache remote resource
#' pin(resource, name = "example_retail_sales")
#'
#' # load cached csv
#' pin_get("example_retail_sales") %>% read.csv()
#'
#' # cache and read csv
#' read.csv(pin(resource))
#'
#' @export
pin <- function(x, name = NULL, description = NULL, board = NULL, ...) {
  UseMethod("pin")
}

#' Retrieve Pin
#'
#' Retrieves a pin by name from the local or given board.
#'
#' @param name The name of the pin.
#' @param board The board where this pin will be retrieved from.
#' @param cache Should the pin cache be used? Defaults to \code{TRUE}.
#' @param extract Should compressed files be extracted? Each board defines the
#'   default behavior.
#' @param version The version of the dataset to retrieve, defaults to latest one.
#' @param files Should only the file names be returned?
#' @param signature Optional signature to validate this pin, use \code{pin_info()}
#'   to compute signature.
#' @param ... Additional parameters.
#'
#' @details
#'
#' \code{pin_get()} retrieves a pin by name and, by default, from the local board.
#' You can use the \code{board} parameter to specify which board to retrieve a pin from.
#' If a board is not specified, it will use \code{pin_find()} to find the pin across
#' all boards and retrieve the one that matches by name.
#'
#' @examples
#'
#' library(pins)
#'
#' # define local board
#' board_register_local(cache = tempfile())
#'
#' # cache the mtcars dataset
#' pin(mtcars)
#'
#' # retrieve the mtcars pin
#' pin_get("mtcars")
#'
#' # retrieve mtcars pin from packages board
#' pin_get("easyalluvial/mtcars2", board = "packages")
#' @export
pin_get <- function(name,
                    board = NULL,
                    cache = TRUE,
                    extract = NULL,
                    version = NULL,
                    files = FALSE,
                    signature = NULL,
                    ...) {
  if (is.null(board)) {
    board_pin_get_or_null <- function(...) tryCatch(board_pin_get(...), error = function(e) NULL)

    result <- board_pin_get_or_null(board_get(NULL), name, version = version)

    if (is.null(result) && is.null(board)) {
      for (board_name in board_list()) {
        if (!cache) pin_reset_cache(board_name, name)
        result <- board_pin_get_or_null(board_get(board_name), name, extract = extract, version = version)
        if (!is.null(result)) {
          pin_log("Found pin ", name, " in board ", board_name)
          break
        }
      }
    }
    if (is.null(result)) stop("Failed to retrieve '", name, "' pin.")
  }
  else {
    if (!cache) pin_reset_cache(board, name)
    result <- board_pin_get(board_get(board), name, extract = extract, version = version, ...)
  }

  manifest <- pin_manifest_get(result)
  if (is.null(manifest$type)) manifest$type <- "files"

  result_files <- result[!grepl(paste0("^", pin_versions_path_name()), result)]
  result_files <- dir(result_files, full.names = TRUE)
  if (manifest$type == "files" && length(result_files) > 1) result_files <- result_files[!grepl("/data.txt$", result_files)]

  if (!is.null(signature)) {
    pin_signature <- pin_version_signature(result_files)
    if (!identical(signature, pin_signature)) stop("Pin signature '", pin_signature, "' does not match given signature.")
  }

  if (files) {
    result_files
  }
  else {
    pin_load(structure(result, class = manifest$type))
  }
}

#' Remove Pin
#'
#' Unpins the given named pin from the given board.
#'
#' @param name The name for the pin.
#' @param board The board from where this pin will be removed.
#'
#' @details
#'
#' Notice that some boards do not support deleting pins, this is the case
#' for the Kaggle board. For these boards, you would manually have to
#' remote resources using the tools the board provides.
#'
#' @examples
#'
#' library(pins)
#'
#' # define local board
#' board_register_local(cache = tempfile())
#'
#' # create mtcars pin
#' pin(mtcars)
#'
#' # remove mtcars pin
#' pin_remove("mtcars", board = "local")
#' @export
pin_remove <- function(name, board) {
  board <- board_get(board)

  board_pin_remove(board, name)
  ui_viewer_updated(board)

  invisible(NULL)
}

pin_find_empty <- function() {
  data.frame(
    name = character(),
    description = character(),
    type = character(),
    metadata = character(),
    board = character(),
    stringsAsFactors = FALSE)
}

#' Find Pin
#'
#' Find a pin in any board registered using \code{board_register()}.
#'
#' @param text The text to find in the pin description or name.
#' @param board The board name used to find the pin.
#' @param name The exact name of the pin to match when searching.
#' @param extended Should additional board-specific columns be shown?
#' @param ... Additional parameters.
#'
#' @details
#'
#' \code{pin_find()} allows you to discover new resources or retrieve
#' pins you've previously created with \code{pin()}.
#'
#' The \code{pins} package comes with a CRAN packages board which
#' allows searching all CRAN packages; however, you can add additional
#' boards to search from like Kaggle, Github and RStudio Connect.
#'
#' For 'local' and 'packages' boards, the 'text' parameter searches
#' the title and description of a pin using a regular expression. Other
#' boards search in different ways, most of them are just partial matches,
#' please refer to their documentation to understand how other
#' boards search for pins.
#'
#' Once you find a pin, you can retrieve with \code{pin_get("pin-name")}.
#'
#' @examples
#' library(pins)
#'
#' # retrieve pins
#' pin_find()
#'
#' # search pins related to 'cars'
#' pin_find("cars")
#'
#' # search pins related to 'seattle' in the 'packages' board
#' pin_find("seattle", board = "packages")
#'
#' # search pins related to 'london' in the 'packages' board
#' pin_find("london", board = "packages")
#'
#' \donttest{
#' # retrieve 'hpiR/seattle_sales' pin
#' pin_get("hpiR/seattle_sales")
#'
#' # retrieve 'bsamGP/London.Mortality' pin
#' pin_get("bsamGP/London.Mortality")
#' }
#'
#' @export
pin_find <- function(text = NULL,
                     board = NULL,
                     name = NULL,
                     extended = FALSE,
                     ...) {
  if (is.null(board) || nchar(board) == 0) board <- board_list()
  metadata <- identical(list(...)$metadata, TRUE)
  text <- pin_content_name(text)
  if (is.null(text) && !is.null(name)) text <- name

  all_pins <- pin_find_empty()

  for (board_name in board) {
    board_object <- board_get(board_name)

    board_pins <- tryCatch(
      board_pin_find(board = board_object, text, name = name, extended = extended, ...),
      error = function(e) {
        warning("Error searching '", board_name, "' board: ", e$message)
        board_empty_results()
      })

    if (identical(extended, TRUE)) {
      ext_df <- tryCatch(
        paste("[", paste(board_pins$metadata, collapse = ","), "]") %>% jsonlite::fromJSON(),
        error = function(e) NULL)

      if (is.data.frame(ext_df) && nrow(board_pins) == nrow(ext_df)) {
        ext_df <- ext_df[, !names(ext_df) %in% colnames(board_pins)]
        board_pins <- cbind(board_pins, ext_df)
      }
    }

    if (nrow(board_pins) > 0) {
      board_pins$board <- rep(board_name, nrow(board_pins))

      all_pins <- pin_results_merge(all_pins, board_pins, identical(extended, TRUE))
    }
  }

  if (!is.null(text)) {
    find_names <- grepl(text, all_pins$name, ignore.case = TRUE)
    find_description <- if (is.null(all_pins$description)) FALSE else grepl(text, all_pins$description, ignore.case = TRUE)
    all_pins <- all_pins[find_names | find_description,]
  }

  if (!metadata) {
    all_pins <- all_pins[, names(all_pins) != "metadata"]
  }

  if (!is.null(name)) {
    all_pins <- all_pins[grepl(paste0("(.*/)?", name, "$"), all_pins$name),]
    if (nrow(all_pins) > 0) all_pins <- all_pins[1,]
  }

  # sort pin results by name
  all_pins <- all_pins[order(all_pins$name), ]

  format_tibble(all_pins)
}


#' @rdname custom-pins
#' @keywords internal
#' @export
pin_preview <- function(x, board = NULL, ...) {
  UseMethod("pin_preview")
}

#' @rdname custom-pins
#' @keywords internal
#' @export
pin_load <- function(path, ...) {
  UseMethod("pin_load")
}

pin_files <- function(name, board = NULL, ...) {
  entry <- pin_find(name = name, board = board, metadata = TRUE)

  if (nrow(entry) != 1) stop("Pin '", name, "' not found.")
  metadata <- jsonlite::fromJSON(as.list(entry)$metadata)

  metadata$path
}

pin_get_one <- function(name, board, extended, metadata) {
  # first ensure there is always one pin since metadata with multiple entries can fail
  entry <- pin_find(name = name, board = board, metadata = FALSE, extended = FALSE)

  if (nrow(entry) == 0) stop("Pin '", name, "' was not found.")
  if (nrow(entry) > 1) stop("Pin '", name, "' was found in multiple boards: ", paste(entry$board, collapse = ","),  ".")

  board <- entry$board
  entry <- pin_find(name = name, board = board, metadata = metadata, extended = extended)

  entry
}

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
#' library(pins)
#'
#' # define local board
#' board_register_local(cache = tempfile())
#'
#' # cache the mtcars dataset
#' pin(mtcars)
#'
#' # print pin information
#' pin_info("mtcars")
#'
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
    metadata <- jsonlite::fromJSON(entry$metadata)
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

print_pin_info <- function(name, e, ident) {
  # avoid empty lavels that are nested
  if (is.list(e) && is.null(names(e)) && length(e) == 1) e <- e[[1]]

  # one-row data frames are better displayed as lists
  if (is.data.frame(e) && nrow(e) == 1) e <- as.list(e)

  name_prefix <- if (!is.null(name) && nchar(name) > 0) paste0(name, ": ") else ""

  if (!is.list(e) && is.vector(e)) {
    # Long strings (like paths) print better on their own line
    if (length(e) > 1 && is.character(e) && max(nchar(e)) > 20) {
      cat(crayon::silver(paste0("#", ident, "- ", name_prefix, "\n")))
      for (i in e) {
        print_pin_info("", i, paste0(ident, "  "))
      }
    }
    else {
      cat(crayon::silver(paste0("#", ident, "- ", name_prefix, paste(e, collapse = ", "), "\n")))
    }
  }
  else if (is.data.frame(e)) {
    cat(crayon::silver(paste0("#", ident, "- ", name_prefix)))
    if (length(colnames(e)) > 0) cat(crayon::silver(paste0("(", colnames(e)[[1]], ") ")))
    cat(crayon::silver(paste(e[,1], collapse = ", ")))
    if (length(colnames(e)) > 1) cat(crayon::silver("..."))
    cat(crayon::silver("\n"))
  }
  else if (is.list(e)) {
    cat(crayon::silver(paste0("#", ident, "- ", name_prefix, "\n")))
    for (i in names(e)) {
      print_pin_info(i, e[[i]], paste0(ident, "  "))
    }
  }
  else {
    cat(crayon::silver(paste0("#", ident, "- ", name_prefix, class(e)[[1]], "\n")))
  }
}

#' @keywords internal
#' @export
print.pin_info <- function(x, ...) {
  info <- x

  cat(crayon::silver(paste0("# Source: ", info$board, "<", info$name, "> [", info$type, "]\n")))
  if (nchar(info$description) > 0) cat(crayon::silver(paste0("# Description: ", info$description, "\n")))
  if (!is.null(info$signature)) cat(crayon::silver(paste0("# Signature: ", info$signature, "\n")))

  info$board <- info$name <- info$type <- info$description <- info$signature <- NULL

  is_first <- TRUE
  for (name in names(info)) {
    e <- info[[name]]
    if (identical(is.na(e), FALSE) && identical(is.null(e), FALSE) && !(is.character(e) && nchar(e) == 0)) {
      if (is_first) cat(crayon::silver(paste0("# Properties:", "\n")))
      is_first <- FALSE

      print_pin_info(name, e, "   ")
    }
  }
}

#' @rdname custom-pins
#' @keywords internal
#' @export
pin_fetch <- function(path, ...) {
  UseMethod("pin_fetch")
}

#' Pin Versions
#'
#' Retrieve versions available for a given pin.
#'
#' @param name The exact name of the pin to match when searching.
#' @param board The board name used to find the pin.
#' @param full Should the full versioned paths be shown? Defaults to \code{FALSE}.
#' @param ... Additional parameters.
#'
#' @examples
#' library(pins)
#'
#' # define local board with versioning enabled
#' board_register_local(cache = tempfile(), versions = TRUE)
#'
#' # cache the mtcars dataset
#' pin(mtcars, name = "mtcars")
#'
#' # cache variation of the mtcars dataset
#' pin(mtcars * 10, name = "mtcars")
#'
#' # print the mtcars versions
#' versions <- pin_versions("mtcars") %>% print()
#'
#' # retrieve the original version
#' pin_get("mtcars", version = versions$version[1])
#'
#' # retrieve the variation version
#' pin_get("mtcars", version = versions$version[2])
#' @export
pin_versions <- function(name, board = NULL, full = FALSE, ...) {
  versions <- board_pin_versions(board_get(board), name)

  if (!full) {
    versions$version <- board_versions_shorten(versions$version)
  }

  format_tibble(versions)
}
