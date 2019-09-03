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
#' # cache the mtcars dataset
#' pin(mtcars)
#'
#' # cache computation oveer mtcars
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
#' # cache the mtcars dataset
#' pin(mtcars)
#'
#' # retrieve the mtcars pin
#' pin_get("mtcars")
#'
#' # retrieve mtcars pin from packages board
#' pin_get("easyalluvial/mtcars2", board = "packages")
#' @export
pin_get <- function(name, board = NULL, cache = TRUE, ...) {
  if (is.null(board)) {
    board_pin_get_or_null <- function(...) tryCatch(board_pin_get(...), error = function(e) NULL)

    result <- board_pin_get_or_null(board_get(NULL), name)

    if (is.null(result) && is.null(board)) {
      for (board_name in board_list()) {
        if (!cache) pin_reset_cache(board_name, name)
        result <- board_pin_get_or_null(board_get(board_name), name)
        if (!is.null(result)) break
      }
    }
    if (is.null(result)) stop("Failed to retrieve '", name, "' pin.")
  }
  else {
    if (!cache) pin_reset_cache(board, name)
    result <- board_pin_get(board_get(board), name, ...)
  }

  manifest <- pin_manifest_get(result)
  if (is.null(manifest$type)) manifest$type <- "files"

  result <- pin_load(structure(result, class = manifest$type))

  format_tibble(result)
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
#' pin(mtcars)
#'
#' # remove mtcars pin
#' pin_remove(mtcars, board = "temp")
#' @export
pin_remove <- function(name, board) {
  board_pin_remove(board_get(board), name)
}

#' Find Pin
#'
#' Find a pin in any board registered using \code{board_register()}.
#'
#' @param text The text to find in the pin description or name.
#' @param board The board name used to find the pin.
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
pin_find <- function(text = NULL, board = NULL, ...) {
  if (is.null(board) || nchar(board) == 0) board <- board_list()
  metadata <- identical(list(...)$metadata, TRUE)
  text <- pin_content_name(text)

  all_pins <- data.frame(
    name = character(),
    description = character(),
    type = character(),
    metadata = character(),
    board = character())

  for (board_name in board) {
    board_object <- board_get(board_name)

    board_pins <- tryCatch(
      board_pin_find(board = board_object, text, ...),
      error = function(e) {
        warning("Error searching '", board_name, "' board: ", e$message)
        board_empty_results()
      })

    board_pins$board <- rep(board_name, nrow(board_pins))

    all_pins <- rbind(all_pins, board_pins)
  }

  if (!is.null(text)) {
    find_names <- grepl(text, all_pins$name, ignore.case = TRUE)
    find_description <- if (is.null(all_pins$description)) FALSE else grepl(text, all_pins$description, ignore.case = TRUE)
    all_pins <- all_pins[find_names | find_description,]
  }

  if (!metadata) {
    all_pins <- all_pins[, names(all_pins) != "metadata"]
  }

  if (!is.null(list(...)$name)) {
    name <- list(...)$name
    all_pins <- all_pins[all_pins$name == name,]
    if (nrow(all_pins) > 0) all_pins <- all_pins[1,]
  }

  format_tibble(all_pins)
}

#' Preview Pin
#'
#' Previews a subset of the pin contents, useful to print or display
#' a subset of the pin contents.
#'
#' @param x The pin to preview, retrieved with \code{pin_get()}.
#' @param board The board where this pin will be retrieved from.
#' @param ... Additional parameters.
#'
#' @keywords internal
#' @export
pin_preview <- function(x, board = NULL, ...) {
  UseMethod("pin_preview")
}

#' Load Pin
#'
#' Load a pin from the given file path making use of the pin type.
#'
#' @param path The file to load as a pin.
#' @param ... Additional parameters.
#'
#' @keywords internal
#' @export
pin_load <- function(path, ...) {
  UseMethod("pin_load")
}
