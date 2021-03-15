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
#' `pin()` allows you to cache remote resources and intermediate results with ease. When
#' caching remote resources, usually URLs, it will check for HTTP caching headers to avoid
#' re-downloading when the remote result has not changed.
#'
#' This makes it ideal to support reproducible research by requiring manual instruction to
#' download resources before running your R script.
#'
#' In addition, `pin()` still works when working offline or when the remote resource
#' becomes unavailable; when this happens, a warning will be triggered but your code will
#' continue to work.
#'
#' `pin()` will stores data frames in two files, an R native file and a 'CSV' file. To
#' force saving a pin only using R's native (RDS) format, you can use `pin(I(data))`.
#' This can improve performance and size at the cost of making the pin unreadable from other
#' tools and programming languages.
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

#' Create Pin Name
#'
#' Creates a pin name from an character expression generated with `deparse(substitute(x))`.
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

