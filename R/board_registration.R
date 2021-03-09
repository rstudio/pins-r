#' Retrieve Default Cache Path
#'
#' Retrieves the default path used to cache boards and pins. Makes
#' use of the `rappdirs` package to use cache folders
#' defined by each OS.
#'
#' @examples
#' # retrieve default cache path
#' board_cache_path()
#' @export
board_cache_path <- function() {
  # if a configuration is present this could mean we are running in a production environment without user caches
  if (nchar(Sys.getenv("R_CONFIG_ACTIVE")) > 0 && nchar(Sys.getenv("PINS_USE_CACHE")) == 0)
    tempfile()
  else
    getOption("pins.path", rappdirs::user_cache_dir("pins"))
}
