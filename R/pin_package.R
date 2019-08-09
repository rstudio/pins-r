#' @keywords internal
#' @export
pin_load.package <- function(path, ...) {
  files <- dir(path, full.names = TRUE)
  files <- files[!grepl("data\\.txt$", files)]

  get(load(files))
}
