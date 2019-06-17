#' @keywords internal
#' @export
pin_load.package <- function(path, ...) {
  get(load(path))
}
