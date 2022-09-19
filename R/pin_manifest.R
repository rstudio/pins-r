#' Write manifest file
#'
#' This can be used if you have a `pins::board_folder()` that you want to
#' serve as a web-site such that others can consume using `pins::board_url()`.
#'
#' This function is called for the side-effect of writing a manifest file,
#' `pins.txt`, to the `boards`'s root-directory.
#'
#' @param board A pin board, currently only `board_folder()` is supported.
#'
#' @return `board`, invisibly.
#' @examples
#' board <- board_temp()
#' pin_write(board, mtcars, "mtcars-csv", type = "csv")
#' pin_write(board, mtcars, "mtcars-json", type = "json")
#'
#' pin_manifest(board)
#' fs::path(board$path, "pins.txt") %>% readLines() %>% cat(sep = "\n")
#' @export
pin_manifest <- function(board) {
  UseMethod("pin_manifest")
}

#' @export
#'
pin_manifest.default <- function(board) {
  abort(glue::glue("Not supported for {class(board)[[1]]}."))
}
