#' Write manifest file
#'
#' A manifest file contains a named list of pins, where the values identify
#' pin-version directories.
#' This can be useful for a board built using, for example,
#' [board_folder()] or [board_s3()], then served as a web-site,
#' such that others can consume using [board_url()].
#'
#' This function is not supported for read-only boards.
#' It is called for the side-effect of writing a manifest file,
#' `pins.txt`, to the `boards`'s root-directory.
#'
#' @param board A writable pin board.
#'
#' @return `board`, invisibly.
#' @export
#' @examples
#' board <- board_temp()
#' pin_write(board, mtcars, "mtcars-csv", type = "csv")
#' pin_write(board, mtcars, "mtcars-json", type = "json")
#'
#' pin_manifest(board)
#'
#' # see the manifest's format
#' fs::path(board$path, "pins.txt") %>% readLines() %>% cat(sep = "\n")
pin_manifest <- function(board) {
  UseMethod("pin_manifest")
}

#' @export
pin_manifest.default <- function(board) {
  abort(glue::glue("Not supported for {class(board)[[1]]}."))
}

make_manifest <- function(board) {
  # given board, return named list:
  #   - names are pin names
  #    - values are relative paths to version directories

  pin_names <- pin_list(board)

  result <- map(
    pin_names,
    ~fs::path(.x, pin_versions(board, name = .x)$version) %>%
       append_slash() %>%
       as.list()
  )
  names(result) <- pin_names

  result
}

