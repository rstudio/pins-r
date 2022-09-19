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
#' @export
#' @examples
#' board <- board_temp()
#' pin_write(board, mtcars, "mtcars-csv", type = "csv")
#' pin_write(board, mtcars, "mtcars-json", type = "json")
#'
#' pin_manifest(board)
#' fs::path(board$path, "pins.txt") %>% readLines() %>% cat(sep = "\n")
pin_manifest <- function(board) {
  UseMethod("pin_manifest")
}

#' @export
pin_manifest.default <- function(board) {
  abort(glue::glue("Not supported for {class(board)[[1]]}."))
}

pin_manifest_internal <- function(board, pins_txt_uploader) {

  # pins_txt_uploader: function that takes path to a manifest file,
  # then uploads file to the root folder of board, naming it "pins.txt".
  pins_txt_uploader <- rlang::as_function(pins_txt_uploader)

  manifest <- make_manifest(board)

  # write manifest to temporary tile
  temp_file <- withr::local_tempfile()
  yaml::write_yaml(manifest, file = temp_file)

  # upload file to board
  pins_txt_uploader(temp_file)

  pins_inform("Manifest file written to root-folder of board, as 'pins.txt'.")

  invisible(board)
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

