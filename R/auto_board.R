#' Create a board from a url
#'
#' @param url A url to e.g. a vanity_url on a Connect server
#'
#' @return the board created from the url
#'
#' @export
#'
auto_board <- function(url) {
  preprocessed <- board_and_name_from_url(url)
  preprocessed$board
}
