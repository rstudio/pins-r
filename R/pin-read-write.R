#' Read and write pins
#'
#' Use `pin_write()` to pin an object to board, and `pin_read()` to retrieve
#' it.
#'
#' @param board A pin board, created by [board_local()], [board_rsconnect()],
#'   [board_github()] or other other `board_` function.
#' @param name Pin name.
#' @export
#' @examples
#' b <- board_local(tempfile())
#'
#' b %>% pin_write(mtcars, "mtcars")
#' b
#'
#' b %>% pin_read("mtcars")
pin_read <- function(board, name) {
  pin_get(name, board)
}

#' @param x An object (typically a data frame) to pin.
#' @param description A text description of the pin; most important for
#'   shared boards so that others can understand what the pin contains.
#' @rdname pin_read
#' @export
pin_write <- function(board, x, name = NULL, description = NULL) {
  if (missing(name)) {
    expr <- expr_deparse(enexpr(x))
    name <- pin_default_name(expr, board)
    inform(paste0("Creating pin named '", name, "'"))
  }

  pin(x,
    name = name,
    board = board,
    description = description,
  )
}
