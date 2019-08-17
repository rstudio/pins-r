#' Register GitHub Board
#'
#' Wrapper with explicit parameters over \code{board_register()} to
#' register a GitHub repo as a board.
#'
#' @param repo The GitHub repository formatted as 'owner/repo'.
#' @param name Optional name for this board, defaults to 'github'.
#' @param branch The branch to use when commiting pins.
#' @param token Token to use when \code{GITHUB_PAT} is not specified.
#' @param path The subdireectory in the repo where the pins will be stored.
#'
#' @seealso board_register
#'
#' @examples
#' board_register("github", repo = "datatxtorg/datatxt-site")
#'
#' @export
board_register_github <- function(repo,
                                  name = "github",
                                  branch = "master",
                                  token = NULL,
                                  path = "") {
  board_register("github", name = name, repo = repo, branch = branch, token = token, path = path)
}
