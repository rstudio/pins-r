#' Register GitHub Board
#'
#' Wrapper with explicit parameters over \code{board_register()} to
#' register a GitHub repo as a board.
#'
#' @param repo The GitHub repository formatted as 'owner/repo'.
#' @param name Optional name for this board, defaults to 'github'.
#' @param branch The branch to use when commiting pins.
#' @param token Token to use when \code{GITHUB_PAT} is not specified.
#' @param path The subdirectory in the repo where the pins will be stored.
#'
#' @seealso board_register
#'
#' @examples
#' board_register_github("github", repo = "datatxtorg/datatxt-site")
#'
#' @export
board_register_github <- function(repo,
                                  name = "github",
                                  branch = "master",
                                  token = NULL,
                                  path = "") {
  board_register("github", name = name, repo = repo, branch = branch, token = token, path = path)
}

#' Register Kaggle Board
#'
#' Wrapper with explicit parameters over \code{board_register()} to
#' register Kaggle as a board.
#'
#' @param token The Kaggle token as a path to the \code{kaggle.json} file.
#' @param name Optional name for this board, defaults to 'kaggle'.
#'
#' @seealso board_register
#'
#' @examples
#' \dontrun{
#' board_register_kaggle("kaggle", token = "path/to/kaggle.json")
#' }
#'
#' @export
board_register_kaggle <- function(token,
                                  name = "kaggle") {
  board_register("kaggle", name = name, token = repo)
}
