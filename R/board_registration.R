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
#' board_register_github(repo = "datatxtorg/datatxt-site")
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
#' board_register_kaggle(token = "path/to/kaggle.json")
#' }
#'
#' @export
board_register_kaggle <- function(token,
                                  name = "kaggle") {
  board_register("kaggle", name = name, token = repo)
}

#' Register RStudio Connect Board
#'
#' Wrapper with explicit parameters over \code{board_register()} to
#' register RStudio Connecet as a board.
#'
#' @param name Optional name for this board, defaults to 'rsconnect'.
#' @param server Optional address to RStudio Connect server.
#' @param account Optional account name to use with RStudio Connect.
#' @param token The RStudio Connect API token.
#' @param output_files Should the output in an automated report create output files?
#'
#' @seealso board_register
#'
#' @examples
#' \dontrun{
#' # register from rstudio
#' board_register_rsconnect()
#'
#' # register from rstudio with multiple servers
#' board_register_rsconnect(server = "https://rstudio-connect-server")
#'
#' # register from rstudio with multiple account
#' board_register_rsconnect(account = "account-name")
#'
#' # register automated report for rstudio connect
#' board_register_rsconnect(key = Sys.getenv("RSTUDIO_KEY"),
#'                          server = "https://rstudio-connect-server")
#' }
#'
#' @export
board_register_rsconnect <- function(name = "rsconnect",
                                     server = NULL,
                                     account = NULL,
                                     token = NULL,
                                     output_files = FALSE) {
  board_register("rsconnect", name = name, server = server, account = account, token = token, output_files = output_files)
}
