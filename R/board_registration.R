#' Register GitHub Board
#'
#' Wrapper with explicit parameters over \code{board_register()} to
#' register a GitHub repo as a board.
#'
#' @param name Optional name for this board, defaults to 'github'.
#' @param repo The GitHub repository formatted as 'owner/repo', can be
#'   \code{NULL} if the \code{GITHUB_PAT} environment variable is set.
#' @param branch The branch to use when commiting pins.
#' @param token Token to use when \code{GITHUB_PAT} is not specified.
#' @param path The subdirectory in the repo where the pins will be stored.
#'
#' @seealso board_register
#'
#' @examples
#' \dontrun{
#' # the following example requires a GitHub API key
#' board_register_github(repo = "owner/repo")
#' }
#' @export
board_register_github <- function(name = "github",
                                  repo = NULL,
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
#' @param name Optional name for this board, defaults to 'kaggle'.
#' @param token The Kaggle token as a path to the \code{kaggle.json} file, can
#'   be \code{NULL} if the \code{~/.kaggle/kaggle.json} file already exists.
#' @param overwrite Should \code{~/.kaggle/kaggle.json} be overriden?
#'
#' @seealso board_register
#'
#' @examples
#' \dontrun{
#' # the following example requires a Kaggle API token
#' board_register_kaggle(token = "path/to/kaggle.json")
#' }
#'
#' @export
board_register_kaggle <- function(name = "kaggle",
                                  token = NULL,
                                  overwrite = FALSE) {
  board_register("kaggle", name = name, token = token, overwrite = overwrite)
}

#' Register RStudio Connect Board
#'
#' Wrapper with explicit parameters over \code{board_register()} to
#' register RStudio Connecet as a board.
#'
#' @param name Optional name for this board, defaults to 'rsconnect'.
#' @param server Optional address to RStudio Connect server.
#' @param account Optional account name to use with RStudio Connect.
#' @param key The RStudio Connect API key.
#' @param output_files Should the output in an automated report create output files?
#'
#' @seealso board_register
#'
#' @examples
#' \dontrun{
#' # the following examples require an RStudio Connect API key
#'
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
                                     key = NULL,
                                     output_files = FALSE) {
  board_register("rsconnect", name = name, server = server, account = account, key = key, output_files = output_files)
}

#' Register Data TXT Board
#'
#' Wrapper with explicit parameters over \code{board_register()} to
#' register as a board a website describing resources with a \code{data.txt} file.
#'
#' @param name The name for this board, usually the domain name of the website.
#' @param url Path to the \code{data.txt} file or path containing it.
#'
#' @seealso board_register
#'
#' @examples
#'
#' # register website board using datatxt file
#' board_register_datatxt(name = "txtexample", url = "https://datatxt.org/data.txt")
#'
#' # find pins
#' pin_find(board = "txtexample")
#'
#' @export
board_register_datatxt <- function(name, url) {
  board_register("datatxt", name = name, url = url)
}
