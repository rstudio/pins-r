#' Use an RStudio Connect board
#'
#' @description
#' To use a RStudio Connect board, you need to first authenticate. The easiest
#' way to do so is by launching **Tools** - **Global Options** - **Publishing**
#' - **Connect**, and follow the instructions.
#'
#' You can share pins with others in RStudio Connect by changing the viewers
#' of the document to specific users or groups. This is accomplished by opening
#' the new published pin and then changing access under the settings tab.
#' After you've shared the pin, it will be automatically available to others.
#'
#' # Public
#'
#' You can also choose to share a pin publicly and avoid having to register
#' the RStudio Connect board to retrieve this pin.
#'
#' To create a public pin, first publish a pin and navigate to RStudio Connect;
#' then set the "Access" to "Anyone - no login required" -- The pin will become
#' public and accessible to anyone using their content URL. The remote resource
#' stored in RStudio Connect can then be cached locally with `pin()` as follows:
#'
#' ```r
#' pin("https://rstudio-connect-server/content/1234", name = "my-rsc-content")
#' ```
#'
#' To avoid having to change the "Access" manually, you can also set the
#' `access_type` to `acl`, `loggend_in` or `all` when creating a pin:
#'
#' ```r
#'  pin("https://rstudio-connect-server/content/1234", name = "my-rsc-content",
#'    access_type = "all"
#'  )
#' ```
#'
#' @inheritParams new_board
#' @param auth There are two approaches to auth: you can either use `"envvars"`
#'   `CONNECT_API_KEY` and `CONNECT_SERVER` or the rsconnect package. The
#'   default is `auto`, which will use the environment variables if both are
#'   available, and rsconnect if not.
#' @param server For `auth = "envvar"` the full url to the server.
#'   For `auth = 'rsconnect'` a host name used to disambiguate RSC accounts.
#' @param account A user name used to disambiguate multiple RSC accounts
#' @param key The RStudio Connect API key.
#' @param output_files `r lifecycle::badge("deprecated") No longer supported.
#' @family boards
#' @export
#' @examples
#' \dontrun{
#' board <- board_rsconnect()
#' # Share the mtcars with your team
#' board %>% pin_write(mtcars, "mtcars")
#'
#' # Download a shared dataset
#' board %>% pin_read(mtcars)
#' }
board_rsconnect <- function(
                            auth = c("auto", "envvar", "rsconnect"),
                            server = NULL,
                            account = NULL,
                            key = NULL,
                            output_files = FALSE,
                            cache = board_cache_path(name),
                            name = "rsconnect",
                            versions = TRUE,
                            ...) {

  auth <- check_auth(auth)
  if (auth == "envvar") {
    server <- server %||% Sys.getenv("CONNECT_SERVER")
    account <- NULL # see below
    server_name <- httr::parse_url(server)$hostname
    key <- key %||% Sys.getenv("CONNECT_API_KEY")
  } else {
    info <- rsc_account_find(server, account)
    account <- info$name
    server <- info$server
    server_name <- info$server_name
  }

  board <- new_board("pins_board_rsconnect",
    name = name,
    cache = cache,
    server = server,
    account = account,
    server_name = server_name,
    url = url,
    key = key,
    versions = versions,
    ...
  )

  if (rsc_version(board) < "1.7.7") {
    abort("Pins requires RSC 1.7.7 or later")
  }

  # Fill in account name if auth == "envvar"
  board$account <- board$account %||% rsc_GET(board, "users/current/")$username

  board
}

check_auth <- function(auth = c("auto", "envvar", "rsconnect")) {
  auth <- arg_match(auth)
  if (auth == "auto") {
    if (has_envvars(c("CONNECT_API_KEY", "CONNECT_SERVER"))) {
      "envvar"
    } else {
      "rsconnect"
    }
  } else {
    auth
  }
}

rsc_account_find <- function(server = NULL, name = NULL) {
  check_installed("rsconnect")

  accounts <- rsconnect::accounts()
  if (is.null(accounts)) {
    abort("No RStudio Connect accounts has been registered")
  }

  if (!is.null(server)) {
    accounts <- accounts[accounts$server == server, , drop = FALSE]
  } else {
    accounts <- accounts[accounts$server != "shinyapps.io", , drop = FALSE]
  }

  if (!is.null(name)) {
    accounts <- accounts[accounts$name == name, , drop = FALSE]
  }

  if (nrow(accounts) == 0) {
    abort("No matching RStudio Connect accounts found")
  } else if (nrow(accounts) > 1) (
    abort(c(
      "Multiple matching RStudio Connect found",
      i = "Please disambiguate with `server` and `account`"
    ))
  )

  url <- rsconnect::serverInfo(accounts$server)$url

  list(
    name = accounts$name,
    server_name = accounts$server,
    server = sub("/__api__$", "", url)
  )
}

#' @export
board_pin_find.pins_board_rsconnect <- function(board,
                                               text = NULL,
                                               name = NULL,
                                               extended = FALSE,
                                               metadata = FALSE,
                                     ...) {

  params <- list(
    search = text,
    filter = "content_type:pin",
    count = 1000
  )
  json <- rsc_GET(board, "applications/", params)

  pins <- json$applications
  name <- map_chr(pins, ~ .x$name)
  user <- map_chr(pins, ~ .x$owner_username)

  wibble(
    name = paste0(user, "/", name),
    title = map_chr(pins, ~ .x$title %||% ""),
    description = map_chr(pins, ~ .x$description)
  )
}

#' @export
board_pin_remove.pins_board_rsconnect <- function(board, name, ...) {
  rsc_content_delete(board, name)
}

#' @export
board_browse.pins_board_rsconnect <- function(board, ...) {
  utils::browseURL(board$server)
}

#' @export
board_pin_versions.pins_board_rsconnect <- function(board, name, ...) {
  guid <- rsc_content_find(board, name)$guid
  rsc_content_versions(board, guid)
}
