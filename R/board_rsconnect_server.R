rsc_server <- function(auth, server = NULL, account = NULL, key = NULL) {
  auth <- check_auth(auth, server, key)

  if (auth == "manual") {
    rsc_server_manual(server, key)
  } else if (auth == "envvar") {
    rsc_server_manual(
      server %||% envvar_get("CONNECT_SERVER") %||% abort("Can't find CONNECT_SERVER env var"),
      key %||% envvar_get("CONNECT_API_KEY") %||% abort("Can't find CONNECT_API_KEY env var")
    )
  } else {
    rsc_server_rsconnect(server, account)
  }
}

check_auth <- function(auth = c("auto", "manual", "envvar", "rsconnect"), server = NULL, key = NULL) {
  auth <- arg_match(auth)
  if (auth == "auto") {
    if (!is.null(server) && !is.null(key)) {
      "manual"
    } else if (has_envvars(c("CONNECT_API_KEY", "CONNECT_SERVER"))) {
      "envvar"
    } else if (rsc_rsconnect_is_configured()) {
      "rsconnect"
    } else {
      abort(c(
        "auth = `auto` has failed to find a way to authenticate",
        "`server` and `key` not provided for `auth = 'manual'`",
        "Can't find CONNECT_SERVER and CONNECT_API_KEY envvars for `auth = 'envvar'`",
        "Can't find any rsconnect::accounts() for `auth = 'rsconnect'`"
      ))
    }
  } else {
    auth
  }
}

rsc_server_manual <- function(server, key) {
  url <- server %||% abort("`server` must be supplied")
  url <- rsc_normalize_server_url(url)
  server_name <- httr::parse_url(url)$hostname

  key <- key %||% abort("`key` must be supplied")

  list(
    url = url,
    account = NULL, # determined in board_rsconnect() by querying API
    server_name = server_name,
    auth = new_hidden(key)
  )
}

rsc_rsconnect_is_configured <- function() {
  is_installed("rsconnect") && !is.null(rsconnect::accounts())
}

rsc_server_rsconnect <- function(server = NULL, name = NULL) {
  check_installed("rsconnect")

  accounts <- rsconnect::accounts()
  if (is.null(accounts)) {
    abort("No RStudio Connect servers have been registered")
  }

  if (!is.null(server)) {
    server <- arg_match0(server, accounts$server, "server")
    accounts <- accounts[accounts$server == server, , drop = FALSE]
  } else {
    accounts <- accounts[accounts$server != "shinyapps.io", , drop = FALSE]
  }

  if (!is.null(name)) {
    name <- arg_match0(name, accounts$name, "account")
    accounts <- accounts[accounts$name == name, , drop = FALSE]
  }

  if (nrow(accounts) > 1) (
    abort(c(
      "Found multiple matching RStudio Connect servers",
      i = "Please disambiguate with `server` and/or `account`"
    ))
  )

  server_info <- rsconnect::serverInfo(accounts$server)
  account_info <- rsconnect::accountInfo(accounts$name, accounts$server)
  account_info$private_key <- new_hidden(account_info$private_key)

  list(
    url = rsc_normalize_server_url(server_info$url),
    account = accounts$name,
    server_name = accounts$server,
    auth = account_info
  )
}

rsc_normalize_server_url <- function(x) {
  x <- sub("__api__/?$", "", x)
  x <- sub("/$", "", x)
  x
}

new_hidden <- function(x) structure(x, class = "pins_hidden")

#' @export
print.pins_hidden <- function(x, ...) {
  cat("<hidden>")
  invisible(x)
}
#' @export
str.pins_hidden <- function(object, ...) {
  cat(" <hidden>\n")
}
