# nocov start

rsconnect_token_dependencies <- function() {
  list(
    accounts = get_function("accounts", "rsconnect"),
    account_info = get_function("accountInfo", "rsconnect"),
    server_info = get_function("serverInfo", "rsconnect"),
    signature_headers = get_function("signatureHeaders", "rsconnect"),
    http_function = get_function("httpFunction", "rsconnect")
  )
}

rsconnect_token_parse_url <- function(urlText) {
  # see rsconnect::parseHttpUrl

  matches <- regexec("(http|https)://([^:/#?]+)(?::(\\d+))?(.*)", urlText)
  components <- regmatches(urlText, matches)[[1]]
  if (length(components) == 0)
    stop("Invalid url: ", urlText)

  url <- list()
  url$protocol <- components[[2]]
  url$host <- components[[3]]
  url$port <- components[[4]]
  url$path <- components[[5]]
  url$path_sans_api <- base_path <- gsub("/__api__", "", url$path)
  url
}

rsconnect_token_initialize <- function(board) {
  deps <- rsconnect_token_dependencies()

  if (is.null(deps$accounts)) stop("RStudio Connect is not registered, please install the 'rsconnect' package or specify an API key.")

  accounts <- deps$accounts()
  if (is.null(accounts)) stop("RStudio Connect is not registered, please add a publishing account or specify an API key.")

  if (is.null(board$server)) {
    board$server_name <- accounts$server[1]
    board$server <- gsub("/__api__", "", deps$server_info(board$server_name)$url)
  }

  if (is.null(board$account)) board$account <- accounts[accounts$server == board$server_name,]$name

  board
}

rsconnect_token_headers <- function(board, url, verb, content) {
  deps <- rsconnect_token_dependencies()

  account_info <- deps$account_info(board$account, board$server_name)

  content_file <- NULL
  if (identical(class(content), "form_file")) {
    content_file <- content$path
  }
  else if (!identical(content, NULL)){
    if (!is.character(content)) stop("Unsupported object of class", class(content)[[1]])
    content_file <- tempfile()
    on.exit(unlink(content_file))
    writeChar(content, content_file,  eos = NULL, useBytes = TRUE)
  }

  deps$signature_headers(account_info, verb, url, content_file)
}

rsconnect_token_post <- function(board, path, content, encode) {
  deps <- rsconnect_token_dependencies()

  server_info <- deps$server_info(board$server_name)
  parsed <- rsconnect_token_parse_url(server_info$url)

  if (identical(class(content), "form_file")) {
    content_file <- content$path
    content_type <- "application/x-gzip"
  }
  else {
    content_file <- tempfile()
    on.exit(unlink(content_file))
    writeChar(content, content_file, eos = NULL, useBytes = TRUE)
    content_type <- "application/json"
  }

  result <- deps$http_function()(parsed$protocol,
                                 parsed$host,
                                 parsed$port,
                                 "POST",
                                 paste0(parsed$path_sans_api, path),
                                 rsconnect_token_headers(board, rsconnect_url_from_path(board, path), "POST", content),
                                 content_type,
                                 content_file)

  jsonlite::fromJSON(result$content)
}

# nocov end
