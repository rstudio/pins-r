
rsconnect_api_auth <- function(board) !is.null(board$key)

rsconnect_api_auth_headers <- function(board, path, verb, content = NULL) {
  if (rsconnect_api_auth(board)) {
    headers <- list("Authorization" = paste("Key", board$key))
  } else {
    headers <- rsconnect_token_headers(board, path, verb, content)
  }

  if (!identical(class(content), "form_file")) {
    headers$`Content-Type` <- "application/json"
  }

  httr::add_headers(.headers = unlist(headers))
}

rsconnect_api_version <- function(board) {
  jsonlite::fromJSON(rsconnect_api_get(board, "/server_settings")$content)$version
}

rsconnect_api_get <- function(board, path) {
  url <- paste0(board$server, path)
  result <- httr::GET(url, rsconnect_api_auth_headers(board, path, "GET"))

  if (httr::status_code(result) < 200 || httr::status_code(result) >= 300) {
    stop("Failed to retrieve ", url, " ", as.character(httr::content(result)))
  }

  result %>% httr::content()
}

rsconnect_api_delete <- function(board, path) {
  httr::DELETE(paste0(board$server, path),
               rsconnect_api_auth_headers(board, path, "DELETE")) %>%
    httr::content()
}

rsconnect_api_post <- function(board, path, content, encode) {
  url <- paste0(board$server, path)

  if (identical(class(content), "form_file")) {
    encode <- "multipart"
  }
  else {
    content <- as.character(jsonlite::toJSON(content,
                                             dataframe = "columns",
                                             null = "null",
                                             na = "null",
                                             auto_unbox = TRUE,
                                             pretty = TRUE))
    encode <- "raw"
  }

  if (rsconnect_api_auth(board)) {
    result <- httr::POST(url,
                         encode = encode,
                         body = content,
                         rsconnect_api_auth_headers(board, url, "POST", content))
    content <- httr::content(result)
  }
  else {
    result <- 200
    content <- rsconnect_token_post(board, path, content, encode)
  }

  if (httr::status_code(result) != 200 && !is.list(content)) {
    list(
      error = paste("Operation failed with status", httr::status_code(result))
    )
  }
  else {
    content
  }
}

rsconnect_api_download <- function(board, name, path, etag) {
  url <- paste0(board$server, path)

  pin_download(url,
               name,
               board$name,
               headers = rsconnect_api_auth_headers(board, path, "GET"),
               custom_etag = etag)
}
