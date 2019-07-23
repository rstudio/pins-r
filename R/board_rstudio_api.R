
rstudio_api_auth <- function(board) !is.null(board$key)

rstudio_api_auth_headers <- function(board, path, verb, content = NULL) {
  if (rstudio_api_auth(board)) {
    headers <- list("Authorization" = paste("Key", board$key))
  } else {
    headers <- rstudio_token_headers(board, path, verb, content)
  }

  if (!identical(class(content), "form_file")) {
    headers$`Content-Type` <- "application/json"
  }

  httr::add_headers(.headers = unlist(headers))
}

rstudio_api_get <- function(board, path) {
  httr::GET(paste0(board$server, path),
            rstudio_api_auth_headers(board, path, "GET")) %>%
    httr::content()
}

rstudio_api_post <- function(board, path, content, encode) {
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

  if (rstudio_api_auth(board)) {
    result <- httr::POST(url,
                         encode = encode,
                         body = content,
                         rstudio_api_auth_headers(board, url, "POST", content))
    content <- httr::content(result)
  }
  else {
    result <- 200
    content <- rstudio_token_post(board, path, content, encode)
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

rstudio_api_download <- function(board, path, download) {
  url <- paste0(board$server, path)

  httr::GET(url,
            httr::write_disk(download),
            rstudio_api_auth_headers(board, path, "GET"))
}
