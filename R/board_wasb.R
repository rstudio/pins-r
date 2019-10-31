wasb_headers <- function(board, verb, path, file) {
  date <- format(Sys.time(), "%a, %d %b %Y %H:%M:%S %Z", tz = "GMT")
  wasb_version <- "2015-04-05"

  # allow full urls to allow arbitrary file downloads
  path <- gsub(paste0(board$url, "/"), "", path, fixed = TRUE)

  content_length <- ""
  content_type <- ""

  if (!is.null(file)) {
    content_length <- as.integer(file.info(file)$size)
    content_type <- mime::guess_type(file)
  }

  content <- paste(
    verb,
    "\n",
    content_length,
    "",
    content_type,
    "\n\n\n\n\n",
    paste("x-ms-blob-type", "BlockBlob", sep = ":"),
    paste("x-ms-date", date, sep = ":"),
    paste("x-ms-version", wasb_version, sep = ":"),
    paste0("/", board$account, "/", board$container, "/", path),
    sep = "\n")

  signature <- openssl::sha256(charToRaw(content), key = base64enc::base64decode(board$key)) %>%
    base64enc::base64encode()

  headers <- httr::add_headers(
    `x-ms-date` = date,
    `x-ms-version` = wasb_version,
    `x-ms-blob-type` = "BlockBlob",
    Authorization = paste0("SharedKey ", board$account, ":", signature)
  )

  headers
}

board_initialize.wasb <- function(board,
                                  container = Sys.getenv("AZURE_STORAGE_CONTAINER"),
                                  account = Sys.getenv("AZURE_STORAGE_ACCOUNT"),
                                  key = Sys.getenv("AZURE_STORAGE_KEY"),
                                  cache = NULL,
                                  ...) {
  if (nchar(container) == 0)  stop("The 'wasb' board requires a 'container' parameter.")
  if (nchar(account) == 0)  stop("The 'wasb' board requires an 'account' parameter.")
  if (nchar(key) == 0)  stop("The 'wasb' board requires a 'key' parameter.")


  wasb_url <- paste0(
    "https://",
    account,
    ".blob.core.windows.net/",
    container
  )

  board_register_datatxt(name = board$name,
                         url = wasb_url,
                         cache = cache,
                         headers = wasb_headers,
                         needs_index = FALSE,
                         container = container,
                         account = account,
                         key = key)

  board_get(board$name)
}

