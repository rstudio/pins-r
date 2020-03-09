azure_headers <- function(board, verb, path, file) {
  date <- format(Sys.time(), "%a, %d %b %Y %H:%M:%S %Z", tz = "GMT")
  azure_version <- "2015-04-05"

  # allow full urls to allow arbitrary file downloads
  container <- board$container
  account <- board$account
  if (grepl("^https?://", path)) {
    path_nohttp <-  gsub("^https?://", "", path)
    sub_path <- gsub("^[^/]+/", "", path_nohttp)
    account <- gsub("\\..*", "", path_nohttp)
    path <- gsub("^[^/]+/", "", sub_path)
    container <- sub("/.*", "", sub_path)
  }

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
    paste("x-ms-version", azure_version, sep = ":"),
    paste0("/", account, "/", container, "/", path),
    sep = "\n")

  signature <- openssl::sha256(charToRaw(content), key = base64enc::base64decode(board$key)) %>%
    base64enc::base64encode()

  headers <- httr::add_headers(
    `x-ms-date` = date,
    `x-ms-version` = azure_version,
    `x-ms-blob-type` = "BlockBlob",
    Authorization = paste0("SharedKey ", account, ":", signature)
  )

  headers
}

board_initialize.azure <- function(board,
                                  container = Sys.getenv("AZURE_STORAGE_CONTAINER"),
                                  account = Sys.getenv("AZURE_STORAGE_ACCOUNT"),
                                  key = Sys.getenv("AZURE_STORAGE_KEY"),
                                  cache = NULL,
                                  ...) {
  if (nchar(container) == 0)  stop("The 'azure' board requires a 'container' parameter.")
  if (nchar(account) == 0)  stop("The 'azure' board requires an 'account' parameter.")
  if (nchar(key) == 0)  stop("The 'azure' board requires a 'key' parameter.")


  azure_url <- paste0(
    "https://",
    account,
    ".blob.core.windows.net/",
    container
  )

  board_register_datatxt(name = board$name,
                         url = azure_url,
                         cache = cache,
                         headers = azure_headers,
                         needs_index = FALSE,
                         container = container,
                         account = account,
                         key = key,
                         connect = FALSE,
                         borwse_url = "https://portal.azure.com",
                         ...)

  board_get(board$name)
}

