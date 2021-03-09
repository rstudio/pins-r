
#' Register Azure Board
#'
#' Wrapper with explicit parameters over \code{board_register()} to
#' register a Microsoft Azure Storage Blob as a board.
#'
#' @param name Optional name for this board, defaults to 'azure'.
#' @param container The name of the Azure Storage container. Defaults to the \code{AZURE_STORAGE_CONTAINER} environment
#'   variable.
#' @param account The account of the Azure Storage container. Defaults to the \code{AZURE_STORAGE_ACCOUNT} environment
#'   variable.
#' @param key The key of the Azure Storage container Defaults to the \code{AZURE_STORAGE_KEY} environment
#'   variable.
#' @param cache The local folder to use as a cache, defaults to \code{board_cache_path()}.
#' @param path The subdirectory in the repo where the pins will be stored.
#' @param ... Additional parameters required to initialize a particular board.
#'
#' @details
#'
#' This function requires an Azure Storage container to be manually created; otherwise,
#' registering an Azire board will fail.
#'
#' @seealso board_register
#'
#' @examples
#' \dontrun{
#' # the following example requires an Azure Storage key
#' board_register_azure(container = "pinscontainer",
#'                      account = "pinsstorage",
#'                      key = "abcabcabcabcabcabcabcabcabcab==")
#' }
#' @export
board_register_azure <- function(name = "azure",
                                 container = Sys.getenv("AZURE_STORAGE_CONTAINER"),
                                 account = Sys.getenv("AZURE_STORAGE_ACCOUNT"),
                                 key = Sys.getenv("AZURE_STORAGE_KEY"),
                                 cache = board_cache_path(),
                                 path = NULL,
                                 ...) {
  board_register("azure",
                 name = name,
                 account = account,
                 container = container,
                 key = key,
                 cache = cache,
                 path = path,
                 ...)
}

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

#' @export
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
                         browse_url = "https://portal.azure.com",
                         ...)

  board_get(board$name)
}

