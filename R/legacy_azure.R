#' Azure board (legacy API)
#'
#' @description
#' To use Microsoft Azure Storage as a board, you'll need an Azure Storage
#' account, an Azure Storage container, and an Azure Storage key.
#' You can sign-up and create those at [portal.azure.com](https://portal.azure.com).
#' @inheritParams legacy_datatxt
#' @param container The name of the Azure Storage container.
#' @param account The name of the Azure Storage account.
#' @param key The access key for the Azure Storage container. You can find
#'  this under "Access keys" in your storage account settings.
#'
#'  The `key` is equivalent to a password, so generally should not be stored
#'  in your script. The easiest alternative is to store it in the
#'  `AZURE_STORAGE_KEY` environment variable, which `legacy_azure()` will
#'  use by default.
#' @examples
#' \dontrun{
#' # the following example requires an Azure Storage key
#' board_register_azure(
#'   container = "pinscontainer",
#'   account = "pinsstorage",
#'   key = "abcabcabcabcabcabcabcabcabcab=="
#' )
#' }
#' @export
legacy_azure <- function(
                        container = Sys.getenv("AZURE_STORAGE_CONTAINER"),
                        account = Sys.getenv("AZURE_STORAGE_ACCOUNT"),
                        key = Sys.getenv("AZURE_STORAGE_KEY"),
                        cache = NULL,
                        name = "azure",
                        ...) {
  if (nchar(container) == 0) stop("The 'azure' board requires a 'container' parameter.")
  if (nchar(account) == 0) stop("The 'azure' board requires an 'account' parameter.")
  if (nchar(key) == 0) stop("The 'azure' board requires a 'key' parameter.")

  azure_url <- paste0("https://", account, ".blob.core.windows.net/", container)

  legacy_datatxt(
    name = name,
    url = azure_url,
    cache = cache,
    headers = azure_headers,
    needs_index = FALSE,
    container = container,
    account = account,
    key = key,
    connect = FALSE,
    browse_url = "https://portal.azure.com",
    ...
  )
}

#' @rdname legacy_azure
#' @export
board_register_azure <- function(name = "azure",
                                 container = Sys.getenv("AZURE_STORAGE_CONTAINER"),
                                 account = Sys.getenv("AZURE_STORAGE_ACCOUNT"),
                                 key = Sys.getenv("AZURE_STORAGE_KEY"),
                                 cache = board_cache_path(name),
                                 path = NULL,
                                 ...) {
  board <- legacy_azure(
    name = name,
    container = container,
    account = account,
    key = key,
    cache = cache,
    path = path,
    ...
  )
  board_register2(board)
}


azure_headers <- function(board, verb, path, file) {
  date <- http_date()
  azure_version <- "2015-04-05"

  # allow full urls to allow arbitrary file downloads
  container <- board$container
  account <- board$account
  if (grepl("^https?://", path)) {
    path_nohttp <- gsub("^https?://", "", path)
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
    sep = "\n"
  )

  signature <- openssl::sha256(charToRaw(content), key = jsonlite::base64_dec(board$key)) %>%
    jsonlite::base64_enc()

  headers <- httr::add_headers(
    `x-ms-date` = date,
    `x-ms-version` = azure_version,
    `x-ms-blob-type` = "BlockBlob",
    Authorization = paste0("SharedKey ", account, ":", signature)
  )

  headers
}
