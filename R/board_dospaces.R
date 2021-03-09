#' Register DigitalOcean Board
#'
#' Wrapper with explicit parameters over `board_register()` to
#' register a DigitalOcean Spaces board.
#'
#' @param name Optional name for this board, defaults to 's3'.
#' @param space The name of the DigitalOcean space. Defaults to the `DO_SPACE` environment
#'   variable.
#' @param key The key of the DigitalOcean space. Defaults to the `DO_ACCESS_KEY_ID` environment
#'   variable.
#' @param secret The secret of the DigitalOcean space. Defaults to the `DO_SECRET_ACCESS_KEY` environment
#'   variable.
#' @param datacenter The datacenter of the DigitalOcean space. Defaults to the `DO_DATACENTER` environment
#'   variable.
#' @param cache The local folder to use as a cache, defaults to `board_cache_path()`.
#' @param host The host to use for storage, defaults to `"digitaloceanspaces.com"`.
#' @param path The subdirectory in the repo where the pins will be stored.
#' @param ... Additional parameters required to initialize a particular board.
#'
#' @details
#'
#' This function requires a DigitalOcean space to be manually created; otherwise,
#' registering a DigitalOcean space will fail.
#'
#' @seealso board_register
#'
#' @examples
#' \dontrun{
#' # the following example requires a DigitalOcean Spaces API key
#' board_register_s3(bucket = "s3bucket")
#' }
#' @export
board_register_dospace <- function(name = "dospace",
                                   space = Sys.getenv("DO_SPACE"),
                                   key = Sys.getenv("DO_ACCESS_KEY_ID"),
                                   secret = Sys.getenv("DO_SECRET_ACCESS_KEY"),
                                   datacenter = Sys.getenv("DO_DATACENTER"),
                                   cache = board_cache_path(),
                                   host = "digitaloceanspaces.com",
                                   path = NULL,
                                   ...) {
  board_register("dospace",
                 name = name,
                 space = space,
                 key = key,
                 secret = secret,
                 datacenter = datacenter,
                 cache = cache,
                 host = host,
                 path = path,
                 ...)
}

dospace_headers <- function(board, verb, path, file) {
  date <- format(Sys.time(), "%a, %d %b %Y %H:%M:%S %z", tz = "UTC")

  # allow full urls to allow arbitrary file downloads
  space <- board$space
  if (grepl("^https?://", path)) {
    path_nohttp <-  gsub("^https?://", "", path)
    path <- gsub("^[^/]+/", "", path_nohttp)
    space <- gsub("\\..*", "", path_nohttp)
  }

  content <- paste(
    verb,
    "",
    "application/octet-stream",
    date,
    file.path("", space, path),
    sep = "\n"
  )

  signature <- openssl::sha1(charToRaw(content), key = board$secret) %>%
    base64enc::base64encode()

  headers <- httr::add_headers(
    Host = paste0(space, ".", board$datacenter, ".", board$host),
    Date = date,
    `Content-Type` = "application/octet-stream",
    Authorization = paste0("AWS ", board$key, ":", signature)
  )

  headers
}

#' @export
board_initialize.dospace <- function(board,
                                     space = Sys.getenv("DO_SPACE"),
                                     key = Sys.getenv("DO_ACCESS_KEY_ID"),
                                     secret = Sys.getenv("DO_SECRET_ACCESS_KEY"),
                                     datacenter = Sys.getenv("DO_DATACENTER"),
                                     cache = NULL,
                                     host = "digitaloceanspaces.com",
                                     ...) {
  board$space <- space
  if (nchar(space) == 0) stop("The 'dospace' board requires a 'space' parameter.")

  if (nchar(key) == 0)  stop("The 'dospace' board requires a 'key' parameter.")
  if (nchar(secret) == 0)  stop("The 'dospace' board requires a 'secret' parameter.")
  if (nchar(datacenter) == 0) stop("The 'dospace' board requires a 'datacenter' parameter.")

  dospaces_url <- paste0("https://", board$space, ".", datacenter, ".", host)

  board_register_datatxt(name = board$name,
                         url = dospaces_url,
                         cache = cache,
                         headers = dospace_headers,
                         needs_index = FALSE,
                         key = key,
                         secret = secret,
                         space = space,
                         connect = FALSE,
                         datacenter = datacenter,
                         browse_url = paste0("https://cloud.digitalocean.com/spaces/", space),
                         host = host,
                         ...)

  board_get(board$name)
}

