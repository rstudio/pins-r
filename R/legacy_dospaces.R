#' DigitalOcean board (legacy API)
#'
#' To use DigitalOcean Spaces as a board, you first
#' need an DigitalOcean space and a storage key. You can sign-up and create
#' those at [digitalocean.com](https://www.digitalocean.com/).
#'
#' @inheritParams legacy_datatxt
#' @param space The name of the DigitalOcean space.
#' @param key,secret The key and secret for your space. You can create
#'   a key and secret in the "Spaces access keys" in your API settings.
#'
#'  The `secret` is equivalent to a password, so generally should not be stored
#'  in your script. The easiest alternative is to store it in the
#'  `DO_SECRET_ACCESS_KEY` environment variable, which `legacy_dospace()` will
#'  use by default.
#' @param datacenter The datacenter name.
#' @param host The host to use for storage, defaults to `"digitaloceanspaces.com"`.
#' @examples
#' \dontrun{
#' # the following example requires a DigitalOcean Spaces API key
#' board <- legacy_dospace(bucket = "s3bucket")
#' }
#' @export
legacy_dospace <- function(
                          space = Sys.getenv("DO_SPACE"),
                          key = Sys.getenv("DO_ACCESS_KEY_ID"),
                          secret = Sys.getenv("DO_SECRET_ACCESS_KEY"),
                          datacenter = Sys.getenv("DO_DATACENTER"),
                          cache = NULL,
                          host = "digitaloceanspaces.com",
                          name = "dospace",
                          ...) {
  if (nchar(space) == 0) stop("The 'dospace' board requires a 'space' parameter.")
  if (nchar(key) == 0) stop("The 'dospace' board requires a 'key' parameter.")
  if (nchar(secret) == 0) stop("The 'dospace' board requires a 'secret' parameter.")
  if (nchar(datacenter) == 0) stop("The 'dospace' board requires a 'datacenter' parameter.")

  legacy_datatxt(
    name = name,
    url = paste0("https://", space, ".", datacenter, ".", host),
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
    ...
  )
}


#' @rdname legacy_dospace
#' @export
board_register_dospace <- function(name = "dospace",
                                   space = Sys.getenv("DO_SPACE"),
                                   key = Sys.getenv("DO_ACCESS_KEY_ID"),
                                   secret = Sys.getenv("DO_SECRET_ACCESS_KEY"),
                                   datacenter = Sys.getenv("DO_DATACENTER"),
                                   cache = board_cache_path(name),
                                   host = "digitaloceanspaces.com",
                                   path = NULL,
                                   ...) {
  board <- legacy_dospace(
    name = name,
    space = space,
    key = key,
    secret = secret,
    datacenter = datacenter,
    cache = cache,
    host = host,
    path = path,
    ...
  )
  board_register2(board)
}


dospace_headers <- function(board, verb, path, file) {
  date <- http_date()

  # allow full urls to allow arbitrary file downloads
  space <- board$space
  if (grepl("^https?://", path)) {
    path_nohttp <- gsub("^https?://", "", path)
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
    jsonlite::base64_enc()

  headers <- httr::add_headers(
    Host = paste0(space, ".", board$datacenter, ".", board$host),
    Date = date,
    `Content-Type` = "application/octet-stream",
    Authorization = paste0("AWS ", board$key, ":", signature)
  )

  headers
}
