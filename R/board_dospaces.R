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

