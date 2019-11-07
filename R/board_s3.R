s3_headers <- function(board, verb, path, file) {
  date <- format(Sys.time(), "%a, %d %b %Y %H:%M:%S %z")

  # allow full urls to allow arbitrary file downloads
  path <- gsub(paste0(board$url, "/"), "", path, fixed = TRUE)

  content <- paste(
    verb,
    "",
    "application/octet-stream",
    date,
    file.path("", board$bucket, path),
    sep = "\n"
  )

  signature <- openssl::sha1(charToRaw(content), key = board$secret) %>%
    base64enc::base64encode()

  headers <- httr::add_headers(
    Host = paste0(board$bucket, ".s3.amazonaws.com"),
    Date = date,
    `Content-Type` = "application/octet-stream",
    Authorization = paste0("AWS ", board$key, ":", signature)
  )

  headers
}

board_initialize.s3 <- function(board,
                                bucket = Sys.getenv("AWS_BUCKET"),
                                key = Sys.getenv("AWS_ACCESS_KEY_ID"),
                                secret = Sys.getenv("AWS_SECRET_ACCESS_KEY"),
                                cache = NULL,
                                ...) {
  board$bucket <- bucket
  if (nchar(bucket) == 0) stop("The 's3' board requires a 'bucket' parameter.")

  if (nchar(key) == 0)  stop("The 's3' board requires a 'key' parameter.")
  if (nchar(secret) == 0)  stop("The 's3' board requires a 'secret' parameter.")

  s3_url <- paste0("http://", board$bucket, ".s3.amazonaws.com")

  board_register_datatxt(name = board$name,
                         url = s3_url,
                         cache = cache,
                         headers = s3_headers,
                         needs_index = FALSE,
                         key = key,
                         secret = secret,
                         bucket = bucket,
                         connect = FALSE,
                         browse_url = paste0("https://s3.console.aws.amazon.com/s3/buckets/", bucket, "/"))

  board_get(board$name)
}

