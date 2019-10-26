s3_headers <- function(board, verb, path) {
  date <- format(Sys.time(), "%a, %d %b %Y %X %z")

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
                                bucket,
                                key = Sys.getenv("AWS_ACCESS_KEY_ID"),
                                secret = Sys.getenv("AWS_SECRET_ACCESS_KEY"),
                                cache = NULL,
                                ...) {
  board$bucket <- bucket
  if (identical(bucket, NULL)) stop("The 's3' board requires a 'bucket' parameter.")

  board$key <- key
  board$secret <- secret

  if (nchar(key) == 0)  stop("The 's3' board requires a 'key' parameter.")
  if (nchar(secret) == 0)  stop("The 's3' board requires a 'secret' parameter.")

  s3_url <- paste0("http://", board$bucket, ".s3.amazonaws.com/")

  board_register_datatxt(name = board$name,
                         url = s3_url,
                         cache = cache,
                         headers = s3_headers,
                         allow_empty = TRUE)

  board_get(board$name)
}

