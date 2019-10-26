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
                                ...) {
  board$bucket <- bucket
  if (identical(bucket, NULL)) stop("The 's3' board requires a 'bucket' parameter.")

  board$key <- key
  board$secret <- secret

  if (nchar(key) == 0)  stop("The 's3' board requires a 'key' parameter.")
  if (nchar(secret) == 0)  stop("The 's3' board requires a 'secret' parameter.")

  board
}

board_pin_get.s3 <- function(board, name, ...) {
  headers <- s3_headers(board, verb = "GET", path = name)

  s3_path <- paste0("http://", board$bucket, ".s3.amazonaws.com/", name)

  local_path <- pin_download(s3_path, name, board$name, headers = headers)

  local_path
}

board_pin_find.s3 <- function(board, text, ...) {
  board_empty_results()
}

board_pin_create.s3 <- function(board, path, name, metadata, ...) {
  stop("The 's3' board does not support creating pins.")
}

board_pin_remove.s3 <- function(board, name, ...) {
  stop("The 's3' board does not support removing pins.")
}
