s3_signature <- function(content, secret, bucket, path) {
  content <- paste(
    "PUT",
    "",
    "application/octet-stream",
    format(Sys.time(), "%a, %b %d %Y %X"),
    file.path("", bucket, path),
    sep = "\n"
  )

  openssl::sha1(charToRaw(content), key = secret) %>%
    base64enc::base64encode()
}

board_initialize.s3 <- function(board, bucket, ...) {
  board$bucket <- bucket
  if (identical(bucket, NULL)) stop("The 's3' board requires a 'bucket' parameter.")

  board
}

board_pin_get.s3 <- function(board, name, ...) {
  NULL
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
