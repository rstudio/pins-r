
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
