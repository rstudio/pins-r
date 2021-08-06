local_pin <- function(board, value, ..., env = parent.frame()) {
  name <- random_pin_name()

  pin_write(board, value, name, ...)
  withr::defer(pin_delete(board, name), env)

  name
}

random_pin_name <- function() {
  rand <- sample(c(letters, LETTERS, 0:9), 10, replace = TRUE)
  paste0("test-", paste(rand, collapse = ""))
}
