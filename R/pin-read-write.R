#' Read and write pins
#'
#' Use `pin_write()` to pin an object to board, and `pin_read()` to retrieve
#' it.
#'
#' @param board A pin board, created by [board_folder()], [board_rsconnect()],
#'   [board_github()] or other other `board_` function.
#' @param name Pin name.
#' @export
#' @examples
#' b <- board_temp()
#'
#' b %>% pin_write(mtcars, "mtcars")
#' b
#'
#' b %>% pin_read("mtcars")
pin_read <- function(board, name, version = NULL, hash = NULL) {
  pin <- board_pin_download(board, name, version = version)

  if (!is.null(hash)) {
    pin_hash <- digest::digest(pin$path, file = TRUE, algo = "xxhash64")
    if (!is_prefix(hash, pin_hash)) {
      abort(paste0(
        "Specified hash '", hash, "' doesn't match pin hash '", pin_hash, "'"
      ))
    }
  }

  object_load(pin$path, pin$meta$type)
}

#' @param x An object (typically a data frame) to pin.
#' @param desc A text description of the pin; most important for
#'   shared boards so that others can understand what the pin contains.
#' @param metadata A list containing additional metadata to store with the pins.
#' @rdname pin_read
#' @export
pin_write <- function(board, x,
                      name = NULL,
                      type = NULL,
                      desc = NULL,
                      metadata = NULL,
                      versioned = FALSE) {

  if (is.null(type)) {
    type <- guess_type(type)
    inform(paste0("Guessing `type = '", type, "'`"))
  }
  if (is.null(name)) {
    name <- pin_default_name(expr_deparse(enexpr(x)), board)
    inform(paste0("Guessing `name = '", name, "'`"))
  }

  path <- object_save(x, tempfile(), type = type)
  metadata <- modifyList(metadata, object_meta(x))
  metadata <- modifyList(metadata, standard_meta(path, type, desc))

  board_pin_upload(board, name, path, metadata, versioned = versioned)

  invisible(board)
}

upload_inform <- function(type, name, version = NULL) {
  type <- arg_match0(type, c("unchanged", "create", "replace", "versioned"))

  switch(type,
    unchanged = inform("Existing pin unchanged"),
    versioned = inform(paste0("Created version ", version)),
    replace = inform("Replaced existing pin"),
    create = inform("Created new pin"),
  )
}

guess_type <- function(x) {
  if (is.data.frame(x)) {
    check_installed("arrow")
    "arrow"
  } else if (is_bare_list(x)) {
    "json"
  } else {
    "rds"
  }
}

# Should be a generic?
object_meta <- function(x) {
  if (is.data.frame(x)) {
    list(row = nrow(x), cols = ncol(x))
  } else {
    list()
  }
}

standard_meta <- function(path, type, desc = NULL) {
  list(
    type = type,
    descripton = desc,
    date = Sys.time(),
    file_size = as.integer(fs::file_size(path)),
    file_hash = digest::digest(path, "xxhash64", file = TRUE)
  )
}

object_save <- function(x, path, type = "rds") {
  type <- arg_match0(type, c("rds", "json", "arrow", "pickle", "csv"))

  switch(type,
    rds = saveRDS(x, path, version = 2),
    json = jsonlite::write_json(x, path, auto_unbox = TRUE),
    arrow = arrow::write_feather(x, path),
    pickle = abort("'pickle' pins not supported in R"),
    csv = write.csv(x, path, row.names = FALSE)
  )

  path
}

object_load <- function(path, type) {
  type <- arg_match0(type, c("rds", "json", "arrow", "pickle", "csv"))

  switch(type,
    rds = readRDS(path),
    json = jsonlite::read_json(path),
    arrow = arrow::read_feather(path),
    pickle = abort("'pickle' pins not supported in R"),
    csv = read.csv(path, stringsAsFactors = TRUE)
  )
}

is_prefix <- function(prefix, string) {
  substr(string, 1, nchar(prefix)) == prefix
}
