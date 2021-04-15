#' Read and write pins
#'
#' Use `pin_write()` to pin an object to board, and `pin_read()` to retrieve
#' it.
#'
#' `pin_write()` takes care of the details of serialising an R object to
#' disk, controlled by the `type` argument. See [pin_download()]/[pin_upload()]
#' if you want to perform the serialisation yourself and work just with files.
#'
#' @param board A pin board, created by [board_folder()], [board_rsconnect()],
#'   [board_github()] or other other `board_` function.
#' @param name Pin name.
#' @param version For boards that support versions, this allows you to retrieve
#'   a specific version of a pin.
#' @param hash Specify a hash to verify that you get exactly the dataset that
#'   you expect. You can find the hash of an existing pin by inspecting the
#'   pin metadata.
#' @export
#' @examples
#' b <- board_temp()
#'
#' b %>% pin_write(mtcars, "mtcars")
#' b
#'
#' b %>% pin_read("mtcars")
pin_read <- function(board, name, version = NULL, hash = NULL) {
  check_board(board)
  check_name(name)

  pin <- board_pin_download(board, name, version = version)

  if (!is.null(hash)) {
    pin_hash <- hash_file(pin$path)
    if (!is_prefix(hash, pin_hash)) {
      abort(paste0(
        "Specified hash '", hash, "' doesn't match pin hash '", pin_hash, "'"
      ))
    }
  }

  object_load(pin$path, pin$meta)
}

#' @param x An object (typically a data frame) to pin.
#' @param desc A text description of the pin; most important for
#'   shared boards so that others can understand what the pin contains.
#' @param metadata A list containing additional metadata to store with the pin.
#' @param type File type used to save `x` to disk. Must be one of
#'   "csv", "rds", "json", or "arrow". If not supplied will use json for bare
#'   lists and rds for everything else.
#' @param versioned Should the pin be versioned? The default, `NULL`, will
#'   use the default for `board`
#' @rdname pin_read
#' @export
pin_write <- function(board, x,
                      name = NULL,
                      type = NULL,
                      desc = NULL,
                      metadata = NULL,
                      versioned = NULL) {

  check_board(board)
  if (is.null(name)) {
    name <- pin_default_name(expr_deparse(enexpr(x)), board)
    inform(paste0("Guessing `name = '", name, "'`"))
  } else {
    check_name(name)
  }

  if (is.null(type)) {
    type <- guess_type(x)
    inform(paste0("Guessing `type = '", type, "'`"))
  }

  path <- object_save(x, fs::path_temp(fs::path_ext_set(name, type)), type = type)
  metadata <- modifyList(metadata, object_meta(x))
  metadata <- modifyList(metadata, standard_meta(path, type, desc))

  board_pin_upload(board, name, path, metadata, versioned = versioned, x = x)

  invisible(board)
}

pins_inform <- function(...) {
  if (isTRUE(getOption("pins.quiet"))) {
    invisible()
  } else {
    inform(...)
  }
}

guess_type <- function(x) {
  if (is.data.frame(x)) {
    "rds"
    # Might consider switch to arrow in the future
  } else if (is_bare_list(x)) {
    "json"
  } else {
    "rds"
  }
}

# Should be a generic?
object_meta <- function(x) {
  if (is.data.frame(x)) {
    list(rows = nrow(x), cols = ncol(x))
  } else {
    list()
  }
}

standard_meta <- function(path, type, desc = NULL) {
  list(
    file = fs::path_file(path),
    type = type,
    descripton = desc,
    date = format(Sys.time(), "%Y-%m-%dT%H:%M:%S", tz = "UTC"),
    file_size = as.integer(fs::file_size(path)),
    file_hash = hash_file(path),
    api_version = 1
  )
}

object_save <- function(x, path, type = "rds") {
  type <- arg_match0(type, c("rds", "json", "arrow", "pickle", "csv"))

  switch(type,
    rds = saveRDS(x, path, version = 2),
    json = jsonlite::write_json(x, path, auto_unbox = TRUE),
    arrow = arrow::write_feather(x, path),
    pickle = abort("'pickle' pins not supported in R"),
    csv = utils::write.csv(x, path, row.names = FALSE)
  )

  path
}

object_load <- function(path, meta) {
  if (meta$api_version == 1) {
    type <- arg_match0(meta$type, c("rds", "json", "arrow", "pickle", "csv"))

    switch(type,
      rds = readRDS(path),
      json = jsonlite::read_json(path, simplifyVector = TRUE),
      arrow = arrow::read_feather(path),
      pickle = abort("'pickle' pins not supported in R"),
      csv = utils::read.csv(path, stringsAsFactors = TRUE)
    )
  } else {
    type <- arg_match0(meta$type, c("default", "files", "table"))
    path <- fs::path_dir(path[[1]])

    switch(type,
      default = pin_load.default(path),
      table = pin_load.table(path),
      files = pin_load.files(path)
    )
  }
}

is_prefix <- function(prefix, string) {
  substr(string, 1, nchar(prefix)) == prefix
}

hash_file <- function(path) {
  digest::digest(path, file = TRUE, algo = "xxhash64")
}

check_board <- function(x) {
  if (!inherits(x, "pins_board")) {
    abort("`board` must be a pin board")
  }
}
check_name <- function(x) {
  if (!is_string(x)) {
    abort("`name` must be a string")
  }
}
