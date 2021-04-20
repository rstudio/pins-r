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
#'   a specific version of a pin. See `vignette("versioning)"` for details.
#' @param hash Specify a hash to verify that you get exactly the dataset that
#'   you expect. You can find the hash of an existing pin by looking for
#'   `pin_hash` in [pin_meta()].
#' @param ... Additional arguments passed on to methods for a specific board.
#' @export
#' @examples
#' b <- board_temp()
#'
#' b %>% pin_write(mtcars, "mtcars")
#' b
#'
#' b %>% pin_meta("mtcars")
#' b %>% pin_read("mtcars")
pin_read <- function(board, name, version = NULL, hash = NULL, ...) {
  pin <- pin_retrieve(board, name, version = version, hash = hash, ...)
  object_read(pin$path, pin$meta)
}

pin_retrieve <- function(board, name, version = NULL, hash = NULL, ...) {
  check_board(board)
  check_name(name)
  ellipsis::check_dots_used()

  pin <- board_pin_download(board, name, version = version, ...)

  if (!is.null(hash)) {
    pin_hash <- pin_hash(pin$path)
    if (!is_prefix(hash, pin_hash)) {
      abort(paste0(
        "Specified hash '", hash, "' doesn't match pin hash '", pin_hash, "'"
      ))
    }
  }

  pin
}

#' @param x An object (typically a data frame) to pin.
#' @param desc A text description of the pin; most important for
#'   shared boards so that others can understand what the pin contains.
#' @param metadata A list containing additional metadata to store with the pin.
#'   When retrieving the pin, this will be stored in the `user` key, to
#'   avoid potential clashes with the metadata that pins itself uses.
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
    pins_inform(paste0("Guessing `name = '", name, "'`"))
  } else {
    check_name(name)
  }
  check_metadata(metadata)

  if (is.null(type)) {
    type <- guess_type(x)
    pins_inform(paste0("Guessing `type = '", type, "'`"))
  }

  path <- object_write(x, fs::path_temp(fs::path_ext_set(name, type)), type = type)
  meta <- path_meta(path, object = x, type = type, desc = desc, user = metadata)

  board_pin_upload(board, name, path, meta, versioned = versioned, x = x)

  invisible(board)
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

object_write <- function(x, path, type = "rds") {
  type <- arg_match0(type, c("rds", "json", "arrow", "pickle", "csv"))

  switch(type,
    rds = write_rds(x, path),
    json = jsonlite::write_json(x, path, auto_unbox = TRUE),
    arrow = arrow::write_feather(x, path),
    pickle = abort("'pickle' pins not supported in R"),
    csv = utils::write.csv(x, path, row.names = FALSE)
  )

  path
}

write_rds <- function(x, path) {
  if (!is_testing()) {
    saveRDS(x, path, version = 2)
  } else {
    # compression algorithm changed in 4.1
    saveRDS(x, path, version = 2, compress = FALSE)

    old <- readBin(path, "raw", fs::file_size(path))

    # Record fixed R version number (3.5.3) to avoid spurious hash changes
    con <- file(path, open = "wb")
    writeBin(old[1:7], con)
    writeBin(as.raw(c(3, 5, 3)), con)
    writeBin(old[-(1:10)], con)
    close(con)
  }
  invisible(path)
}

object_read <- function(path, meta) {
  if (meta$api_version == 1) {
    type <- arg_match0(meta$type, c("rds", "json", "arrow", "pickle", "csv", "file"))

    switch(type,
      rds = readRDS(path),
      json = jsonlite::read_json(path, simplifyVector = TRUE),
      arrow = arrow::read_feather(path),
      pickle = abort("'pickle' pins not supported in R"),
      csv = utils::read.csv(path, stringsAsFactors = TRUE),
      file = abort(c(
        "Pin created with `pin_upload()`",
        i = "Retrieve uploaded paths with `pin_download()`"
      ))
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

pin_hash <- function(paths) {
  if (length(paths) == 1) {
    hash_file(paths)
  } else {
    hashes <- map_chr(paths, hash_file)
    hash(hashes)
  }
}

hash_file <- function(path) {
  digest::digest(file = path, algo = "xxhash64")
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

  if (grepl("\\\\|/", x, perl = TRUE)) {
    abort("`name` can not contain slashes")
  }
}
check_metadata <- function(x) {
  if (!is.null(x) && !is_bare_list(x)) {
    abort("`metadata` must be a list")
  }
}
