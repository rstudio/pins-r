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
#'   [board_url()] or other other `board_` function.
#' @param name Pin name.
#' @param version For boards that support versions, this allows you to retrieve
#'   a specific version of a pin. See `vignette("versioning)"` for details.
#' @param hash Specify a hash to verify that you get exactly the dataset that
#'   you expect. You can find the hash of an existing pin by looking for
#'   `pin_hash` in [pin_meta()].
#' @param ... Additional arguments passed on to methods for a specific board.
#' @return `pin_read()` returns an R object read from the pin;
#'   `pin_write()` returns the fully qualified name of the new pin, invisibly.
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
  ellipsis::check_dots_used()
  check_board(board, "pin_read()", "pin_get()")

  meta <- pin_fetch(board, name, version = version, ...)
  check_hash(meta, hash)

  object_read(meta)
}

#' @param x An object (typically a data frame) to pin.
#' @param desc A text description of the pin; most important for
#'   shared boards so that others can understand what the pin contains.
#'   If omitted, pins will generate a brief description of the contents.
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
                      versioned = NULL,
                      ...) {
  ellipsis::check_dots_used()
  check_board(board, "pin_write()", "pin()")

  if (is.null(name)) {
    name <- enexpr(x)
    if (is_symbol(name)) {
      name <- as.character(name)
      pins_inform("Using `name = '{name}'`")
    } else {
      abort("Must supply `name` when `x` is an expression")
    }
  }
  check_metadata(metadata)
  if (!is_string(name)) {
    abort("`name` must be a string")
  }

  if (is.null(type)) {
    type <- guess_type(x)
    pins_inform("Guessing `type = '{type}'`")
  }

  filename <- fs::path_ext_set(fs::path_file(name), type)
  path <- object_write(x, fs::path_temp(filename), type = type)
  meta <- standard_meta(path, object = x, type = type, desc = desc)
  meta$user <- metadata

  invisible(pin_store(board, name, path, meta, versioned = versioned, x = x, ...))
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

object_types <- c("rds", "json", "arrow", "pickle", "csv", "file")

object_read <- function(meta) {
  path <- fs::path(meta$local$dir, meta$file)
  missing <- !fs::file_exists(path)
  if (any(missing)) {
    abort(c("Cache failure. Missing files:", path[!missing]))
  }

  if (meta$api_version == 1) {
    type <- arg_match0(meta$type, object_types)

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
    # used by board_rsconnect()
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

check_board <- function(x, v1, v0) {
  if (!inherits(x, "pins_board")) {
    abort("`board` must be a pin board")
  }

  if (!1 %in% x$api) {
    this_not_that(v0, v1)
  }
}
check_name <- function(x) {
  if (grepl("\\\\|/", x, perl = TRUE)) {
    abort("`name` can not contain slashes")
  }
}
check_metadata <- function(x) {
  if (!is.null(x) && !is_bare_list(x)) {
    abort("`metadata` must be a list")
  }
}
check_hash <- function(meta, hash) {
  if (is.null(hash)) {
    return()
  }

  pin_hash <- pin_hash(fs::path(meta$local$dir, meta$file))
  if (!is_prefix(hash, pin_hash)) {
    abort(paste0(
      "Specified hash '", hash, "' doesn't match pin hash '", pin_hash, "'"
    ))
  }
}
