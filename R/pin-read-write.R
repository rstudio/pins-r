#' Read and write objects to and from a board
#'
#' Use `pin_write()` to pin an object to board, and `pin_read()` to retrieve
#' it.
#'
#' `pin_write()` takes care of the details of serialising an R object to
#' disk, controlled by the `type` argument. See [pin_download()]/[pin_upload()]
#' if you want to perform the serialisation yourself and work just with files.
#'
#' @param board A pin board, created by [board_folder()], [board_connect()],
#'   [board_url()] or another `board_` function.
#' @param name Pin name.
#' @param version Retrieve a specific version of a pin. Use [pin_versions()] to
#'   find out which versions are available and when they were created.
#' @param hash Specify a hash to verify that you get exactly the dataset that
#'   you expect. You can find the hash of an existing pin by looking for
#'   `pin_hash` in [pin_meta()].
#' @param ... Additional arguments passed on to methods for a specific board.
#' @return `pin_read()` returns an R object read from the pin;
#'   `pin_write()` returns the fully qualified name of the new pin, invisibly.
#' @export
#' @examples
#' b <- board_temp(versioned = TRUE)
#'
#' b %>% pin_write(1:10, "x", description = "10 numbers")
#' b
#'
#' b %>% pin_meta("x")
#' b %>% pin_read("x")
#'
#' # Add a new version
#' b %>% pin_write(2:11, "x")
#' b %>% pin_read("x")
#'
#' # Retrieve an older version
#' b %>% pin_versions("x")
#' b %>% pin_read("x", version = .Last.value$version[[1]])
#' # (Normally you'd specify the version with a string, but since the
#' # version includes the date-time I can't do that in an example)
pin_read <- function(board, name, version = NULL, hash = NULL, ...) {
  ellipsis::check_dots_used()
  check_board(board, "pin_read()", "pin_get()")

  meta <- pin_fetch(board, name, version = version, ...)
  check_hash(meta, hash)

  object_read(meta)
}

#' @param x An object (typically a data frame) to pin.
#' @param title A title for the pin; most important for shared boards so that
#'   others can understand what the pin contains. If omitted, a brief
#'   description of the contents will be automatically generated.
#' @param description A detailed description of the pin contents.
#' @param metadata A list containing additional metadata to store with the pin.
#'   When retrieving the pin, this will be stored in the `user` key, to
#'   avoid potential clashes with the metadata that pins itself uses.
#' @param type File type used to save `x` to disk. Must be one of
#'   "csv", "json", "rds", "arrow", or "qs". If not supplied, will use JSON for
#'   bare lists and RDS for everything else. Be aware that CSV and JSON are
#'   plain text formats, while RDS, Arrow, and
#'   [qs](https://CRAN.R-project.org/package=qs) are binary formats.
#' @param versioned Should the pin be versioned? The default, `NULL`, will
#'   use the default for `board`
#' @param tags A character vector of tags for the pin; most important for
#'   discoverability on shared boards.
#' @rdname pin_read
#' @export
pin_write <- function(board, x,
                      name = NULL,
                      type = NULL,
                      title = NULL,
                      description = NULL,
                      metadata = NULL,
                      versioned = NULL,
                      tags = NULL,
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
  check_tags(tags)
  if (!is_string(name)) {
    abort("`name` must be a string")
  }

  if (is.null(type)) {
    type <- guess_type(x)
    pins_inform("Guessing `type = '{type}'`")
  }

  path <- fs::path_temp(fs::path_ext_set(fs::path_file(name), type))
  object_write(x, path, type = type)
  withr::defer(fs::file_delete(path))

  meta <- standard_meta(
    paths = path,
    type = type,
    title = title %||% default_title(name, data = x),
    description = description,
    tags = tags
  )
  meta$user <- metadata

  name <- pin_store(board, name, path, meta, versioned = versioned, x = x, ...)
  pins_inform("Writing to pin '{name}'")
  invisible(name)
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
  type <- arg_match0(type, setdiff(object_types, "file"))

  switch(type,
    rds = write_rds(x, path),
    json = jsonlite::write_json(x, path, auto_unbox = TRUE),
    arrow = write_arrow(x, path),
    pickle = abort("'pickle' pins not supported in R"),
    joblib = abort("'joblib' pins not supported in R"),
    csv = utils::write.csv(x, path, row.names = FALSE),
    qs = write_qs(x, path)
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

write_qs <- function(x, path) {
  check_installed("qs")
  qs::qsave(x, path)
  invisible(path)
}

write_arrow <- function(x, path) {
  check_installed("arrow")
  arrow::write_feather(x, path)
  invisible(path)
}

object_types <- c("rds", "json", "arrow", "pickle", "csv", "qs", "file")

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
      arrow = read_arrow(path),
      pickle = abort("'pickle' pins not supported in R"),
      joblib = abort("'joblib' pins not supported in R"),
      csv = utils::read.csv(path),
      qs = read_qs(path),
      file = abort(c(
        "Pin does not declare file type so can't be automatically read",
        i = "Retrieve uploaded paths with `pin_download()`"
      ))
    )
  } else {
    # used by board_connect()
    type <- arg_match0(meta$type, c("default", "files", "table"))
    path <- fs::path_dir(path[[1]])

    switch(type,
      default = pin_load.default(path),
      table = pin_load.table(path),
      files = pin_load.files(path)
    )
  }
}

read_qs <- function(path) {
  check_installed("qs")
  qs::qread(path, strict = TRUE)
}

read_arrow <- function(path) {
  check_installed("arrow")
  arrow::read_feather(path)
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
    abort("`name` must not contain slashes", class = "pins_check_name")
  }
}
check_metadata <- function(x) {
  if (!is.null(x) && !is_bare_list(x)) {
    abort("`metadata` must be a list.")
  }
}
check_tags <- function(x) {
  if (!is.null(x) && !is_character(x)) {
    abort("`tags` must be a character vector.")
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
