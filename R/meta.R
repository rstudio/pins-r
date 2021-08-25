# i/o ---------------------------------------------------------------------

read_meta <- function(path) {
  path <- fs::path(path, "data.txt")

  if (!fs::file_exists(path)) {
    return(list(api_version = 1L))
  }

  yaml <- yaml::read_yaml(path, eval.expr = FALSE)
  if (is.null(yaml$api_version)) {
    yaml$api_version <- 0L
  } else if (yaml$api_version == 1) {
    yaml$file_size <- fs::as_fs_bytes(yaml$file_size)
    yaml$created <- parse_8601_compact(yaml$created)
    yaml$user <- yaml$user %||% list()
  } else if (yaml$api_version > 1) {
    abort(c(
      paste0("Metadata requires pins ", yaml$api_version, ".0.0 or greater"),
      i = "Do you need to upgrade the pins package?"
    ))
  }

  yaml
}

write_meta <- function(x, path) {
  path <- fs::path(path, "data.txt")
  write_yaml(x, path)
}

# pin metadata ------------------------------------------------------------

standard_meta <- function(paths, type, object = NULL, title = NULL, description = NULL) {
  list(
    file = fs::path_file(paths),
    file_size = as.integer(fs::file_size(paths)),
    pin_hash = pin_hash(paths),
    type = type,
    title = title %||% default_title(object, paths),
    description = description,
    created = format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "UTC"),
    api_version = 1
  )
}

as_8601_compact <- function(x = Sys.time()) {
  format(x, "%Y%m%dT%H%M%SZ", tz = "UTC")
}
parse_8601_compact <- function(x) {
  y <- as.POSIXct(strptime(x, "%Y%m%dT%H%M", tz = "UTC"))
  attr(y, "tzone") <- ""
  y
}

default_title <- function(object, path) {
  if (is.null(object)) {
    n <- length(path)
    if (n == 1) {
      desc <- glue(".{fs::path_ext(path)} file")
    } else {
      desc <- glue("{n} files")
    }
  } else if (is.data.frame(object)) {
    desc <- glue("{nrow(object)} x {ncol(object)} data frame")
  } else {
    desc <- friendly_type(object)
  }

  paste0("A pinned ", desc)
}
friendly_type <- function(x) {
  switch(typeof(x),
    logical = "logical vector",
    integer = "integer vector",
    numeric = ,
    double = "double vector",
    complex = "complex vector",
    character = "character vector",
    raw = "raw vector",
    list = "list",
    typeof(x)
  )
}
