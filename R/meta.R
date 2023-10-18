# i/o ---------------------------------------------------------------------

read_meta <- function(path) {
  path <- fs::path(path, "data.txt")

  if (!fs::file_exists(path)) {
    return(list(api_version = 1L))
  }

  yaml <- yaml::read_yaml(path, eval.expr = FALSE)
  if (is.null(yaml$api_version)) {
    yaml$api_version <- 0L
    yaml$file <- yaml$path %||% yaml$file
  } else if (yaml$api_version == 1) {
    yaml$file_size <- fs::as_fs_bytes(yaml$file_size)
    yaml$created <- parse_8601_compact(yaml$created)
    yaml$user <- yaml$user %||% list()
  } else if (yaml$api_version > 1) {
    cli::cli_abort(c(
      "Metadata requires pins {yaml$api_version}.0.0 or greater",
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

standard_meta <- function(paths,
                          type,
                          title = NULL,
                          description = NULL,
                          tags = NULL,
                          urls = NULL) {
  list(
    file = fs::path_file(paths),
    file_size = as.integer(fs::file_size(paths)),
    pin_hash = pin_hash(paths),
    type = type,
    title = title,
    description = description,
    tags = if (is.null(tags)) tags else as.list(tags),
    urls = if (is.null(urls)) urls else as.list(urls),
    created = format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "UTC"),
    api_version = 1L
  )
}

as_8601_compact <- function(x = Sys.time()) {
  format(x, "%Y%m%dT%H%M%SZ", tz = "UTC")
}
parse_8601_compact <- function(x) {
  y <- as.POSIXct(strptime(x, "%Y%m%dT%H%M%S", tz = "UTC"))
  attr(y, "tzone") <- ""
  y
}

parse_8601 <- function(x) {
  y <- as.POSIXct(strptime(x, "%Y-%m-%dT%H:%M", tz = "UTC"))
  attr(y, "tzone") <- ""
  y
}

default_title <- function(name, data = NULL, path = NULL) {
  if (!xor(is.null(data), is.null(path))) {
    abort("Must supply exactly one of `path` and `data`")
  }

  if (is.null(data)) {
    n <- length(path)
    if (n == 1) {
      desc <- glue("a pinned .{fs::path_ext(path)} file")
    } else {
      desc <- glue("{n} pinned files")
    }
  } else if (is.data.frame(data)) {
    desc <- glue("a pinned {nrow(data)} x {ncol(data)} data frame")
  } else {
    desc <- paste0("a pinned ", friendly_type(data))
  }

  paste0(name, ": ", desc)
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
