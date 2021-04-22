# i/o ---------------------------------------------------------------------

read_meta <- function(path) {
  path <- fs::path(path, "data.txt")

  if (!fs::file_exists(path)) {
    return(list(api_version = 1L))
  }

  yaml <- yaml::read_yaml(path, eval.expr = FALSE)
  if (is.null(yaml$api_version)) {
    yaml$api_version <- 0L
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
  yaml::write_yaml(x, path)
}

# pin metadata ------------------------------------------------------------

path_meta <- function(path, type = NULL, object = NULL, user = NULL, desc = NULL) {
  meta <- list(
    file = fs::path_file(path),
    file_size = as.integer(fs::file_size(path)),
    pin_hash = pin_hash(path),
    type = type %||% "file",
    description = desc %||% default_description(object, path),
    created = format(Sys.time(), "%Y-%m-%dT%H:%M:%S", tz = "UTC"),
    api_version = 1
  )

  meta$user <- user
  meta
}

# description -------------------------------------------------------------

default_description <- function(object, path) {
  if (is.null(object)) {
    n <- length(path)
    if (n == 1) {
      desc <- glue("a .{fs::path_ext(path)} file")
    } else {
      desc <- glue("{n} files")
    }
  } else if (is.data.frame(object)) {
    desc <- glue("a data frame with {nrow(object)} rows and {ncol(object)} columns")
  } else {
    desc <- friendly_type(typeof(object))
  }

  paste0("A pin containing ", desc)
}

