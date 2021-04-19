# i/o ---------------------------------------------------------------------

read_meta <- function(path) {
  path <- fs::path(path, "data.txt")

  if (!fs::file_exists(path)) {
    return(list(api_version = 1))
  }

  yaml <- yaml::read_yaml(path, eval.expr = FALSE)
  if (is.null(yaml$api_version)) {
    yaml$api_version <- 0
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
    description = desc,
    date = format(Sys.time(), "%Y-%m-%dT%H:%M:%S", tz = "UTC"),
    api_version = 1
  )

  meta$user <- user
  if (!is.null(object)) {
    meta$object <- object_meta(object)

  }
  meta
}

object_meta <- function(object) {
  if (is.data.frame(object)) {
    list(rows = nrow(object), cols = ncol(object))
  } else {
    list()
  }
}
