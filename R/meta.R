read_meta <- function(path) {
  path <- fs::path(path, "data.txt")

  if (!fs::file_exists(path)) {
    return(list(api_version = 1))
  }

  yaml <- yaml::read_yaml(path, eval.expr = FALSE)
  if (is.null(yaml$api_version)) {
    yaml$api_version <- 0
    yaml$type <- yaml$type %||% "files"
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
