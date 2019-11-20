board_manifest_get <- function(path, default_empty = FALSE) {
  if (!file.exists(path) && default_empty) return(list())
  suppressWarnings(yaml::read_yaml(path, eval.expr = FALSE))
}

board_manifest_load <- function(manifest) {
  suppressWarnings(yaml::yaml.load(manifest, eval.expr = FALSE))
}

board_manifest_create <- function(index, file) {
  yaml::write_yaml(index, file)
}
