board_manifest_get <- function(path) {
  yaml::read_yaml(path, eval.expr = FALSE)
}

board_manifest_load <- function(manifest) {
  yaml::yaml.load(manifest, eval.expr = FALSE)
}

board_manifest_create <- function(index, file) {
  yaml::write_yaml(index, file)
}
