pin_versions_path <- function(component, name) {
  storage_path <- pin_storage_path(component = component, name = name)

  all_sha1 <- sapply(dir(storage_path, full.names = TRUE), function(x) digest::digest(x, algo = "sha1", file = TRUE))
  signature <- paste(paste(all_sha1, collapse = ","), paste(dir(storage_path), collapse = ","), sep = ",")

  version <- digest::digest(signature, algo = "sha1", file = FALSE)

  normalizePath(file.path(storage_path, "..", "_versions", name, version), mustWork = FALSE)
}

board_versions_enabled <- function(board) {
  identical(board$versions, TRUE)
}
