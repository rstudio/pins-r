pin_versions_path <- function(component, name) {
  storage_path <- pin_storage_path(component = component, name = name)

  hash_files <- dir(storage_path, full.names = TRUE)
  hash_files <- hash_files[!grepl("/_versions$", hash_files)]

  all_sha1 <- sapply(hash_files, function(x) digest::digest(x, algo = "sha1", file = TRUE))
  signature <- paste(paste(all_sha1, collapse = ","), paste(dir(storage_path), collapse = ","), sep = ",")

  version <- digest::digest(signature, algo = "sha1", file = FALSE)

  normalizePath(file.path(storage_path, getOption("pins.versions.path", "_versions"), version), mustWork = FALSE)
}

board_versions_enabled <- function(board) {
  !identical(board$versions, FALSE)
}

board_versions_create <- function(board, name, path) {
  versions <- NULL
  if (board_versions_enabled(board)) {
    version_path <- pin_versions_path(component = board$name, name = name)
    version_relative <- pin_registry_relative(version_path, component = board$name)
    entries <- pin_registry_retrieve_maybe(name = name, component = board$name)
    versions <- entries$versions

    if (any(entries$versions == version_relative)) {
      if (dir.exists(version_path)) unlink(version_path, recursive = TRUE)
      versions <- versions[versions != version_relative]
    }

    if (dir.exists(version_path)) unlink(version_path, recursive = TRUE)
    dir.create(version_path, recursive = TRUE)

    file.copy(dir(path, full.names = TRUE), version_path, recursive = TRUE)

    versions <- c(versions, list(version_relative))
  }

  versions
}

board_versions_get <- function(board, name) {
  versions <- data.frame(versions = character(0), stringsAsFactors = FALSE)

  entries <- pin_registry_retrieve_maybe(name = name, component = board$name)
  versions <- entries$versions

  if (length(versions) > 0) {
    versions <- data.frame(versions = versions, stringsAsFactors = FALSE)
  }

  versions
}
