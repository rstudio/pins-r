pin_versions_path <- function(storage_path) {
  hash_files <- dir(storage_path, full.names = TRUE)
  hash_files <- hash_files[!grepl("/_versions$", hash_files)]

  all_sha1 <- sapply(hash_files, function(x) digest::digest(x, algo = "sha1", file = TRUE))
  signature <- paste(paste(all_sha1, collapse = ","), paste(dir(storage_path), collapse = ","), sep = ",")

  version <- digest::digest(signature, algo = "sha1", file = FALSE)

  normalizePath(file.path(storage_path, getOption("pins.versions.path", "_versions"), version), mustWork = FALSE)
}

board_versions_enabled <- function(board) {
  identical(board$versions, TRUE)
}

board_versions_create <- function(board, name, path) {
  versions <- NULL

  if (board_versions_enabled(board)) {
    # read the versions from the non-versioned manifest
    component_path <- pin_storage_path(component = board$name, name = name)
    component_manifest <- pin_manifest_get(component_path)
    versions <- component_manifest$versions

    version_path <- pin_versions_path(path)
    version_relative <- pin_registry_relative(version_path, base_path = path)

    if (any(component_manifest$versions == version_relative)) {
      if (dir.exists(version_path)) unlink(version_path, recursive = TRUE)
      versions <- versions[versions != version_relative]
    }

    if (dir.exists(version_path)) unlink(version_path, recursive = TRUE)
    dir.create(version_path, recursive = TRUE)

    files <- dir(path, full.names = TRUE)
    files <- files[files != file.path(path, "_versions")]
    file.copy(files, version_path, recursive = TRUE)

    versions <- c(versions, list(version_relative))

    manifest <- pin_manifest_get(path)
    manifest$versions <- versions
    pin_manifest_update(path, manifest)
  }

  versions
}

board_versions_get <- function(board, name) {
  versions <- data.frame(versions = character(0), stringsAsFactors = FALSE)

  component_path <- pin_storage_path(component = board$name, name = name)
  manifest <- pin_manifest_get(component_path)

  versions <- manifest$versions
  if (length(versions) > 0) {
    versions <- data.frame(versions = versions, stringsAsFactors = FALSE)
  }

  versions
}

board_versions_shorten <- function(versions) {
  paths <- gsub("[^/]+$", "", versions)
  if (length(unique(paths))) {
    versions <- gsub(".*/", "", versions)
  }

  shortened <- substr(versions, 1, 7)
  if (length(unique(shortened)) == length(versions)) {
    versions <- shortened
  }

  versions
}

board_versions_expand <- function(versions, version) {
  shortened <- board_versions_shorten(versions)

  version_index <- which(shortened == version)

  if (length(version_index) == 0) {
    stop("Version '", version, "' is not valid, please select from pin_versions().")
  }

  versions[version_index]
}
