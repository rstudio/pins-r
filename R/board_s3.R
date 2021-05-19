#' @examples
#' board_s3("pins-test-hadley", region = "us-east-2")
board_s3 <- function(
                    bucket,
                    access_key = NULL,
                    secret_access_key = NULL,
                    session_token = NULL,
                    credential_expiration = NULL,
                    profile = NULL,
                    region = NULL,
                    endpoint = NULL,
                    cache = NULL) {

  check_installed("paws.storage")

  config <- compact(list(
    credentials = compact(list(
      creds = compact(list(
        access_key_id = access_key,
        secret_access_key = secret_access_key,
        session_token = session_token
      )),
      profile = profile
    )),
    endpoint = endpoint,
    region = region
  ))
  svc <- paws.storage::s3(config = config)

  # Check that have access to the bucket
  svc$head_bucket(bucket)

  cache <- cache %||% board_cache_path(paste0("s3-", bucket))
  new_board("pins_board_s3",
    name = "s3",
    api = 1,
    bucket = bucket,
    svc = svc,
    cache = cache
  )
}

#' @export
pin_list.pins_board_s3 <- function(board, ...) {
  # TODO: implement pagination
  resp <- board$svc$list_objects_v2(board$bucket, Delimiter = "/")

  # TODO: check that directory contains versions.yml
  prefixes <- map_chr(resp$CommonPrefixes, ~ .x$Prefix)
  sub("/$", "", prefixes)
}

#' @export
pin_delete.pins_board_s3 <- function(board, names, ...) {
  for (name in names) {
    board$svc$delete_object(board$bucket, name)
  }
  invisible(board)
}

#' @export
pin_versions.pins_board_s3 <- function(board, name, ...) {
  path_pin <- fs::path(board$cache, name)
  fs::dir_create(path_pin)

  tryCatch({
    path <- s3_download(board, fs::path(name, "versions.yml"))
    yaml <- read_cache(path)
    tibble::as_tibble(yaml$versions)
  }, http_404 = function(err) {
    tibble::tibble(version = character(), created = .POSIXct(double()))
  })

}

#' @export
pin_meta.pins_board_s3 <- function(board, name, version = NULL, ...) {
  version <- version %||%
    last(pin_versions(board, name)$version) %||%
    abort("No versions found")

  # TODO: error messages for bad name/bad version
  path_version <- fs::path(board$cache, name, version)
  fs::dir_create(path_version)

  s3_download(board, fs::path(name, version, "data.txt"))
  local_meta(
    read_meta(fs::path(board$cache, name, version)),
    dir = path_version,
    version = version
  )
}

#' @export
pin_fetch.pins_board_s3 <- function(board, name, version = NULL, ...) {
  meta <- pin_meta(board, name, version = version)
  for (file in meta$file) {
    s3_download(board, fs::path(name, meta$local$version, file))
  }

  meta
}

#' @export
pin_store.pins_board_s3 <- function(board, name, path, metadata,
                                    versioned = NULL, ...) {
  check_name(name)

  ver <- record_version(board, name, metadata, versioned = versioned)
  if (ver$delete) {
    meta <- pin_meta(board, name)
    keys <- fs::path(name, meta$local$version, c(meta$file, "data.txt"))
    delete <- list(Objects = map(keys, ~ list(Key = .x)))
    board$svc$delete_objects(board$bucket, Delete = delete)
  }

  path_s3 <- fs::path(name, metadata$pin_hash)
  s3_upload_yaml(board, fs::path(path_s3, "data.txt"), metadata)
  for (file in path) {
    s3_upload_file(board, fs::path(path_s3, fs::path_file(file)), file)
  }

  # Add to list of versions so we know which is most recent
  s3_upload_yaml(board, fs::path(name, "versions.yml"), list(versions = ver$versions))

  invisible(board)
}

record_version <- function(board, name, metadata, versioned = NULL) {
  versions <- pin_versions(board, name)
  if (nrow(versions) > 1) {
    versioned <- versioned %||% TRUE
  } else {
    versioned <- versioned %||% board$versions
  }

  if (!versioned) {
    if (nrow(versions) == 0) {
      pins_inform(paste0("Creating new version '", metadata$pin_hash, "'"))
    } else if (nrow(versions) == 1) {
      pins_inform(paste0(
        "Replacing version '", versions$version, "'",
        " with '", metadata$pin_hash, "'"
      ))
    } else {
      abort(c(
        "Pin is versioned, but you have requested a write without versions",
        i = "To un-version a pin, you must delete it"
      ))
    }
  } else {
    pins_inform(paste0("Creating new version '", metadata$pin_hash, "'"))
  }

  delete <- !versioned && nrow(versions) == 1

  new_row <- data.frame(
    version = metadata$pin_hash,
    created = metadata$created
  )
  versions <- rbind(if (!delete) versions, new_row)

  list(
    delete = delete,
    versions = versions
  )
}

# Helpers -----------------------------------------------------------------

s3_upload_yaml <- function(board, key, yaml) {
  body <- charToRaw(yaml::as.yaml(yaml))
  board$svc$put_object(Bucket = board$bucket, Body = body, Key = key)
}

s3_upload_file <- function(board, key, path) {
  body <- brio::read_file_raw(path)
  board$svc$put_object(Bucket = board$bucket, Body = body, Key = key)
}

s3_download <- function(board, key, use_cache_on_failure = FALSE) {
  cache_path <- download_cache_path(board$cache)
  cache <- read_cache(cache_path)[[key]]

  if (!is.null(cache)) {
    # https://github.com/paws-r/paws/issues/418
    return(fs::path(board$cache, key))

    if_modified_since <- http_date(cache$modified)
    if_none_match <- cache$etag
  } else {
    if_modified_since <- NULL
    if_none_match <- NULL
  }

  req <- tryCatch(
    resp <- board$svc$get_object(
      Bucket = board$bucket,
      Key = key,
      IfModifiedSince = if_modified_since,
      IfNoneMatch = if_none_match
    ),
    error = function(e) {
      if (!is.null(cache) && use_cache_on_failure) {
        NULL
      } else {
        stop(e)
      }
    }
  )

  if (is.null(resp)) {
    warn(glue::glue("Downloading '{key}' failed; falling back to cached version"))
  } else if (!is.null(resp$Body)) {
    signal("", "pins_cache_downloaded")
    writeBin(resp$Body, fs::path(board$cache, key))

    update_cache(cache_path, key, list(
      expires = if (length(resp$Expiration) == 0) NULL else resp$Expiration,
      etag = gsub('^"|"$', "", resp$ETag),
      modified = unclass(null_if_na(resp$LastModified))
    ))
  } else {
    signal("", "pins_cache_not_modified")
  }
  fs::path(board$cache, key)
}
