#' Use an S3 bucket as a board
#'
#' Pin data to a bucket on Amazon's S3 service.
#'
#' # Authentication
#'
#' `board_s3()` is powered by the paws package which provides a wide range
#' of authnetication options, as documented at
#' <https://github.com/paws-r/paws/blob/main/docs/credentials.md>.
#' In brief, there are four main options that are tried in order:
#'
#' * The `access_key` and `secret_access_key` arguments to this function.
#'   If you have a temporary session token, you'll also need to supply
#'   `session_token` and `credential_expiration`.
#'   (Not recommended since your `secret_access_key` will be recorded
#'   in `.Rhistory`)
#'
#' * The `AWS_ACCESS_KEY_ID` and `AWS_SECRET_ACCESS_KEY` env vars.
#'   (And `AWS_SESSION_TOKEN` and `AWS_CREDENTIAL_EXPIRATION` env vars if you
#'   have a temporary session token)
#'
#' * The AWS shared credential file, `~/.aws/credentials`:
#'
#'     ```
#'     [profile-name]
#'     aws_access_key_id=your AWS access key
#'     aws_secret_access_key=your AWS secret key
#'     ```
#'
#'     The "default" profile will be used if you don't supply the access key
#'     and secret access key as described above. Otherwise you can use the
#'     `profile` argument to use a profile of your choice.
#'
#' * Automatic authentication from EC2 instance or container IAM role.
#'
#' See the paws documentation for more unusual options including getting
#' credentials from a command line process, picking a role when running inside
#' an EC2 instance, using a role from another profile, and using multifactor
#' authentication.
#'
#' # Caveats
#'
#' * If you point at a bucket that's not created by pins, some functions
#'   like `pins_list()` will work, but won't return useful output.
#'
#' @inheritParams new_board
#' @param bucket Bucket name.
#' @param access_key,secret_access_key,session_token,credential_expiration
#'   Manually control authentication. See documentation below for details.
#' @param region AWS region. If not specified, will be read from `AWS_REGION`,
#'   or AWS config file.
#' @param endpoint AWS endpoint to use; usually generated automatically from
#'   `region`.
#' @param profile Role to use from AWS shared credentials/config file.
#' @export
#' @examples
#' \dontrun{
#' board <- board_s3("pins-test-hadley", region = "us-east-2")
#' board %>% pin_write(mtcars)
#' board %>% pin_read("mtcars")
#' }
board_s3 <- function(
                    bucket,
                    versioned = TRUE,
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
  new_board_v1("pins_board_s3",
    name = "s3",
    bucket = bucket,
    svc = svc,
    cache = cache,
    versioned = versioned
  )
}

board_s3_test <- function(...) {
  if (Sys.info()[["user"]] != "hadley" || !has_envvars("AWS_ACCESS_KEY_ID")) {
    testthat::skip("S3 tests only work for Hadley")
  }

  board_s3("pins-test-hadley", region = "us-east-2", cache = tempfile(), ...)
}

#' @export
pin_list.pins_board_s3 <- function(board, ...) {
  # TODO: implement pagination
  resp <- board$svc$list_objects_v2(board$bucket, Delimiter = "/")

  prefixes <- map_chr(resp$CommonPrefixes, ~ .x$Prefix)
  sub("/$", "", prefixes)
}

#' @export
pin_exists.pins_board_s3 <- function(board, name, ...) {
  resp <- board$svc$list_objects_v2(board$bucket, Prefix = paste0(name, "/"))
  resp$KeyCount > 0
}

#' @export
pin_delete.pins_board_s3 <- function(board, names, ...) {
  for (name in names) {
    s3_delete_dir(board, name)
  }
  invisible(board)
}

#' @export
pin_versions.pins_board_s3 <- function(board, name, ...) {
  check_pin_exists(board, name)

  path_pin <- fs::path(board$cache, name)
  fs::dir_create(path_pin)

  path <- s3_download(board, fs::path(name, "versions.yml"))
  yaml <- read_cache(path)
  tibble::as_tibble(yaml$versions)
}

#' @export
pin_meta.pins_board_s3 <- function(board, name, version = NULL, ...) {
  check_pin_exists(board, name)

  if (is.null(version)) {
    version <- last(pin_versions(board, name)$version) %||% abort("No versions found")
  } else if (is_string(version)) {
    # check_pin_version(board, name, version)
  } else {
    abort("`version` must be a string or `NULL`")
  }

  path_version <- fs::path(board$cache, name, version)
  fs::dir_create(path_version)

  s3_download(board, fs::path(name, version, "data.txt"), immutable = TRUE)
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
    key <- fs::path(name, meta$local$version, file)
    s3_download(board, key, immutable = TRUE)
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
    s3_delete_dir(board, fs::path(name, meta$local$version))
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
  if (pin_exists(board, name)) {
    versions <- pin_versions(board, name)
  } else {
    versions <- data.frame(version = character(), created = .POSIXct(double()))
  }

  if (nrow(versions) > 1) {
    versioned <- versioned %||% TRUE
  } else {
    versioned <- versioned %||% board$versioned
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

s3_delete_dir <- function(board, dir) {
  resp <- board$svc$list_objects_v2(board$bucket, Prefix = paste0(dir, "/"))
  if (resp$KeyCount == 0) {
    return(invisible())
  }

  delete <- list(Objects = map(resp$Contents, "[", "Key"))
  board$svc$delete_objects(board$bucket, Delete = delete)
  invisible()
}

s3_upload_yaml <- function(board, key, yaml) {
  body <- charToRaw(yaml::as.yaml(yaml))
  board$svc$put_object(Bucket = board$bucket, Body = body, Key = key)
}

s3_upload_file <- function(board, key, path) {
  body <- readBin(path, "raw", file.size(path))
  board$svc$put_object(Bucket = board$bucket, Body = body, Key = key)
}

s3_download <- function(board, key, immutable = FALSE) {
  path <- fs::path(board$cache, key)

  if (!immutable || !fs::file_exists(path)) {
    resp <- board$svc$get_object(Bucket = board$bucket, Key = key)
    writeBin(resp$Body, path)
  }

  path
}

