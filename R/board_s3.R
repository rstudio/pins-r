#' Use an S3 bucket as a board
#'
#' Pin data to an S3-compatible storage bucket, such as on Amazon's S3 service,
#' MinIO, or Digital Ocean, using the paws.storage package.
#'
#' # Authentication
#'
#' `board_s3()` is powered by the paws package which provides a wide range
#' of authentication options, as documented at
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
#' # Details
#'
#' * The functions in pins do not create a new bucket. You can create
#'   a new bucket from R with [paws][paws.storage::s3_create_bucket].
#' * Some functions like [pin_list()] will work for an S3 board, but don't
#'   return useful output.
#' * You can pass arguments for [paws.storage::s3_put_object] such as `Tagging`
#'   and `ServerSideEncryption` through the dots of `pin_write()`. (Note that
#'   these are separate from [pin_write()] arguments like `tags`.)
#' * You can use `board_s3()` with S3-compatible object storage on non-AWS
#'   platforms such as MinIO and Digital Ocean. For this type of object storage,
#'   use the full URL (including scheme like `https://`) of the storage endpoint.
#' * `board_s3()` is powered by the paws.storage package, which is a
#'   suggested dependency of pins (not required for pins in general). If
#'   you run into errors when deploying content to a server like
#'   <https://www.shinyapps.io> or [Connect](https://posit.co/products/enterprise/connect/),
#'   add `requireNamespace("paws.storage")` to your app or document for [automatic
#'   dependency discovery](https://docs.posit.co/connect/user/troubleshooting/#render-missing-r-package).
#'
#' @inheritParams new_board
#' @param bucket Bucket name. You can only write to an existing bucket.
#' @param prefix Prefix within this bucket that this board will occupy.
#'   You can use this to maintain multiple independent pin boards within
#'   a single S3 bucket. Will typically end with `/` to take advantage of
#'   S3's directory-like handling.
#' @param access_key,secret_access_key,session_token,credential_expiration
#'   Manually control authentication. See documentation below for details.
#' @param region AWS region. If not specified, will be read from `AWS_REGION`,
#'   or AWS config file.
#' @param endpoint Endpoint to use; usually generated automatically for AWS
#'   from `region`. For MinIO and Digital Ocean, use the full URL (including
#'   scheme like `https://`) of your S3-compatible storage endpoint.
#' @param profile Role to use from AWS shared credentials/config file.
#' @export
#' @examples
#' \dontrun{
#' board <- board_s3("pins-test-hadley", region = "us-east-2")
#' board %>% pin_write(mtcars)
#' board %>% pin_read("mtcars")
#'
#' # A prefix allows you to have multiple independent boards in the same pin.
#' board_sales <- board_s3("company-pins", prefix = "sales/")
#' board_marketing <- board_s3("company-pins", prefix = "marketing/")
#' # You can make the hierarchy arbitrarily deep.
#'
#' # Pass S3 arguments like `Tagging` through the dots of `pin_write`:
#' board %>% pin_write(mtcars, Tagging = "key1=value1&key2=value2")
#'
#' }
board_s3 <- function(
    bucket,
    prefix = NULL,
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
               prefix = prefix,
               svc = svc,
               cache = cache,
               versioned = versioned
  )
}

board_s3_test <- function(...) {
  skip_if_missing_envvars(
    tests = "board_s3()",
    envvars = c("PINS_AWS_ACCESS_KEY", "PINS_AWS_SECRET_ACCESS_KEY")
  )

  board_s3("pins-test-hadley",
           region = "us-east-2",
           cache = tempfile(),
           access_key = Sys.getenv("PINS_AWS_ACCESS_KEY"),
           secret_access_key = Sys.getenv("PINS_AWS_SECRET_ACCESS_KEY"),
           ...
  )
}

#' @export
pin_list.pins_board_s3 <- function(board, ...) {

  resp <- board$svc$list_objects_v2(
    Bucket = board$bucket,
    Prefix = board$prefix,
    Delimiter = "/"
  )

  final_list <- resp$CommonPrefixes

  while(!is_empty(resp$NextContinuationToken)) {
    resp <- board$svc$list_objects_v2(
      Bucket = board$bucket,
      Prefix = board$prefix,
      Delimiter = "/",
      ContinuationToken = resp$NextContinuationToken)
    final_list <- c(final_list, resp$CommonPrefixes)
  }

  prefixes <- map_chr(final_list, ~ .x$Prefix)
  strip_prefix(sub("/$", "", prefixes), board$prefix)
}

#' @export
pin_exists.pins_board_s3 <- function(board, name, ...) {
  s3_file_exists(board, paste0(name, "/"))
}

#' @export
pin_delete.pins_board_s3 <- function(board, names, ...) {
  for (name in names) {
    check_pin_exists(board, name)
    s3_delete_dir(board, name)
  }
  invisible(board)
}

#' @export
pin_versions.pins_board_s3 <- function(board, name, ...) {
  check_pin_exists(board, name)

  resp <- board$svc$list_objects_v2(
    Bucket = board$bucket,
    Prefix = paste0(board$prefix, name, "/"),
    Delimiter = "/")

  final_list <- resp$CommonPrefixes

  while(!is_empty(resp$NextContinuationToken)) {
    resp <- board$svc$list_objects_v2(
      Bucket = board$bucket,
      Prefix = paste0(board$prefix, name, "/"),
      Delimiter = "/",
      ContinuationToken = resp$NextContinuationToken)
    final_list <- c(final_list, resp$CommonPrefixes)
  }

  paths <- fs::path_file(map_chr(final_list, ~ .$Prefix))
  version_from_path(paths)
}

#' @export
pin_version_delete.pins_board_s3 <- function(board, name, version, ...) {
  s3_delete_dir(board, fs::path(name, version))
}

#' @export
pin_meta.pins_board_s3 <- function(board, name, version = NULL, ...) {
  check_pin_exists(board, name)
  version <- check_pin_version(board, name, version)
  metadata_key <- fs::path(name, version, "data.txt")

  if (!s3_file_exists(board, metadata_key)) {
    abort_pin_version_missing(version)
  }

  path_version <- fs::path(board$cache, name, version)
  fs::dir_create(path_version)

  s3_download(board, metadata_key, immutable = TRUE)
  local_meta(
    read_meta(fs::path(board$cache, name, version)),
    name = name,
    dir = path_version,
    version = version
  )
}

#' @export
pin_fetch.pins_board_s3 <- function(board, name, version = NULL, ...) {
  meta <- pin_meta(board, name, version = version)
  cache_touch(board, meta)

  for (file in meta$file) {
    key <- fs::path(name, meta$local$version, file)
    s3_download(board, key, immutable = TRUE)
  }

  meta
}

#' @export
pin_store.pins_board_s3 <- function(board, name, paths, metadata,
                                    versioned = NULL, x = NULL, ...) {
  check_dots_used()
  check_pin_name(name)
  version <- version_setup(board, name, version_name(metadata), versioned = versioned)

  version_dir <- fs::path(name, version)
  s3_upload_yaml(board, fs::path(version_dir, "data.txt"), metadata, ...)
  for (path in paths) {
    s3_upload_file(board, fs::path(version_dir, fs::path_file(path)), path, ...)
  }

  name
}

#' @rdname board_deparse
#' @export
board_deparse.pins_board_s3 <- function(board, ...) {
  bucket <- check_board_deparse(board, "bucket")

  config <- board$svc$.internal$config
  board_args <- compact(list(
    bucket = bucket,
    prefix = board[["prefix"]],
    region = empty_string_to_null(config$region),
    endpoint = empty_string_to_null(config$endpoint),
    profile = empty_string_to_null(config$credentials$profile)
  ))
  expr(board_s3(!!!board_args))
}

empty_string_to_null <- function(x) {
  if (is.null(x) || nchar(x) == 0) NULL else x
}

#' @export
write_board_manifest_yaml.pins_board_s3 <- function(board, manifest, ...) {
  s3_upload_yaml(board, key = manifest_pin_yaml_filename, yaml = manifest, ...)
}

#' @rdname required_pkgs.pins_board
#' @export
required_pkgs.pins_board_s3 <- function(x, ...) {
  check_dots_empty()
  "paws.storage"
}

# Helpers -----------------------------------------------------------------

s3_delete_dir <- function(board, dir) {
  resp <- board$svc$list_objects_v2(
    Bucket = board$bucket,
    Prefix = paste0(board$prefix, dir, "/")
  )
  if (resp$KeyCount == 0) {
    return(invisible())
  }

  delete <- list(Objects = map(resp$Contents, ~ list(Key = .$Key)))
  board$svc$delete_objects(board$bucket, Delete = delete)
  invisible()
}

s3_upload_yaml <- function(board, key, yaml, ...) {
  body <- charToRaw(yaml::as.yaml(yaml))
  board$svc$put_object(
    Bucket = board$bucket,
    Key = paste0(board$prefix, key),
    Body = body,
    ...
  )
}

s3_upload_file <- function(board, key, path, ...) {
  check_dots_used()
  body <- readBin(path, "raw", file.size(path))
  board$svc$put_object(
    Bucket = board$bucket,
    Key = paste0(board$prefix, key),
    Body = body,
    ...
  )
}

s3_download <- function(board, key, immutable = FALSE) {
  path <- fs::path(board$cache, key)

  if (!immutable || !fs::file_exists(path)) {
    resp <- board$svc$get_object(
      Bucket = board$bucket,
      Key = paste0(board$prefix, key)
    )
    writeBin(resp$Body, path)
    fs::file_chmod(path, "u=r")
  }

  path
}

s3_file_exists <- function(board, key) {
  resp <- board$svc$list_objects_v2(
    Bucket = board$bucket,
    Prefix = paste0(board$prefix, key)
  )
  resp$KeyCount > 0
}

strip_prefix <- function(x, prefix) {
  if (is.null(prefix)) {
    return(x)
  }

  to_strip <- startsWith(x, prefix)
  x[to_strip] <- substr(x[to_strip], nchar(prefix) + 1, nchar(x[to_strip]))
  x
}
