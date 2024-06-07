#' Use a Google Cloud Storage bucket as a board
#'
#' Pin data to a Google Cloud Storage bucket using the googleCloudStorageR
#' package.
#'
#' # Authentication
#'
#' `board_gcs()` is powered by the googleCloudStorageR package which provides
#' several authentication options, as documented in its
#' [main vignette](https://code.markedmondson.me/googleCloudStorageR/articles/googleCloudStorageR.html).
#' The two main options are to create a service account key (a JSON file) or an
#' authentication token; you can manage either using the [gargle](https://gargle.r-lib.org/) package.
#'
#' # Details
#'
#' * The functions in pins do not create a new bucket. You can create
#'   a new bucket from R with [googleCloudStorageR::gcs_create_bucket()].
#' * You can pass arguments for [googleCloudStorageR::gcs_upload] such as
#'   `predefinedAcl` and `upload_type` through the dots of `pin_write()`.
#' * `board_gcs()` is powered by the googleCloudStorageR package, which is a
#'   suggested dependency of pins (not required for pins in general). If
#'   you run into errors when deploying content to a server like
#'   <https://www.shinyapps.io> or [Connect](https://posit.co/products/enterprise/connect/),
#'   add `requireNamespace("googleCloudStorageR")` to your app or document for [automatic
#'   dependency discovery](https://docs.posit.co/connect/user/troubleshooting/#render-missing-r-package).
#'
#' @inheritParams new_board
#' @param bucket Bucket name. You can only write to an existing bucket, and you
#'   can use [googleCloudStorageR::gcs_get_global_bucket()] here.
#' @param prefix Prefix within this bucket that this board will occupy.
#'   You can use this to maintain multiple independent pin boards within
#'   a single GCS bucket. Will typically end with `/` to take advantage of
#'   Google Cloud Storage's directory-like handling.
#' @export
#' @examples
#' \dontrun{
#' board <- board_gcs("pins-testing")
#' board %>% pin_write(mtcars)
#' board %>% pin_read("mtcars")
#'
#' # A prefix allows you to have multiple independent boards in the same pin.
#' board_sales <- board_gcs("company-pins", prefix = "sales/")
#' board_marketing <- board_gcs("company-pins", prefix = "marketing/")
#' # You can make the hierarchy arbitrarily deep.
#'
#' # Pass arguments like `predefinedAcl` through the dots of `pin_write`:
#' board %>% pin_write(mtcars, predefinedAcl = "publicRead")
#' }
board_gcs <- function(bucket,
                      prefix = NULL,
                      versioned = TRUE,
                      cache = NULL) {

  check_installed("googleCloudStorageR")

  # Check that have access to the bucket
  googleCloudStorageR::gcs_get_bucket(bucket)

  cache <- cache %||% board_cache_path(paste0("gcs-", bucket))
  new_board_v1(
    "pins_board_gcs",
    name = "gcs",
    bucket = bucket,
    prefix = prefix,
    cache = cache,
    versioned = versioned
  )
}

board_gcs_test <- function(...) {

  skip_if_missing_envvars(
    tests = "board_gcs()",
    envvars = c("PINS_GCS_PASSWORD")
  )

  path_to_encrypted_json <- fs::path_package("pins", "secret", "pins-gcs-testing.json")
  raw <- readBin(path_to_encrypted_json, "raw", file.size(path_to_encrypted_json))
  pw <- Sys.getenv("PINS_GCS_PASSWORD", "")
  json <- sodium::data_decrypt(
    bin = raw,
    key = sodium::sha256(charToRaw(pw)),
    nonce = secret_nonce()
  )
  googleCloudStorageR::gcs_auth(json_file = rawToChar(json))

  board_gcs("pins-dev", cache = tempfile(), ...)
}

## for decrypting JSON for service account:
secret_nonce <- function() {
  sodium::hex2bin("cb36bab652dec6ae9b1827c684a7b6d21d2ea31cd9f766ac")
}

#' @export
pin_list.pins_board_gcs <- function(board, ...) {
  NA
}

#' @export
pin_exists.pins_board_gcs <- function(board, name, ...) {
  withr::local_options(list(googleAuthR.verbose = 4))
  gcs_file_exists(board, name)
}

#' @export
pin_delete.pins_board_gcs <- function(board, names, ...) {
  for (name in names) {
    check_pin_exists(board, name)
    gcs_delete_dir(board, name)
  }
  invisible(board)
}

#' @export
pin_versions.pins_board_gcs <- function(board, name, ...) {
  check_pin_exists(board, name)
  resp <- googleCloudStorageR::gcs_list_objects(
    bucket = board$bucket,
    prefix = paste0(board$prefix, name)
  )
  paths <- fs::path_split(unique(fs::path_dir(resp$name)))
  version_from_path(map_chr(paths, ~ .x[[length(.x)]]))
}

#' @export
pin_version_delete.pins_board_gcs <- function(board, name, version, ...) {
  gcs_delete_dir(board, fs::path(name, version))
}

#' @export
pin_meta.pins_board_gcs <- function(board, name, version = NULL, ...) {
  withr::local_options(list(googleAuthR.verbose = 4))
  check_pin_exists(board, name)
  version <- check_pin_version(board, name, version)
  metadata_blob <- fs::path(name, version, "data.txt")

  if (!gcs_file_exists(board, metadata_blob)) {
    abort_pin_version_missing(version)
  }

  path_version <- fs::path(board$cache, name, version)
  fs::dir_create(path_version)
  gcs_download(board, metadata_blob)
  local_meta(
    read_meta(fs::path(board$cache, name, version)),
    name = name,
    dir = path_version,
    version = version
  )
}

#' @export
pin_fetch.pins_board_gcs <- function(board, name, version = NULL, ...) {
  withr::local_options(list(googleAuthR.verbose = 4))
  meta <- pin_meta(board, name, version = version)
  cache_touch(board, meta)

  for (file in meta$file) {
    key <- fs::path(name, meta$local$version, file)
    gcs_download(board, key)
  }

  meta
}

#' @export
pin_store.pins_board_gcs <- function(board, name, paths, metadata,
                                     versioned = NULL, x = NULL, ...) {
  withr::local_options(list(googleAuthR.verbose = 4))
  check_dots_used()
  check_pin_name(name)
  version <- version_setup(board, name, version_name(metadata), versioned = versioned)
  version_dir <- fs::path(name, version)
  gcs_upload_yaml(
    board,
    fs::path(paste0(board$prefix, version_dir), "data.txt"),
    metadata
  )

  for (path in paths) {
    googleCloudStorageR::gcs_upload(
      file = path,
      bucket = board$bucket,
      name = fs::path(paste0(board$prefix, version_dir), fs::path_file(path)),
      ...
    )
  }

  name
}

#' @rdname board_deparse
#' @export
board_deparse.pins_board_gcs <- function(board, ...) {
  bucket <- check_board_deparse(board, "bucket")
  expr(board_gcs(!!bucket, prefix = !!board$prefix))
}

#' @rdname required_pkgs.pins_board
#' @export
required_pkgs.pins_board_gcs <- function(x, ...) {
  check_dots_empty()
  "googleCloudStorageR"
}

# Helpers -----------------------------------------------------------------

gcs_delete_dir <- function(board, dir) {
  resp <- googleCloudStorageR::gcs_list_objects(
    bucket = board$bucket,
    prefix = paste0(board$prefix, dir, "/")
  )

  if (nrow(resp) == 0) {
    return(invisible())
  }

  for (path in resp$name) {
    googleCloudStorageR::gcs_delete_object(path, bucket = board$bucket)
  }

  invisible()
}

gcs_upload_yaml <- function(board, key, yaml, ...) {
  temp_file <- withr::local_tempfile()
  yaml::write_yaml(yaml, file = temp_file)
  googleCloudStorageR::gcs_upload(
    file = temp_file,
    bucket = board$bucket,
    type = "text/yaml",
    name = key,
    ...
  )
}

gcs_download <- function(board, key) {
  path <- fs::path(board$cache, key)
  if (!fs::file_exists(path)) {
    suppressMessages(googleCloudStorageR::gcs_get_object(
      object_name = paste0(board$prefix, key),
      bucket = board$bucket,
      saveToDisk = path
    ))
    fs::file_chmod(path, "u=r")
  }
  path
}

gcs_file_exists <- function(board, name) {
  resp <- googleCloudStorageR::gcs_list_objects(
    bucket = board$bucket,
    prefix = paste0(board$prefix, name)
  )
  nrow(resp) > 0
}
