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

  new_board("pins_board_s3",
    name = "s3",
    api = 1,
    bucket = bucket,
    svc = svc
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

  # TODO: cache
  resp <- board$svc$get_object(board$bucket, Key = fs::path(name, "versions.yml"))
  json <- yaml::yaml.load(rawToChar(resp$Body))
  tibble::as_tibble(json$versions)
}

#' @export
pin_meta.pins_board_s3 <- function(board, name, version = NULL, ...) {
  version <- version %||%
    last(pin_versions(board, name)$version) %||%
    abort("No versions found")

  # TODO: error messages for bad name/bad version
  # TODO: cache
  resp <- board$svc$get_object(board$bucket, Key = fs::path(name, version, "data.txt"))
  yaml::yaml.load(rawToChar(resp$Body))
}
