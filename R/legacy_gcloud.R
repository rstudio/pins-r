#' Google Cloud board (legacy API)
#'
#' `r lifecycle::badge('deprecated')`
#'
#' To use a Google Cloud Storage board, you first need a Google Cloud Storage
#' account, a Google Storage bucket, and an access token or the
#' [Google Cloud SDK](https://cloud.google.com/sdk/) properly installed and
#' configured. You can sign-up and create these from
#' <https://console.cloud.google.com>
#'
#' @inheritParams legacy_datatxt
#' @param bucket The name of the Google Cloud Storage bucket. Defaults to the `GCLOUD_STORAGE_BUCKET` environment
#'   variable.
#' @param token The access token of the Google Cloud Storage container.
#'   Generally, it's best to leave this as `NULL`, and rely on the installed
#'   Google Cloud SDK to handle authentication.
#'
#'   If you do want to use an access token, you can retrieve it from
#'   <https://developers.google.com/oauthplayground>. You will need to
#'   authorize the "Google Storage API v1" scope.
#' @examples
#' \dontrun{
#' # the following example requires the Google Cloud SDK to be configured
#' board <- legacy_gcloud(container = "gcloudcontainer")
#' }
#' @export
#' @keywords internal
legacy_gcloud <- function(
                         bucket = Sys.getenv("GCLOUD_STORAGE_BUCKET"),
                         token = NULL,
                         cache = NULL,
                         name = "gcloud",
                         ...) {
  if (nchar(bucket) == 0) stop("Board 'gcloud' requires a 'bucket' parameter.")

  if (is.null(token)) {
    token <- Sys.getenv("GOOGLE_STORAGE_ACCESS_TOKEN")
    if (nchar(token) == 0) {
      gcloud <- gcloud_binary()
      if (!is.null(gcloud)) {
        token <- system2(gcloud_binary(), args = c("auth", "print-access-token"), stdout = TRUE)
      }
      else {
        stop("Board 'gcloud' requires an 'access' parameter with a Google Cloud Access Token.")
      }
    }
  }

  legacy_datatxt(
    name = name,
    url = paste0("https://storage.googleapis.com/", bucket),
    cache = cache,
    headers = gcloud_headers,
    needs_index = FALSE,
    bucket = bucket,
    token = token,
    connect = FALSE,
    browse_url = paste0("https://console.cloud.google.com/storage/browser/", bucket),
    index_randomize = TRUE,
    index_updated = gcloud_index_updated,
    ...
  )
}


#' @rdname legacy_gcloud
#' @export
board_register_gcloud <- function(name = "gcloud",
                                  bucket = Sys.getenv("GCLOUD_STORAGE_BUCKET"),
                                  token = NULL,
                                  cache = NULL,
                                  path = NULL,
                                  ...) {
  lifecycle::deprecate_soft(
    "1.4.0",
    "board_register_gcloud()",
    details = 'Learn more at <https://pins.rstudio.com/articles/pins-update.html>'
  )

  board <- legacy_gcloud(
    name = name,
    bucket = bucket,
    token = token,
    cache = cache,
    path = path,
    ...
  )
  board_register2(board)
}

gcloud_headers <- function(board, verb, path, file) {
  check_installed("mime")
  content_type <- NULL
  if (!is.null(file)) {
    content_type <- mime::guess_type(file)
  }

  headers <- httr::add_headers(
    Authorization = paste("Bearer", board$token),
    `Content-Type` = content_type
  )

  headers
}

gcloud_index_updated <- function(board) {
  metadata <- list(cacheControl = "private, max-age=0, no-transform", name = "data.txt")

  response <- httr::VERB("PATCH",
    paste0("https://storage.googleapis.com/storage/v1/b/", board$bucket, "/o/", "data.txt"),
    body = metadata,
    board_datatxt_headers(board, "o/data.txt", verb = "PATCH"),
    encode = "json"
  )

  if (httr::http_error(response)) {
    warning("Failed to update data.txt metadata: ", datatxt_response_content(response))
  }
}

gcloud_binary <- function() {
  user_path <- Sys.getenv("gcloud.binary.path", getOption("gcloud.binary.path", ""))
  if (nchar(user_path) > 0) {
    return(normalizePath(user_path))
  }

  candidates <- gcloud_candidates("gcloud")

  for (candidate in candidates) {
    if (file.exists(candidate())) {
      return(normalizePath(candidate()))
    }
  }

  NULL
}

gcloud_candidates <- function(binary) {
  if (.Platform$OS.type == "windows") {
    appdata <- normalizePath(Sys.getenv("localappdata"), winslash = "/")
    binary_name <- paste(binary, "cmd", sep = ".")

    c(
      function() file.path(appdata, "Google/Cloud SDK/google-cloud-sdk/bin", binary_name),
      function() file.path(Sys.getenv("ProgramFiles"), "/Google/Cloud SDK/google-cloud-sdk/bin", binary_name),
      function() file.path(Sys.getenv("ProgramFiles(x86)"), "/Google/Cloud SDK/google-cloud-sdk/bin", binary_name)
    )
  } else {
    binary_name <- binary

    c(
      function() Sys.which(binary_name),
      function() paste("~/google-cloud-sdk/bin", binary_name, sep = "/"),
      function() file.path(Sys.getenv("GCLOUD_INSTALL_PATH", "~/google-cloud-sdk"), "bin", binary_name)
    )
  }
}
