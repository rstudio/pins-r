gcloud_headers <- function(board, verb, path, file) {

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
                         encode = "json")

  if (httr::http_error(response)) {
    warning("Failed to update data.txt metadata: ", datatxt_response_content(response))
  }
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

gcloud_binary <- function() {
  user_path <- Sys.getenv("gcloud.binary.path", getOption("gcloud.binary.path", ""))
  if (nchar(user_path) > 0)
    return(normalizePath(user_path))

  candidates <- gcloud_candidates("gcloud")

  for (candidate in candidates)
    if (file.exists(candidate()))
      return(normalizePath(candidate()))

  NULL
}

board_initialize.gcloud <- function(board,
                                    bucket = Sys.getenv("GCLOUD_STORAGE_BUCKET"),
                                    token = NULL,
                                    cache = NULL,
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

  gcloud_url <- paste0(
    "https://storage.googleapis.com/",
    bucket
  )

  board_register_datatxt(name = board$name,
                         url = gcloud_url,
                         cache = cache,
                         headers = gcloud_headers,
                         needs_index = FALSE,
                         bucket = bucket,
                         token = token,
                         connect = FALSE,
                         browse_url = paste0("https://console.cloud.google.com/storage/browser/", bucket),
                         index_randomize = TRUE,
                         index_updated = gcloud_index_updated,
                         ...)

  board_get(board$name)
}

