gfs_headers <- function(board, verb, path, file) {
  headers <- httr::add_headers(
    Authorization = paste("Bearer", board$access_token)
  )

  headers
}

gfs_candidates <- function(binary) {
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
      function() file.path(gcloud_binary_default(), "bin", binary_name)
    )
  }
}

gfs_binary <- function() {
  user_path <- Sys.getenv("gcloud.binary.path", getOption("gcloud.binary.path", ""))
  if (nchar(user_path) > 0)
    return(normalizePath(user_path))

  candidates <- gfs_candidates("gcloud")

  for (candidate in candidates)
    if (file.exists(candidate()))
      return(normalizePath(candidate()))

  NULL
}

board_initialize.gfs <- function(board,
                                 bucket = Sys.getenv("GOOGLE_STORAGE_BUCKET"),
                                 access_token = NULL,
                                 cache = NULL,
                                 ...) {

  if (nchar(bucket) == 0) stop("Board 'gfs' requires a 'bucket' parameter.")

  if (is.null(access_token)) {
    access_token <- Sys.getenv("GOOGLE_STORAGE_ACCESS_TOKEN")
    if (nchar(access_token) == 0) {
      gcloud <- gfs_binary()
      if (!is.null(gcloud)) {
        access_token <- system2(gfs_binary(), args = c("auth", "print-access-token"), stdout = TRUE)
      }
      else {
        stop("Board 'gfs' requires an 'access' parameter with a Google Cloud Access Token.")
      }
    }
  }

  gfs_url <- paste0(
    "https://www.googleapis.com/storage/v1/b/",
    bucket,
    "/o/"
  )

  board_register_datatxt(name = board$name,
                         url = gfs_url,
                         cache = cache,
                         needs_index = FALSE,
                         bucket = bucket,
                         access_token = access_token)

  board_get(board$name)
}

