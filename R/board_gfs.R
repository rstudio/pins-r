gfs_headers <- function(board, verb, path, file) {
  headers <- httr::add_headers(
    Authorization = paste("Bearer", board$token)
  )

  if (!is.null(file)) {
    headers[["Content-Type"]] <- mime::guess_type(file)
  }

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
                                 token = NULL,
                                 cache = NULL,
                                 ...) {

  if (nchar(bucket) == 0) stop("Board 'gfs' requires a 'bucket' parameter.")

  if (is.null(token)) {
    token <- Sys.getenv("GOOGLE_STORAGE_ACCESS_TOKEN")
    if (nchar(token) == 0) {
      gcloud <- gfs_binary()
      if (!is.null(gcloud)) {
        token <- system2(gfs_binary(), args = c("auth", "print-access-token"), stdout = TRUE)
      }
      else {
        stop("Board 'gfs' requires an 'access' parameter with a Google Cloud Access Token.")
      }
    }
  }

  gfs_url <- paste0(
    "https://storage.googleapis.com/",
    bucket
  )

  board_register_datatxt(name = board$name,
                         url = gfs_url,
                         cache = cache,
                         headers = gfs_headers,
                         needs_index = FALSE,
                         bucket = bucket,
                         token = token)

  board_get(board$name)
}

