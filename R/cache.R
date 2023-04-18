#' Cache management
#'
#' @description
#' Most boards maintain a local cache so that if you're reading a pin
#' that hasn't changed since the last time you read it, it can be rapidly
#' retrieved from a local cache. These functions help you manage that cache.
#'
#' * `cache_browse()`: open the cache directory for interactive exploration.
#' * `cache_info()`: report how much disk space each board's cache uses.
#' * `cache_prune()`: delete pin versions that you haven't used for `days`
#'   (you'll be asked to confirm before the deletion happens).
#'
#' In general, there's no real harm to deleting the cached pins, as they'll
#' be re-downloaded as needed. The one exception is [legacy_local()] which
#' mistakenly stored its pinned data in the cache directory; do not touch
#' this directory.
#'
#' @export
cache_browse <- function() {
  browse_url(cache_dir())
}

#' @export
#' @rdname cache_browse
cache_info <- function() {
  boards <- cache_boards()

  pins_inform("Cache info: '{cache_dir()}'")
  for (board in boards) {
    size <- dir_size(board)
    cat("* ", fs::path_file(board), ": ", format(size), "\n", sep = "")
  }
}

#' @export
#' @rdname cache_browse
#' @param days Number of days to preserve cached data; any pin versions
#'   older than `days` will be removed.
cache_prune <- function(days = 30) {
  to_delete <- cache_old(days)
  if (length(to_delete) == 0) {
    pins_inform("No stale pins found")
    return(invisible())
  }

  size <- dir_size(to_delete)
  pins_inform("Delete {length(to_delete)} pin versions, freeing {format(size)} ?")

  if (utils::menu(c("Yes", "No")) == 1) {
    fs::dir_delete(to_delete)
  }
}

cache_old <- function(days = 30) {
  versions <- cache_versions()
  data_txt <- fs::path(versions, "data.txt")

  mtime <- fs::file_info(data_txt)$modification_time
  prune <- mtime < (Sys.time() - 86400 * days)

  versions[prune]
}

# Paths -------------------------------------------------------------------

cache_boards <- function() {
  boards <- fs::dir_ls(cache_dir(), type = "directory")
  # Must remove old local board since it's not a cache!!
  boards[fs::path_file(boards) != "local"]
}

cache_versions <- function() {
  versions <- fs::dir_ls(cache_boards(), recurse = 2, type = "directory")
  versions[fs::file_exists(fs::path(versions, "data.txt"))]
}

cache_dir <- function() {
  rappdirs::user_cache_dir("pins")
}

# Helpers -----------------------------------------------------------------

dir_size <- function(x) {
  paths <- fs::dir_ls(x, recurse = TRUE, type = "file")
  sum(fs::file_size(paths))
}

cache_touch <- function(board, meta, time = Sys.time()) {
  path <- fs::path(meta$local$dir, "data.txt")
  if (fs::file_exists(path)) {
    fs::file_chmod(path, "u+w")
    fs::file_touch(path, access_time = time)
    fs::file_chmod(path, "u=r")
  } else {
    fs::file_create(path)
  }
}
