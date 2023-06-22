#' Use a Google Drive folder as a board
#'
#' Pin data to a folder in Google Drive using the googledrive package.
#'
#' @inheritParams new_board
#' @param path Path to existing directory on Google Drive to store pins. Can be
#'   given as an actual path like `"path/to/folder"` (character), a file id or
#'   URL marked with [googledrive::as_id()], or a [googledrive::dribble].
#'
#' @details
#' * The functions in pins do not create a new Google Drive folder. You can
#'   create a new folder from R with [googledrive::drive_mkdir()], and then set
#'   the sharing for your folder with [googledrive::drive_share()].
#' * If you have problems with authentication to Google Drive, learn more at
#'   [googledrive::drive_auth()].
#' * `board_gdrive()` is powered by the googledrive package, which is a
#'   suggested dependency of pins (not required for pins in general). If
#'   you run into errors when deploying content to a server like
#'   <https://www.shinyapps.io> or [Connect](https://posit.co/products/enterprise/connect/),
#'   add `requireNamespace("googledrive")` to your app or document for [automatic
#'   dependency discovery](https://docs.posit.co/connect/user/troubleshooting/#render-missing-r-package).
#'
#' @export
#'
#' @examples
#' \dontrun{
#' board <- board_gdrive("folder-for-my-pins")
#' board %>% pin_write(1:10, "great-integers", type = "json")
#' board %>% pin_read("great-integers")
#' }
board_gdrive <- function(path,
                         versioned = TRUE,
                         cache = NULL) {
  check_installed("googledrive")
  dribble <- googledrive::as_dribble(path)

  if (!googledrive::single_file(dribble) || !googledrive::is_folder(dribble)) {
    cli::cli_abort(c(
      "{.arg path} must resolve to a single existing Drive folder",
      i = "Consider creating your pin board with {.fun googledrive::drive_mkdir}"
    ))
  }

  cache <- cache %||% board_cache_path(paste0("gdrive-", hash(dribble$id)))
  new_board_v1(
    "pins_board_gdrive",
    dribble = dribble,
    cache = cache,
    versioned = versioned
  )
}

board_gdrive_test <- function(...) {
  skip_if_missing_envvars(
    tests = "board_gdrive()",
    envvars = c("PINS_GDRIVE_USE_PERSONAL")
  )

  board_gdrive("pins-testing", cache = tempfile())
}

#' @export
pin_list.pins_board_gdrive <- function(board, ...) {
  googledrive::drive_ls(board$dribble)$name
}

#' @export
pin_exists.pins_board_gdrive <- function(board, name, ...) {
  all_names <- googledrive::drive_ls(board$dribble$name)$name
  name %in% all_names
}

#' @export
pin_delete.pins_board_gdrive <- function(board, names, ...) {
  for (name in names) {
    check_pin_exists(board, name)
    gdrive_delete_dir(board, name)
  }
  invisible(board)
}

#' @export
pin_version_delete.pins_board_gdrive <- function(board, name, version, ...) {
  gdrive_delete_dir(board, fs::path(name, version))
}

#' @export
pin_versions.pins_board_gdrive <- function(board, name, ...) {
  check_pin_exists(board, name)
  path <- fs::path(board$dribble$path, name)
  version_from_path(sort(googledrive::drive_ls(path)$name))
}


#' @export
pin_meta.pins_board_gdrive <- function(board, name, version = NULL, ...) {
  googledrive::local_drive_quiet()
  check_pin_exists(board, name)
  version <- check_pin_version(board, name, version)
  metadata_key <- fs::path(name, version, "data.txt")

  if (!gdrive_file_exists(board, metadata_key)) {
    abort_pin_version_missing(version)
  }

  path_version <- fs::path(board$cache, name, version)
  fs::dir_create(path_version)

  gdrive_download(board, metadata_key)
  local_meta(
    read_meta(fs::path(board$cache, name, version)),
    name = name,
    dir = path_version,
    version = version
  )
}

#' @export
pin_fetch.pins_board_gdrive <- function(board, name, version = NULL, ...) {
  googledrive::local_drive_quiet()
  meta <- pin_meta(board, name, version = version)
  cache_touch(board, meta)

  for (file in meta$file) {
    key <- fs::path(name, meta$local$version, file)
    gdrive_download(board, key)
  }

  meta
}

#' @export
pin_store.pins_board_gdrive <- function(board, name, paths, metadata,
                                        versioned = NULL, ...) {
  googledrive::local_drive_quiet()
  check_pin_name(name)
  version <- version_setup(board, name, version_name(metadata), versioned = versioned)

  gdrive_mkdir(board$dribble$name, name)
  gdrive_mkdir(fs::path(board$dribble$name, name), version)

  version_dir <- fs::path(name, version)

  # Upload metadata
  temp_file <- withr::local_tempfile()
  yaml::write_yaml(metadata, file = temp_file)
  googledrive::drive_upload(
    temp_file,
    fs::path(board$dribble$path, version_dir, "data.txt")
  )

  # Upload files
  for (path in paths) {
    googledrive::drive_upload(
      path,
      fs::path(board$dribble$path, version_dir, fs::path_file(path))
    )
  }

  name
}


#' @rdname required_pkgs.pins_board
#' @export
required_pkgs.pins_board_gdrive <- function(x, ...) {
  ellipsis::check_dots_empty()
  "googledrive"
}


# Helpers -----------------------------------------------------------------

possibly_drive_ls <- function(...) {
  tryCatch(googledrive::drive_ls(...), error = function(err) NULL)
}

gdrive_file_exists <- function(board, name) {
  path <- fs::path(board$dribble$name, fs::path_dir(name))
  name <- fs::path_file(name)
  all_names <- possibly_drive_ls(path)
  name %in% all_names$name
}

gdrive_delete_dir <- function(board, dir) {
  path <- fs::path(board$dribble$path, dir)
  googledrive::drive_trash(path)
  invisible()
}

gdrive_download <- function(board, key) {
  path <- fs::path(board$cache, key)
  if (!fs::file_exists(path)) {
    googledrive::drive_download(key, path)
    fs::file_chmod(path, "u=r")
  }
  path
}

gdrive_mkdir <- function(dir, name) {
  dribble <- googledrive::as_dribble(fs::path(dir, name))
  if (googledrive::no_file(dribble) || !googledrive::is_folder(dribble)) {
    googledrive::drive_mkdir(name, dir, overwrite = FALSE)
  }
  invisible()
}
