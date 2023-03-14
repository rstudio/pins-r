#' Use a OneDrive or Sharepoint document library as a board
#'
#' Pin data to a folder in Onedrive or a SharePoint Online document library
#' using the Microsoft365R package.
#'
#' @inheritParams new_board
#' @param drive A OneDrive or SharePoint document library object, of class
#'   [`Microsoft365R::ms_drive`].
#' @param path Path to directory to store pins. This can be either a string
#'   containing the pathname like `"path/to/board"`, or a
#'   [`Microsoft365R::ms_drive_item`] object pointing to the board path.
#' @param delete_by_item Whether to handle folder deletions on an item-by-item
#'   basis, rather than deleting the entire folder at once. You may need to set
#'   this to `TRUE` for a board in SharePoint Online or OneDrive for Business,
#'   due to document protection policies that prohibit deleting non-empty
#'   folders.
#' @details
#' Sharing a board in OneDrive (personal or business) is a bit complicated, as
#' OneDrive normally allows only the person who owns the drive to access files
#' and folders. First, the drive owner has to set the board folder as shared
#' with other users, using either the OneDrive web interface or Microsoft365R's
#' `ms_drive_item$create_share_link()` method. The other users then call
#' `board_ms365` with a _drive item object_ in the `path` argument, pointing to
#' the shared folder. See the examples below.
#'
#' Sharing a board in SharePoint Online is much more straightforward, assuming
#' all users have access to the document library: in this case, everyone can
#' use the same call `board_ms365(doclib, "path/to/board")`. If you want to
#' share a board with users outside your team, follow the same steps for sharing
#' a board in OneDrive.
#'
#' `board_ms365()` is powered by the Microsoft365R package, which is a suggested
#' dependency of pins (not required for pins in general). If you run into errors
#' when deploying content to a server like <https://www.shinyapps.io> or
#' [Connect](https://posit.co/products/enterprise/connect/), add
#' `requireNamespace("Microsoft365R")` to your app or document for [automatic dependency
#' discovery](https://docs.posit.co/connect/user/troubleshooting/#render-missing-r-package).
#'
#' @export
#' @examples
#' \dontrun{
#' # A board in your personal OneDrive
#' od <- Microsoft365R::get_personal_onedrive()
#' board <- board_ms365(od, "myboard")
#' board %>% pin_write(iris)
#'
#' # A board in OneDrive for Business
#' odb <- Microsoft365R::get_business_onedrive(tenant = "mytenant")
#' board <- board_ms365(odb, "myproject/board")
#'
#' # A board in a SharePoint Online document library
#' sp <- Microsoft365R::get_sharepoint_site("my site", tenant = "mytenant")
#' doclib <- sp$get_drive()
#' board <- board_ms365(doclib, "general/project1/board")
#'
#'
#' ## Sharing a board in OneDrive:
#' # First, create the board on the drive owner's side
#' board <- board_ms365(od, "myboard")
#'
#' # Next, let other users write to the folder
#' # - set the expiry to NULL if you want the folder to be permanently available
#' od$get_item("myboard")$create_share_link("edit", expiry="30 days")
#'
#' # On the recipient's side: find the shared folder item, then pass it to board_ms365
#' shared_items <- od$list_shared_items()
#' board_folder <- shared_items$remoteItem[[which(shared_items$name == "myboard")]]
#' board <- board_ms365(od, board_folder)
#' }
board_ms365 <- function(drive, path, versioned = TRUE, cache = NULL, delete_by_item = FALSE) {
  check_installed("Microsoft365R")

  if (!inherits(drive, "ms_drive")) {
    abort("`drive` must be a OneDrive or SharePoint document library object")
  }
  if (!inherits(path, c("character", "ms_drive_item"))) {
    abort("`path` must be either a string or a drive item object")
  }

  if (!inherits(path, "ms_drive_item")) {
    # try to create the board folder: ignore error if folder already exists
    try(drive$create_folder(path), silent = TRUE)
    folder <- drive$get_item(path)
  }
  else {
    folder <- path
    # ensure we have the correct properties for a shared item in OneDrive
    folder$sync_fields()
    path <- NULL
  }

  if (!folder$is_folder()) {
    abort("Invalid path specified")
  }

  cache <- cache %||% board_cache_path(paste0("ms365-", hash(folder$properties$id)))
  new_board_v1(
    "pins_board_ms365",
    folder = folder,
    path = path,
    cache = cache,
    versioned = versioned,
    delete_by_item = delete_by_item
  )
}

board_ms365_test_charpath <- function(...) {
  if (identical(Sys.getenv("PINS_MS365_USE_PERSONAL"), "true")) {
    drv <- Microsoft365R::get_personal_onedrive()
  } else {
    skip_if_missing_envvars("board_ms365()", "PINS_MS365_TEST_DRIVE")
    drv <- readRDS(Sys.getenv("PINS_MS365_TEST_DRIVE"))
  }
  board_ms365(drv, path = "pin_testing", cache = tempfile(), ...)
}

board_ms365_test_driveitem <- function(...) {
  if (identical(Sys.getenv("PINS_MS365_USE_PERSONAL"), "true")) {
    drv <- Microsoft365R::get_personal_onedrive()
  } else {
    skip_if_missing_envvars("board_ms365()", "PINS_MS365_TEST_DRIVE")
    drv <- readRDS(Sys.getenv("PINS_MS365_TEST_DRIVE"))
  }
  # try to create the folder, get it if it already exists
  folder <- try(drv$create_folder("pin_testing_2"), silent = TRUE)
  if (inherits(folder, "try-error")) {
    folder <- drv$get_item("pin_testing_2")
  }
  board_ms365(drv, path = folder, cache = tempfile(), ...)
}

#' @export
pin_list.pins_board_ms365 <- function(board, ...) {
  ms365_list_dirs(board)
}

#' @export
pin_exists.pins_board_ms365 <- function(board, name, ...) {
  name %in% ms365_list_dirs(board)
}

#' @export
pin_delete.pins_board_ms365 <- function(board, names, ...) {
  for (name in names) {
    check_pin_exists(board, name)
    ms365_delete_dir(board, name)
  }
  invisible(board)
}

#' @export
pin_versions.pins_board_ms365 <- function(board, name, ...) {
  check_pin_exists(board, name)
  version_from_path(ms365_list_dirs(board, name))
}

#' @export
pin_version_delete.pins_board_ms365 <- function(board, name, version, ...) {
  ms365_delete_dir(board, fs::path(name, version))
}

#' @export
pin_meta.pins_board_ms365 <- function(board, name, version = NULL, ...) {
  check_pin_exists(board, name)
  version <- check_pin_version(board, name, version)
  metadata_key <- fs::path(name, version, "data.txt")

  if (!ms365_file_exists(board, metadata_key)) {
    abort_pin_version_missing(version)
  }

  path_version <- fs::path(board$cache, name, version)
  fs::dir_create(path_version)

  ms365_download(board, metadata_key)
  local_meta(
    read_meta(fs::path(board$cache, name, version)),
    name = name,
    dir = path_version,
    version = version
  )
}

#' @export
pin_fetch.pins_board_ms365 <- function(board, name, version = NULL, ...) {
  meta <- pin_meta(board, name, version = version)
  cache_touch(board, meta)

  for (file in meta$file) {
    key <- fs::path(name, meta$local$version, file)
    ms365_download(board, key)
  }

  meta
}

#' @export
pin_store.pins_board_ms365 <- function(board, name, paths, metadata,
                                       versioned = NULL, ...) {
  check_pin_name(name)
  version <- version_setup(board, name, version_name(metadata), versioned = versioned)

  version_dir <- fs::path(name, version)

  # Upload metadata
  meta_tmpfile <- tempfile(fileext = ".yml")
  on.exit(unlink(meta_tmpfile))
  yaml::write_yaml(metadata, meta_tmpfile)
  board$folder$upload(meta_tmpfile, fs::path(version_dir, "data.txt"))

  # Upload files
  for (path in paths) {
    board$folder$upload(path, fs::path(version_dir, fs::path_file(path)))
  }

  name
}

#' @export
write_board_manifest_yaml.pins_board_ms365 <- function(board, manifest, ...) {
  temp_file <- withr::local_tempfile()
  yaml::write_yaml(manifest, file = temp_file)
  board$folder$upload(temp_file, manifest_pin_yaml_filename)
}

#' @rdname required_pkgs.pins_board
#' @export
required_pkgs.pins_board_ms365 <- function(x, ...) {
  ellipsis::check_dots_empty()
  "Microsoft365R"
}

# Helpers -----------------------------------------------------------------

# list all the directories inside 'path', which is assumed to live in the board folder
ms365_list_dirs <- function(board, path = "") {
  conts <- board$folder$list_files(path)
  conts$name[conts$isdir]
}

# delete directory 'path', which is assumed to live in the board folder
ms365_delete_dir <- function(board, path = "") {
  child <- board$folder$get_item(path)
  child$delete(confirm = FALSE, by_item = board$delete_by_item)
}

# check if a file exists and is not a directory
ms365_file_exists <- function(board, key) {
  item <- try(board$folder$get_item(key), silent = TRUE)
  inherits(item, "ms_drive_item") && !item$is_folder()
}

# download a specific file from the board, as given by the 'key' path
ms365_download <- function(board, key) {
  path <- fs::path(board$cache, key)

  if (!fs::file_exists(path)) {
    board$folder$get_item(key)$download(path)
    fs::file_chmod(path, "u=r")
  }

  path
}
