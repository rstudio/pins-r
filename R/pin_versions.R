#' Pin versions
#'
#' `pin_versions()` list versions available for a pin; `pin_version_delete()`
#' allows you to delete a single version. See `vignette("versioning)"` for
#' more details about versioning pins.
#'
#' @param board,name A pair of board and pin name. For modern boards,
#'   use `board %>% pin_versions(name)`. For backward compatibility with the
#'   legacy API, you can also use `pin_versions(name)` or
#'   `pin_version(name, board)`.
#' @param full `r lifecycle::badge("deprecated")`
#' @param ... Additional arguments passed on to methods for a specific board.
#' @return A data frame with at least a `version` column. Some boards may
#'   provided additional data.
#' @examples
#' board <- board_temp(versioned = TRUE)
#'
#' board %>% pin_write(data.frame(x = 1:5), name = "df")
#' board %>% pin_write(data.frame(x = 2:6), name = "df")
#' board %>% pin_write(data.frame(x = 3:7), name = "df")
#'
#' # pin_read() returns the latest version by default
#' board %>% pin_read("df")
#'
#' # but you can return earlier versions if needed
#' board %>% pin_versions("df")
#'
#' ver <- pin_versions(board, "df")$version[[1]]
#' board %>% pin_read("df", version = ver)
#' @export
pin_versions <- function(board, name, ..., full = deprecated()) {
  ellipsis::check_dots_used()
  if (lifecycle::is_present(full)) {
    lifecycle::deprecate_warn("1.0.0", "pin_versions(full)")
  }

  if (missing(name) || is.board(name) || name %in% board_list()) {
    swap <- board
    board <- if (missing(name)) NULL else name
    board <- board_get(board)
    name <- swap

    if (!0 %in% board$api) {
      abort("Please supply `board` then `name` when working with modern boards")
    }

    board_pin_versions(board, name)
  } else {
    if (!1 %in% board$api) {
      abort("Please supply `name` then `board` when working with legacy boards")
    }

    UseMethod("pin_versions")
  }
}

#' @export
pin_versions.pins_board <- function(board, name, ...) {
  abort("board_url() doesn't support versions")
}

#' @export
#' @rdname pin_versions
#' @param version Version identifier.
pin_version_delete <- function(board, name, version, ...) {
  ellipsis::check_dots_used()
  UseMethod("pin_version_delete")
}

#' @export
pin_version_delete.pins_board <- function(board, name, version, ...) {
  abort("board_url() doesn't support versions")
}

versions_template <- function(version = character()) {
  if (!is.character(version)) {
    abort("`version` must be a character vector")
  }

  tibble::tibble(
    version = version,
    created = .POSIXct(NA_real_, tz = ""),
    hash = NA_character_
  )
}

version_name <- function(metadata) {
  if (is_testing()) {
    paste0("20120304T050607Z-", substr(metadata$pin_hash, 1, 5))
  } else {
    paste0(metadata$created, "-", substr(metadata$pin_hash, 1, 5))
  }
}

version_from_path <- function(x) {
  out <- versions_template(x)

  pieces <- strsplit(x, "-")
  n_ok <- lengths(pieces) == 2
  out$created[n_ok] <- parse_8601_compact(map_chr(pieces[n_ok], "[[", 1))
  out$hash[n_ok] <- map_chr(pieces[n_ok], "[[", 2)

  out
}

version_setup <- function(board, name, metadata, versioned = NULL) {
  if (pin_exists(board, name)) {
    versions <- pin_versions(board, name)
    old_version <- versions$version[[1]]
    n_versions <- nrow(versions)
  } else {
    n_versions <- 0
  }

  versioned <- versioned %||% if (n_versions > 1) TRUE else board$versioned
  new_version <- version_name(metadata)

  if (versioned || n_versions == 0) {
    pins_inform(glue("Creating new version '{new_version}'"))
  } else if (n_versions == 1) {
    pins_inform(glue("Replacing version '{old_version}' with '{new_version}'"))
    pin_version_delete(board, name, old_version)
  } else {
    abort(c(
      "Pin is versioned, but you have requested a write without versions",
      i = "To un-version a pin, you must delete it"
    ))
  }

  new_version
}
