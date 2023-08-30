#' List, delete, and prune pin versions
#'
#' @description
#' * `pin_versions()` lists available versions a pin.
#' * `pin_versions_prune()` deletes old versions.
#' * `pin_version_delete()` deletes a single version.
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
#'
#' # delete all versions created more than 30 days ago
#' board %>% pin_versions_prune("df", days = 30)
#' @export
pin_versions <- function(board, name, ..., full = deprecated()) {
  ellipsis::check_dots_used()
  if (lifecycle::is_present(full)) {
    lifecycle::deprecate_stop("1.0.0", "pin_versions(full)")
  }

  if (missing(name) && is.board(board)) {
    abort("Argument `name` is missing, with no default")
  } else if (missing(name) || is.board(name) || name %in% board_list()) {
    swap <- board
    board <- if (missing(name)) NULL else name
    board <- board_get(board)
    name <- swap
    swapped <- TRUE
  } else {
    swapped <- FALSE
  }

  if (0 %in% board$api) {
    board_pin_versions(board, name)
  } else {
    if (swapped) {
      abort("Please supply `board` then `name` when working with modern boards")
    }
    # Can't inline UseMethod() call because it looks at arguments in
    # call, not in local env
    return(pin_versions_modern(board, name, ...))
    # But need to keep it here so roxygen2 detects its a generic
    UseMethod("pin_versions")
  }
}

pin_versions_modern <- function(board, name, ...) {
  UseMethod("pin_versions")
}

#' @export
pin_versions.pins_board <- function(board, name, ...) {
  abort("This board doesn't support versions")
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
  abort("This board doesn't support versions")
}

#' @export
#' @rdname pin_versions
#' @param n,days Pick one of `n` or `days` to choose how many versions to
#'   keep. `n = 3` will keep the last three versions, `days = 14` will
#'   keep all the versions created in the 14 days. Regardless of what values you
#'   set, `pin_versions_prune()` will never delete the most recent version.
pin_versions_prune <- function(board, name, n = NULL, days = NULL, ...) {
  versions <- pin_versions(board, name)
  keep <- versions_keep(versions$created, n = n, days = days)

  if (!all(keep)) {
    to_delete <- versions$version[!keep]

    pins_inform(paste0("Deleting versions: ", paste0(to_delete, collapse = ", ")))
    for (version in to_delete) {
      pin_version_delete(board, name, version, ...)
    }
  } else {
    pins_inform("No old versions to delete")
  }

}

versions_keep <- function(created, n = NULL, days = NULL) {
  if (!inherits(created, "POSIXct")) {
    abort("Internal error: invalid result from pin_versions()")
  }
  if (!xor(is.null(n), is.null(days))) {
    abort("Must supply exactly one of `n` and `days`")
  }

  if (!is.null(n)) {
    if (!is_integerish(n, 1)) {
      abort("`n` must be a single integer")
    }
    keep <- order(created, decreasing = TRUE) <= n
  } else {
    if (!is_integerish(days, 1)) {
      abort("`days` must be a single integer")
    }
    keep <- created > (Sys.time() - days * 86400)
  }
  keep[which.max(created)] <- TRUE

  keep
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
  paste0(metadata$created, "-", substr(metadata$pin_hash, 1, 5))
}

version_from_path <- function(x) {
  out <- versions_template(x)

  pieces <- strsplit(x, "-")
  n_ok <- lengths(pieces) == 2
  out$created[n_ok] <- parse_8601_compact(map_chr(pieces[n_ok], 1))
  out$hash[n_ok] <- map_chr(pieces[n_ok], 2)

  out
}

version_setup <- function(board, name, new_version, versioned = NULL, call = caller_env()) {
  if (pin_exists(board, name)) {
    versions <- pin_versions(board, name)
    old_version <- versions$version[[1]]
    n_versions <- nrow(versions)
    if (old_version == new_version) {
      cli::cli_abort(c(
        "The new version {.val {new_version}} is the same as the most recent version.",
        i = "Did you try to create a new version with the same timestamp as the last version?"
      ),
      call = call)
    }
  } else {
    n_versions <- 0
  }

  versioned <- versioned %||% if (n_versions > 1) TRUE else board$versioned

  if (versioned || n_versions == 0) {
    pins_inform("Creating new version '{new_version}'")
  } else if (n_versions == 1) {
    pins_inform("Replacing version '{old_version}' with '{new_version}'")
    pin_version_delete(board, name, old_version)
  } else {
    abort_pin_versioned()
  }

  new_version
}
