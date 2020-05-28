#' Custom Pins
#'
#' Family of functions meant to be used to implement custom pin extensions, not to be used by users.
#'
#' @param board The board to extended, retrieved with \code{board_get()}.
#' @param name The name of the pin.
#' @param path The path to store.
#' @param description The text patteren to find a pin.
#' @param type The type of pin being stored.
#' @param metadata A list containing additional metadata describing the pin.
#' @param ... Additional parameteres.
#'
#' @rdname custom-pins
#'
#' @export
#' @rdname custom-pins
board_pin_store <- function(board, path, name, description, type, metadata, extract = TRUE, ...) {
  board <- board_get(board)
  if (is.null(name)) name <- gsub("[^a-zA-Z0-9]+", "_", tools::file_path_sans_ext(basename(path)))[[1]]
  pin_log("Storing ", name, " into board ", board$name, " with type ", type)
  custom_metadata <- list(...)$custom_metadata
  zip <- list(...)$zip

  if (identical(list(...)$cache, FALSE)) pin_reset_cache(board$name, name)

  path <- path[!grepl("data\\.txt", path)]

  store_path <- tempfile()
  dir.create(store_path)
  on.exit(unlink(store_path, recursive = TRUE))

  if (length(path) == 1 && grepl("^http", path) && !grepl("\\.[a-z]{2,4}$", path) && getOption("pins.search.datatxt", TRUE)) {
    # attempt to download data.txt to enable public access to boards like rsconnect
    datatxt_path <- file.path(path, "data.txt")
    local_path <- pin_download(datatxt_path, name, board_default(), can_fail = TRUE)
    if (!is.null(local_path)) {
      manifest <- pin_manifest_get(local_path)
      path <- paste(path, manifest$path, sep = "/")
      extract <- FALSE
    }
  }

  if (identical(zip, TRUE)) {
    zip(file.path(store_path, "data.zip"), path)
    something_changed <- TRUE
  }
  else {
    something_changed <- FALSE
    for (single_path in path) {
      details <- as.environment(list(something_changed = TRUE))
      if (grepl("^http", single_path)) {
        single_path <- pin_download(single_path,
                                    name,
                                    board_default(),
                                    extract = extract,
                                    details = details,
                                    can_fail = TRUE,
                                    ...)
        if (!is.null(details$error)) {
          cached_result <- tryCatch(pin_get(name, board = board_default()), error = function(e) NULL)
          if (is.null(cached_result)) stop(details$error) else warning(details$error)
          return(cached_result)
        }
      }

      if (details$something_changed) {
        if (dir.exists(single_path)) {
          file.copy(dir(single_path, full.names = TRUE) , store_path, recursive = TRUE)
        }
        else {
          file.copy(single_path, store_path, recursive = TRUE)
        }

        something_changed <- TRUE
      }
    }
  }

  if (something_changed) {
    if (!pin_manifest_exists(store_path)) {
      metadata$description <- description
      metadata$type <- type

      metadata <- pins_merge_custom_metadata(metadata, custom_metadata)

      pin_manifest_create(store_path, metadata, dir(store_path, recursive = TRUE))
    }

    board_pin_create(board, store_path, name = name, metadata = metadata, ...)

    ui_viewer_updated(board)
  }

  pin_get(name, board$name, ...) %>%
    invisible_maybe()
}

invisible_maybe <- function(e) {
  if (getOption("pins.invisible", TRUE)) invisible(e) else I(e)
}
