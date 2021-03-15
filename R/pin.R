#' Pin Resource
#'
#' Pins the given resource locally or to the given board.
#'
#' @param x An object, local file or remote URL to pin.
#' @param name The name for the dataset or object.
#' @param description Optional description for this pin.
#' @param board The board where this pin will be placed.
#' @param ... Additional parameters.
#'
#' @details
#'
#' `pin()` allows you to cache remote resources and intermediate results with ease. When
#' caching remote resources, usually URLs, it will check for HTTP caching headers to avoid
#' re-downloading when the remote result has not changed.
#'
#' This makes it ideal to support reproducible research by requiring manual instruction to
#' download resources before running your R script.
#'
#' In addition, `pin()` still works when working offline or when the remote resource
#' becomes unavailable; when this happens, a warning will be triggered but your code will
#' continue to work.
#'
#' `pin()` will stores data frames in two files, an R native file and a 'CSV' file. To
#' force saving a pin only using R's native (RDS) format, you can use `pin(I(data))`.
#' This can improve performance and size at the cost of making the pin unreadable from other
#' tools and programming languages.
#'
#' @examples
#' library(pins)
#'
#' # define local board
#' board_register_local(cache = tempfile())
#'
#' # cache the mtcars dataset
#' pin(mtcars)
#'
#' # cache computation over mtcars
#' mtcars[mtcars$mpg > 30,] %>%
#'   pin(name = "mtefficient")
#'
#' # retrieve cached pin
#' pin_get("mtefficient")
#'
#' # url to remote resource
#' resource <- file.path("https://raw.githubusercontent.com/facebook/prophet",
#'                       "master/examples/example_retail_sales.csv")
#'
#' # cache remote resource
#' pin(resource, name = "example_retail_sales")
#'
#' # load cached csv
#' pin_get("example_retail_sales") %>% read.csv()
#'
#' # cache and read csv
#' read.csv(pin(resource))
#'
#' @export
pin <- function(x, name = NULL, description = NULL, board = NULL, ...) {
  UseMethod("pin")
}

#' @rdname custom-pins
#' @export
pin_preview <- function(x, board = NULL, ...) {
  UseMethod("pin_preview")
}

#' @rdname custom-pins
#' @export
pin_load <- function(path, ...) {
  UseMethod("pin_load")
}

#' @rdname custom-pins
#' @export
pin_fetch <- function(path, ...) {
  UseMethod("pin_fetch")
}

#' Custom Pins
#'
#' Family of functions meant to be used to implement custom pin extensions, not to be used by users.
#'
#' @param board The board to extended, retrieved with `board_get()`.
#' @param name The name of the pin.
#' @param path The path to store.
#' @param description The text patteren to find a pin.
#' @param type The type of pin being stored.
#' @param metadata A list containing additional metadata describing the pin.
#' @param retrieve Should the pin be retrieved after being created? Defaults to `TRUE`.
#' @param ... Additional parameteres.
#' @keywords internal
#'
#' @export
#' @rdname custom-pins
board_pin_store <- function(board, path, name, description, type, metadata, extract = TRUE, retrieve = TRUE, ...) {
  board <- board_get(board)
  if (is.null(name)) name <- gsub("[^a-zA-Z0-9]+", "_", tools::file_path_sans_ext(basename(path)))[[1]]
  pin_log("Storing ", name, " into board ", board$name, " with type ", type)
  custom_metadata <- list(...)$custom_metadata
  zip <- list(...)$zip

  if (identical(list(...)$cache, FALSE)) pin_register_reset_cache(board, name)

  path <- path[!grepl("data\\.txt", path)]

  store_path <- tempfile()
  dir.create(store_path)
  on.exit(unlink(store_path, recursive = TRUE))

  if (length(path) == 1 && grepl("^http", path) && !grepl("\\.[a-z]{2,4}$", path) && getOption("pins.search.datatxt", TRUE)) {
    # attempt to download data.txt to enable public access to boards like rsconnect
    datatxt_path <- file.path(path, "data.txt")
    local_path <- pin_download(datatxt_path, name, board_default(), can_fail = TRUE)
    if (!is.null(local_path)) {
      manifest <- tryCatch(pin_manifest_get(local_path), error = function(e) { unlink(file.path(local_path, "data.txt")) ; NULL })
      if (!is.null(manifest) && !is.null(manifest$path)) {
        path <- paste(path, manifest$path, sep = "/")
        extract <- FALSE
      }
    }
  }

  if (identical(zip, TRUE)) {
    find_common_path <- function(path) {
      common <- path[1]
      if (all(startsWith(path, common)) || identical(common, dirname(common))) return(common)
      return(force(find_common_path(dirname(common[1]))))
    }

    common_path <- find_common_path(path)
    withr::with_dir(common_path, {
      zip(file.path(store_path, "data.zip"), gsub(paste0(common_path, "/"), "", path), flags="-q")
    })

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
        copy_or_link <- function(from, to) {
          if (file.exists(from) && file.info(from)$size >= getOption("pins.link.size", 10^8) && .Platform$OS.type != "windows")
            fs::link_create(from, file.path(to, basename(from)))
          else
            file.copy(from, to, recursive = TRUE)
        }

        if (dir.exists(single_path)) {
          for (entry in dir(single_path, full.names = TRUE)) {
            copy_or_link(entry, store_path)
          }
        }
        else {
          copy_or_link(single_path, store_path)
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

      for (metaname in names(metadata)) {
        # see issues/127 which requires encoding to prevent windows crashes
        if (is.character(metadata[metaname])) {
          metadata[metaname] <- enc2utf8(metadata[metaname])
        }
      }
    }

    board_pin_create(board, store_path, name = name, metadata = metadata, ...)

    ui_viewer_updated(board)
  }

  if (retrieve) {
    invisible(pin_get(name, board, ...))
  } else {
    invisible(NULL)
  }
}


# default -----------------------------------------------------------------

#' @keywords internal
#' @export
pin.default <- function(x, name = NULL, description = NULL, board = NULL, ...) {
  if (is.null(name)) name <- pin_default_name(deparse(substitute(x)), board)

  path <- tempfile()
  dir.create(path)
  on.exit(unlink(path))

  saveRDS(x, file.path(path, "data.rds"), version = 2)

  board_pin_store(board, path, name, description, "default", list(), ...)
}

#' @keywords internal
#' @export
pin_load.default <- function(path, ...) {
  result <- readRDS(file.path(path, "data.rds"))

  # TODO: figure out why this is needed; can probably remove
  if ("AsIs" %in% class(result)) {
    class(result) <- class(result)[class(result) != "AsIs"]
  }

  result
}

#' @keywords internal
#' @export
pin_preview.default <- function(x, board = NULL, ...) {
  x
}

#' @keywords internal
#' @export
pin_fetch.default <- function(path, ...) {
  path
}


# data.frame --------------------------------------------------------------

#' @keywords internal
#' @export
pin.data.frame <- function(x, name = NULL, description = NULL, board = NULL, ...) {

  # Used to avoid mutation in pins_save_csv
  if ("data.table" %in% class(x)) {
    return(
      pin.default(x, name = name, description = description, board = board, ...)
    )
  }

  if (is.null(name)) name <- pin_default_name(deparse(substitute(x)), board)

  path <- tempfile()
  dir.create(path)
  on.exit(unlink(path))

  saveRDS(x, file.path(path, "data.rds"), version = 2)
  pins_safe_csv(x, file.path(path, "data.csv"))

  metadata <- list(
    rows = nrow(x),
    cols = ncol(x),
    columns = lapply(x, function(e) class(e)[[1]])
  )
  board_pin_store(board, path, name, description, "table", metadata,...)
}

pins_safe_csv <- function(x, name) {
  tryCatch({
    pins_save_csv(x, name)
  }, error = function(e) {
    warning("Failed to save data frame as CSV file")
  })
}

pins_save_csv <- function(x, name) {
  supported_columns <- c(
    "character",
    "numeric",
    "integer",
    "Date",
    "POSIXlt",
    "logical",
    "raw"
  )

  x_class <- unname(sapply(x, function(e) class(e)[[1]]))
  unsupported_columns <- which(!x_class %in% supported_columns)
  for (col_idx in unsupported_columns) {
    x[[col_idx]] <- as.character(x[[col_idx]])
  }

  utils::write.csv(x, name, row.names = FALSE)
}

#' @keywords internal
#' @export
pin_load.table <- function(path, ...) {
  rds <- file.path(path, "data.rds")
  csv <- file.path(path, "data.csv")

  if (file.exists(rds)) result <- readRDS(rds)
  else if (file.exists(csv)) result <- utils::read.csv(csv, stringsAsFactors = FALSE)
  else stop("A 'table' pin requires CSV or RDS files.")

  format_tibble(result)
}

#' @keywords internal
#' @export
pin_fetch.table <- function(path, ...) {
  rds_match <- grepl(".*.rds", path)
  fetch_all <- identical(getOption("pins.fetch", "auto"), "all")
  if (any(rds_match) && !fetch_all) path[rds_match] else path
}

#' @keywords internal
#' @export
pin_preview.data.frame <- function(x, board = NULL, ...) {
  utils::head(x, n = getOption("pins.preview", 10^3))
}

# files -------------------------------------------------------------------

#' @keywords internal
#' @export
pin.character <- function(x, name = NULL, description = NULL, board = NULL, ...) {
  extension <- if (length(x) > 1) "zip" else tools::file_ext(x)
  board_pin_store(board, x, name, description, "files", list(extension = extension), ...)
}

#' @export
pin_load.files <- function(path, ...) {
  files <- dir(path, recursive = TRUE, full.names = TRUE)

  result <- files[!grepl("data\\.txt$", files)]

  format_tibble(result)
}

#' @keywords internal
#' @export
pin_preview.files <- function(x, board = NULL, ...) {
  data.frame(
    files = x,
    stringsAsFactors = FALSE
  )
}


# asis --------------------------------------------------------------------


#' @keywords internal
#' @export
pin.AsIs <- function(x, name = NULL, description = NULL, board = NULL, ...) {
  pin.default(x = x, name = name, description = description, board = board, ...)
}


# package -----------------------------------------------------------------

#' @keywords internal
#' @export
pin_load.package <- function(path, ...) {
  files <- dir(path, full.names = TRUE)
  files <- files[!grepl("data\\.txt$", files)]

  result <- get(load(files))

  format_tibble(result)
}


# helpers -----------------------------------------------------------------


#' Create Pin Name
#'
#' Creates a pin name from an character expression generated with `deparse(substitute(x))`.
#'
#' @param x The expression to generate  the pin name from.
#' @param board The board to which this name is generating for.
#'
#' @export
#' @keywords internal
pin_default_name <- function(x, board) {
  name <- basename(x)

  error <- "Can't auto-generate pin name from object, please specify the 'name' parameter."
  if (length(name) != 1) stop(error)

  sanitized <- gsub("[^a-zA-Z0-9-]", "-", name)
  sanitized <- gsub("^-*|-*$", "", sanitized)
  sanitized <- gsub("-+", "-", sanitized)

  if (nchar(sanitized) == 0) stop(error)

  # kaggle boards require five or more character names
  if (identical(board, "kaggle") && nchar(sanitized) < 5) sanitized <- paste(sanitized, "pin", sep = "-")

  sanitized
}


pins_merge_custom_metadata <- function(metadata, custom_metadata) {
  fixed_fields <- c("rows",
                    "cols",
                    "name",
                    "description")

  for (entry in names(custom_metadata)) {
    if (identical(entry, "columns")) {
      fixed_columnn_fields <- c("name", "type")

      # convert to list of columns
      if (is.vector(metadata$columns)) {
        metadata$columns <- lapply(seq_along(metadata$columns), function(e) list(name = names(metadata$columns)[[e]], type = metadata$columns[[e]]))
      }

      if (is.data.frame(custom_metadata$columns)) {
        custom_metadata$columns <- custom_metadata$columns %>%
          jsonlite::toJSON() %>% jsonlite::fromJSON(simplifyDataFrame = FALSE)
      }

      for (column in custom_metadata$columns) {
        found_idx <- Filter(function(e) identical(metadata$columns[[e]]$name, column$name), seq_along(metadata$columns))

        if (identical(length(found_idx), 1L)) {
          for (field_name in names(column)) {
            if (!field_name %in% fixed_columnn_fields) {
              metadata$columns[[found_idx]][[field_name]] <- column[[field_name]]
            }
          }
        }
      }
    }
    else if (!entry %in% fixed_fields) {
      metadata[[entry]] <- custom_metadata[[entry]]
    }
  }

  metadata
}
