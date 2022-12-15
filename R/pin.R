#' Pin a resource (legacy API)
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
#' `pin()` stores data frames in two files, an R native file (RDS) and a 'CSV' file. To
#' force saving a pin in R's native format only, you can use `pin(I(data))`.
#' This can improve performance and size at the cost of making the pin unreadable from other
#' tools and programming languages.
#'
#' @examples
#' # old API
#' board_register_local(cache = tempfile())
#' pin(mtcars)
#' pin_get("mtcars")
#'
#' # new api
#' board <- board_local()
#' board %>% pin_write(mtcars)
#' board %>% pin_read("mtcars")
#'
#' @export
pin <- function(x, name = NULL, description = NULL, board = NULL, ...) {
  UseMethod("pin")
}

#' @rdname custom-pins
#' @export
pin_load <- function(path, ...) {
  UseMethod("pin_load")
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
#' @param pin_metadata A list of pin metadata describing the pin. Must contain
#'   `type` and `description`.
#' @param metadat Additional user supplied metadata.
#' @param custom_metadata Deprecated. Please use `metadata` instead.
#' @param retrieve Should the pin be retrieved after being created? Defaults to `TRUE`.
#' @param ... Additional parameteres.
#' @keywords internal
#'
#' @export
#' @keywords internal
#' @rdname custom-pins
board_pin_store <- function(board,
                            path,
                            name,
                            pin_metadata,
                            extract = TRUE,
                            retrieve = TRUE,
                            zip = FALSE,
                            cache = TRUE,
                            metadata = NULL,
                            custom_metadata = NULL,
                            ...) {
  check_store_path(path)
  check_store_zip(zip)

  metadata <- modifyList(metadata, pin_metadata)
  if (!is.null(custom_metadata)) {
    warn("`custom_metadata` is deprecated; please use `metadata` instead")
    metadata <- modifyList(custom_metadata, metadata)
  }

  board <- board_get(board)
  pin_log("Storing ", name, " into board ", board$name, " with type ", metadata$type)

  store_path <- withr::local_tempdir()
  for (single_path in path) {
    if (fs::dir_exists(single_path)) {
      for (entry in dir(single_path, full.names = TRUE)) {
        fs::file_copy(entry, store_path)
      }
    } else {
      fs::file_copy(single_path, store_path)
    }
  }

  pin_manifest_create(store_path, metadata, dir(store_path, recursive = TRUE))
  board_pin_create(board, store_path, name = name, metadata = metadata, ...)

  if (retrieve) {
    # Hack to suppress RSC message that you need to use the full name
    suppressMessages(
      invisible(pin_get(name, board, ...))
    )
  } else {
    invisible(NULL)
  }
}

check_store_path <- function(path) {
  path <- path[!grepl("data\\.txt", path)]
  if (length(path) == 1 && is_url(path) && fs::path_ext(path) == "") {
    abort(c(
      "Pin functions no longer supports direct use of data.txt sites",
      i = paste0("Please use `legacy_datatxt('", path, ') instead')
    ))
  }
}

check_store_zip <- function(zip) {
  if (!identical(zip, FALSE)) {
    # neither used nor documented, as far as I can tell
    abort("`zip` argument is no longer supported")
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

  metadata <- pin_metadata("default", description)
  board_pin_store(board, path, name, metadata, ...)
}

#' @keywords internal
#' @export
pin_load.default <- function(path, ...) {
  readRDS(file.path(path, "data.rds"))
}

# data.frame --------------------------------------------------------------

#' @keywords internal
#' @export
pin.data.frame <- function(x, name = NULL, description = NULL, board = NULL, ...) {
  if (is.null(name)) {
    name <- pin_default_name(deparse(substitute(x)), board)
  }

  path <- withr::local_tempdir()
  saveRDS(x, file.path(path, "data.rds"), version = 2)
  pins_safe_csv(x, file.path(path, "data.csv"))

  metadata <- pin_metadata(
    "table",
    description = description,
    rows = nrow(x),
    cols = ncol(x),
    columns = lapply(x, function(e) class(e)[[1]])
  )
  board_pin_store(board, path, name, metadata, ...)
}

pins_safe_csv <- function(x, name) {
  tryCatch(
    {
      pins_save_csv(x, name)
    },
    error = function(e) {
      warning("Failed to save data frame as CSV file")
    }
  )
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

  if (file.exists(rds)) {
    readRDS(rds)
  } else if (file.exists(csv)) {
    utils::read.csv(csv, stringsAsFactors = FALSE)
  } else {
    stop("A 'table' pin requires CSV or RDS files.")
  }
}

# files -------------------------------------------------------------------

#' @keywords internal
#' @export
pin.character <- function(x, name = NULL, description = NULL, board = NULL, cache = TRUE, extract = TRUE, ...) {
  if (is.null(name)) {
    name <- pin_default_name(fs::path_ext_remove(basename(x[[1]])), board)
  }

  if (length(x) == 1 && is_url(x)) {
    board <- board_get(board)
    details <- as.environment(list(something_changed = TRUE))
    path <- pin_download_files(x,
      name,
      board,
      extract = extract,
      details = details,
      can_fail = TRUE,
      cache = cache,
      ...
    )

    # If failed to download, fall back to cached with warning
    if (!is.null(details$error)) {
      old <- tryCatch(pin_get(name, board = board), error = function(e) NULL)
      if (is.null(old)) {
        abort(details$error)
      } else {
        warn(c(
          "Failed to re-download pin; using cached value",
          details$error
        ))
      }
      return(invisible(old))
    }

    x <- path
  }

  extension <- if (length(x) > 1) "zip" else tools::file_ext(x)
  metadata <- pin_metadata("files", description, extension = extension)
  board_pin_store(board, x, name, metadata, ...)
}

#' @export
pin_load.files <- function(path, ...) {
  files <- dir(path, recursive = TRUE, full.names = TRUE)
  result <- files[!grepl("data\\.txt$", files)]
  result
}

# asis --------------------------------------------------------------------

#' @keywords internal
#' @export
pin.AsIs <- function(x, name = NULL, description = NULL, board = NULL, ...) {
  # Force use of default method to avoid special behaviour for character/data.frame
  class(x) <- setdiff(class(x), "AsIs")
  pin.default(x = x, name = name, description = description, board = board, ...)
}

# package -----------------------------------------------------------------

#' @keywords internal
#' @export
pin_load.package <- function(path, ...) {
  files <- dir(path, full.names = TRUE)
  files <- files[!grepl("data\\.txt$", files)]

  get(load(files))
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

pin_metadata <- function(type,
                         description = NULL,
                         ...) {
  type <- match.arg(type, c("default", "files", "table"))
  list(
    type = type,
    description = description,
    ...
  )
}

