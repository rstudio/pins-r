pin_split_owner <- function(name) {
  parts <- strsplit(name, "/")[[1]]
  list(
    owner = if (length(parts) > 1) paste(parts[1:length(parts) - 1], collapse = "/") else NULL,
    name = if (length(parts) > 0) parts[length(parts)] else NULL
  )
}

pin_content_name <- function(name) {
  if (is.character(name)) pin_split_owner(name)$name else name
}

pin_content_owner <- function(name) {
  if (is.character(name)) pin_split_owner(name)$owner else NULL
}

pin_value_or_null <- function(expr, can_fail) {
  if (can_fail) {
    tryCatch(force(expr), error = function(e) {
      warning(e$message)
      NULL
    })
  }
  else {
    force(expr)
  }
}

pin_get_and_cache <- function(board, name) {
  cache_path <- file.path(board_local_storage(board), name)
  pin_json <- file.path(cache_path, "pin.json")
  can_fail <- dir.exists(cache_path)

  local_path <- pin_value_or_null(board_pin_get(board_get(board), name), can_fail)

  if (!can_fail || !is.null(local_path)) {
    if (dir.exists(cache_path)) unlink(cache_path, recursive = TRUE)
    dir.create(cache_path, recursive = TRUE)
    file.copy(local_path, cache_path, recursive = TRUE)
    result <- dir(cache_path, full.names = TRUE)
    attr(result, "pin_type") <- attr(local_path, "pin_type")
    jsonlite::write_json(list(type = attr(result, "pin_type")), pin_json)
  }
  else {
    result <- dir(cache_path, full.names = TRUE)
    result <- result[!grepl("pin\\.json", result)][[1]]
    attr(result, "pin_type") <- jsonlite::read_json(pin_json)$type
  }

  result
}

