board_packages <- function() {
  new_board_v0("pins_board_packages",
    name = "packages",
    versions = FALSE
  )
}

#' @export
board_pin_find.pins_board_packages <- function(board, text, ...) {
  raw <- utils::data()$results
  results <- data.frame(
    package = raw[, "Package"],
    dataset = raw[, "Item"],
    description = raw[, "Title"]
  )
  results$name <- paste0(results$package, "/", results$dataset)

  if (!is.null(text)) {
    find_names <- grepl(text, results$dataset, ignore.case = TRUE)
    find_description <- grepl(text, results$description, ignore.case = TRUE)
    results <- results[find_names | find_description, , drop = FALSE]
  }

  # Match expected data frame structure
  data.frame(
    name = results$name,
    description = results$description,
    cols = NA_integer_,
    rows = NA_integer_,
    class = NA_character_,
    metadata = NA_character_
  )
}

#' @export
board_pin_get.pins_board_packages <- function(board, name, ...) {
  if (!is_string(name)) {
    abort("A package pin must be a string")
  }

  pieces <- strsplit(name, "/")[[1]]
  if (length(pieces) != 2) {
    abort("A package pin must have structure 'package/dataset'")
  }

  check_installed(pieces[[1]])

  # Save data to temporary directory in order to match pin_get() interface
  path <- fs::dir_create(fs::file_temp())

  env <- new_environment()
  utils::data(list = pieces[[2]], package = pieces[[1]], envir = env)
  if (env_length(env) != 1) {
    abort(paste0("'", name, "' isn't a single dataset"))
  }
  saveRDS(as.list(env)[[1]], fs::path(path, "data.rds"))

  pin_manifest_create(path, list(type = "table"), fs::dir_ls(path))
  path
}
