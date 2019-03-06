local_path <- function() {
  paths <- list(
    unix = "~/pinboard",
    windows = "%LOCALAPPDATA%/pinboard"
  )

  path <- paths[[.Platform$OS.type]]

  if (!identical(getOption("pinboard.path"), NULL)) path <- getOption("pinboard.path")

  if (!dir.exists(path)) dir.create(path, recursive = TRUE)

  path
}

local_random_string <- function(prefix, suffix = "") {
  paste0(basename(tempfile(prefix)), suffix)
}

local_entries_path <- function() {
  file.path(local_path(), "config.yml")
}

local_load_entries <- function() {
  entries_path <- local_entries_path()

  if (file.exists(entries_path)) yaml::read_yaml(entries_path) else list()
}

local_save_entries <- function(entries) {
  yaml::write_yaml(entries, local_entries_path())
}

pin_create.local <- function(config, dataset) {
  path <- local_path()
  entries <- local_load_entries()

  config$path <- file.path(path, local_random_string("dataset_", ".rds"))

  saveRDS(dataset, config$path)

  entries <- c(entries, list(
    config
  ))

  local_save_entries(entries)
}

pin_list.local <- function(config, name, contains) {
  entries <- local_load_entries()

  names <- sapply(entries, function(e) e$name)
  descriptions <- sapply(entries, function(e) e$description)

  data.frame(
    name = names,
    description = descriptions,
    stringsAsFactors = FALSE
  )
}
