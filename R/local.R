pins_local_path <- function(component) {
  paths <- list(
    unix = "~/pins",
    windows = "%LOCALAPPDATA%/pins"
  )

  path <- paths[[.Platform$OS.type]]

  if (!identical(getOption("pins.path"), NULL)) path <- getOption("pins.path")

  if (!dir.exists(path)) dir.create(path, recursive = TRUE)

  normalizePath(file.path(path, component))
}
