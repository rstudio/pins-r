pins_local_path <- function(component) {
  paths <- list(
    unix = "~/pins",
    windows = "%LOCALAPPDATA%/pins"
  )

  path <- paths[[.Platform$OS.type]]

  if (!identical(getOption("pins.path"), NULL)) path <- getOption("pins.path")

  component_path <- file.path(path, component)

  if (!dir.exists(component_path)) dir.create(component_path, recursive = TRUE)

  normalizePath(component_path, mustWork = FALSE)
}
