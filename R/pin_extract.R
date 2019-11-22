pin_extract <- function(file, destination) {
  UseMethod("pin_extract")
}

pin_extract.zip <- function(file, destination) {
  pin_log("Extracting zip file '", file, "'")
  zip::unzip(file, exdir = destination)
  unlink(file)
}

pin_extract.gzip <- function(file, destination) {
  if (length(find.package("R.utils", quiet = TRUE)) == 0)
    warning("To extract gzip pins install the 'R.utils' package")
  else {
    gunzip <- get_function("gunzip", "R.utils")
    gunzip(file, destname = file.path(destination, gsub(".gz", "", basename(file), fixed = TRUE)))
  }
}

`pin_extract.compressed-tar` <- function(file, destination) {
  pin_log("Extracting tgz file '", file, "'")
  utils::untar(file, exdir = destination)
  unlink(file)
}

pin_extract.default <- function(file, destination) {
  ext_map <- list(
    "\\.tar\\.gz$" = `pin_extract.compressed-tar`,
    "\\.zip$" = pin_extract.zip,
    "\\.gz$" = pin_extract.gzip
  )

  matches <- sapply(names(ext_map), function(e) grepl(e, file))
  if (any(matches)) ext_map[[names(which(matches)[1])]](file, destination)
}
