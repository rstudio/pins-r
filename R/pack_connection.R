pin_pack.connection <- function(x, ...) {
  structure(
    jsonlite::toJSON(x),
    class = "connection_pin"
  )
}

retrive_named_function <- function(name) {
  tuple <- strsplit(name, "::")[[1]]
  if (length(tuple) != 2) stop("Named function expect to be formatted 'library::function'.")

  library <- tuple[1]
  func <- tuple[2]

  if (!library %in% installed.packages()) {
    stop("Library '", library, "' is required for function '", func, "'.")
  }

  if (!exists(func, envir = asNamespace(library))) {
    stop("Function '", func, "' does not exist in library '", library, "'.")
  }

  list(
    library = library,
    func = func
  )
}

pin_unpack.connection_pin <- function(x, ...) {
  def <- jsonlite::fromJSON(x)

  initializer <- retrive_named_function(def$initializer)
  params <- def$params

  if (length(def$driver) > 0) {
    driver <- retrive_named_function(def$driver)
    params <- c(
      do.call(driver$func, args = params, envir = driver$library),
      params
    )
  }

  do.call(initializer$func, args = params, envir = asNamespace(initializer$library))
}
