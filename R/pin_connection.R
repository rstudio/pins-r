connection_dependencies <- function() {
  if (!"rstudioapi" %in% installed.packages()) {
    list(
      rstudio_available = function() {
        FALSE
      },
      ask_for_secret = function(secret, name) {
        readline(paste0(name, ": "))
      }
    )
  }
  else {
    list (
      rstudio_available = get("isAvailable", envir = asNamespace("rstudioapi")),
      ask_for_secret = get("askForSecret", envir = asNamespace("rstudioapi"))
    )
  }
}

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

pin_unpack.connection_pin <- function(x, board, name, ..) {
  deps <- connection_dependencies()
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

  prompts <- Filter(function(p) identical(params[[p]], "@prompt"), names(params))
  for (prompt in prompts) {
    params[[prompt]] <- deps$ask_for_secret(paste0("Pin ", name), prompt)
  }

  do.call(initializer$func, args = params, envir = asNamespace(initializer$library))
}
