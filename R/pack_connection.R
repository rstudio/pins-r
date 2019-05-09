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

pin_pack.connection <- function(x, name, board, ...) {
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

#' Pin Connection
#'
#' Describe a connection to be able to \code{pin()} it.
#'
#' @param name The name for this pin.
#' @param initializer Function used to connect. Defaults to \code{"DBI::dbConnect"}.
#' @param driver The name of the driver with an optional library
#' @param ... Additional parameters to initialize the connection.
#'
#' @export
pin_connection <- function(name, initializer = "DBI::dbConnect", driver = NULL, ...) {
  if (identical(initializer, "DBI::dbConnect") && is.null(driver)) {
    stop("Parameter 'driver' required for 'DBI' connection initializer.")
  }

  params <- list(...)

  secrets <- intersect(names(params), connection_secrets())
  for (secret in secrets)
    params[[secret]] <- "@prompt"

  pin(
    name,
    structure(
      list(
        initializer = initializer,
        driver = driver,
        params = params
      ),
      class = "connection"
    )
  )
}

connection_secrets <- function() {
  c(
    "user",
    "username",
    "password"
  )
}
