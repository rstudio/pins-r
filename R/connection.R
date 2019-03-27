#' Describe Connection
#'
#' Describe a connection to be able to \code{pin()} it.
#'
#' @param initializer Function used to connect. Defaults to \code{"DBI::dbConnect"}.
#' @param driver The name of the driver with an optional library
#' @param ... Additional parameters to initialize the connection.
#'
#' @export
connection <- function(initializer = "DBI::dbConnect", driver = NULL, ...) {
  if (identical(initializer, "DBI::dbConnect") && is.null(driver)) {
    stop("Parameter 'driver' required for 'DBI' connection initializer.")
  }

  list(
    initializer = initializer,
    driver = driver,
    params = list(...)
  )
}


