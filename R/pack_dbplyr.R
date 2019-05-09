dplyr_dependencies <- function() {
  if (!"dplyr" %in% installed.packages()) stop("Package 'dplyr' needs to be installed to process a 'dplyr' pin.")

  list (
    sql_render = get("sql_render", envir = asNamespace("dbplyr")),
    tbl = get("tbl", envir = asNamespace("dplyr")),
    sql = get("sql", envir = asNamespace("dplyr")),
    count = get("count", envir = asNamespace("dplyr")),
    pull = get("pull", envir = asNamespace("dplyr")),
    head = get("head", envir = asNamespace("dplyr")),
    remote_con = get("remote_con", envir = asNamespace("dbplyr"))
  )
}

pin_pack.tbl_sql <- function(x, name, board, ...) {
  deps <- dplyr_dependencies()
  params <- list(...)

  sql <- x %>% deps$sql_render() %>% as.character()
  con <- deps$remote_con(x)

  if (identical(params$connection, NULL)) {
    con_maybe <- objects(envir = sys.frame())
    con_search <- Filter(function(x) identical(con, get(x, envir = sys.frame())), con_maybe)
    if (length(con_search) < 1) {
      stop(
        "Can't find global connection for 'dply' expression. ",
        "Try setting connection explicitly with 'connection' parameter"
      )
    }

    con_name <- con_search[[1]]
  }
  else {
    if (!is.character(params$connection))
      stop("The 'connection' parameter must be the name of the object containing the connection.")

    con_name <- params$connection
  }

  result <- list(
    sql = sql,
    connection_name = con_name,
    connection_pin = attr(con, "pin_name")
  )

  result <- as.character(jsonlite::toJSON(result))
  attr(result, "pin_type") <- "dbplyr"
  result
}

pin_unpack.dbplyr_pin <- function(x, board, name, ...) {
  x <- jsonlite::fromJSON(x)

  deps <- dplyr_dependencies()
  params <- list(...)

  if (!identical(x$connection_name, NULL) &&
      exists(x$connection_name, envir = sys.frame())) {
    con <- get(x$connection_name, envir = sys.frame())
  }
  else if (!identical(x$connection_pin, NULL)) {
    con <- get_pin(x$connection_pin, board = board)
    assign(x$connection_name, con, envir = sys.frame())
  }

  deps$tbl(con, deps$sql(x$sql))
}

pin_metadata.tbl_sql <- function(x) {
  deps <- dplyr_dependencies()

  pins_metadata_create(deps$pull(deps$count(x)), ncol(x))
}

pin_preview.tbl_sql <- function(x) {
  deps <- dplyr_dependencies()

  deps$head(x, n = 1000)
}
