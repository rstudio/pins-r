database_dependencies <- function() {
  if (!"DBI" %in% installed.packages()) stop("Package 'DBI' needs to be installed to use a 'database' board.")

  list (
    list_tables = get("dbListTables", envir = asNamespace("DBI")),
    write_table = get("dbWriteTable", envir = asNamespace("DBI")),
    send_query = get("dbSendQuery", envir = asNamespace("DBI")),
    get_query = get("dbGetQuery", envir = asNamespace("DBI")),
    interpolate = get("sqlInterpolate", envir = asNamespace("DBI")),
    append_table = get("sqlAppendTable", envir = asNamespace("DBI"))
  )
}

board_initialize.database <- function(board, ...) {
  if (!"con" %in% names(board)) board$con <- list(...)[[1]]

  board$table_preffix <- "Pins"

  board
}

database_auto_table <- function(prefix = "table") {
  database_sanitize_table(
    paste0("Data", toupper(basename(tempfile(""))))
  )
}

database_sanitize_table <- function(name) {
  parts <- strsplit(name, "[^a-zA-Z]")[[1]]
  parts <- parts[nchar(parts) > 1]
  paste0(tools::toTitleCase(parts), collapse = "")
}

database_sanitize_column <- function(name) {
  parts <- strsplit(name, "[^a-zA-Z0-9_]")[[1]]
  parts <- parts[nchar(parts) > 1]
  paste0(parts, collapse = "_")
}

database_index_table <- function(board) {
  database_sanitize_table(paste0(board$table_preffix, "Index"))
}

pin_create.database <- function(board, dataset, name, description, type, metadata) {
  if (!is.data.frame(dataset)) {
    stop("Only data frames are supported in 'database' boards.")
  }

  deps <- database_dependencies()

  table_name <- paste0(board$table_preffix, database_sanitize_table(name))
  if (table_name %in% deps$list_tables(board$con)) {
    table_name <- paste0(board$table_preffix, database_auto_table())
  }

  colnames(dataset) <- sapply(colnames(dataset), database_sanitize_column)

  deps$write_table(
    board$con,
    table_name,
    dataset,
    overwrite = TRUE
  )

  table_index <- database_index_table(board)

  entry <- data.frame(
    table = table_name,
    name = name,
    type = type,
    description = description,
    metadata = as.character(metadata)
  )

  if (!table_index %in% deps$list_tables(board$con)) {
    deps$write_table(
      board$con,
      table_index,
      entry
    )
  }
  else {
    delete_sql <- deps$interpolate(
      board$con,
      paste0("DELETE FROM ", table_index, " WHERE name = ?name"),
      name = name)
    deps$send_query(board$con, delete_sql)

    insert_sql <- deps$append_table(board$con, table_index, entry)
    deps$send_query(board$con, insert_sql)
  }
}

database_find_pins <- function(board, text) {
  deps <- database_dependencies()

  table_index <- database_index_table(board)
  if (!table_index %in% deps$list_tables(board$con)) {
    data.frame(name = c(), description = c(), type = c(), metadata = c())
  }
  else {
    deps$get_query(board$con, paste0("SELECT * FROM ", table_index))
  }
}

pin_find.database <- function(board, text, ...) {
  index_table <- database_find_pins(board, text)

  data.frame(
    name = index_table$name,
    description = index_table$description,
    type = index_table$type,
    metadata = index_table$metadata,
    stringsAsFactors = FALSE
  )
}

pin_retrieve.database <- function(board, name, details) {
  deps <- database_dependencies()

  index_table <- database_find_pins(board)

  entry <- index_table[index_table$name == name,]
  table_name <- database_sanitize_table(entry$table)

  deps$get_query(board$con, paste0("SELECT * FROM ", table_name))
}

pin_remove.database <- function(board, name) {

}
