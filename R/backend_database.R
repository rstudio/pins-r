board_dependencies <- function() {
  if (!"DBI" %in% installed.packages()) stop("Package 'DBI' needs to be installed to use a 'database' board.")

  list (
    list_tables = get("dbListTables", envir = asNamespace("DBI")),
    write_table = get("dbWriteTable", envir = asNamespace("DBI"))
  )
}

board_initialize.database <- function(board, ...) {
  board <- list(...)

  if (!"con" %in% names(board)) board$con <- board[[1]]

  board$table_preffix <- "Pins"

  board
}

database_auto_table <- function(prefix = "table") {
  paste0("Data", toupper(basename(tempfile(""))))
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

pin_create.database <- function(board, dataset, name, description) {
  if (!is.data.frame(dataset)) {
    stop("Only data frames are supported in 'databse' boards.")
  }

  deps <- board_dependencies()

  table_name <- paste0(board$table_preffix, database_sanitize_table(name))
  if (table_name %in% deps$list_tables(con)) {
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
    metadata = as.character(jsonlite::toJSON(list(
      description = description
    )))
  )

  if (!table_index %in% deps$list_tables(con)) {
    deps$write_table(
      board$con,
      table_index,
      entry
    )
  }
  else {
    delete_rows <- DBI::sqlInterpolate(
      con,
      "DELETE FROM ?table WHERE name = ?name",
      table = table_index,
      name = name)

    DBI::sqlAppendTable(con, table_index, entry)
  }
}

pin_find.database <- function(board, name) {
  deps <- board_dependencies()

  table_index <- database_index_table(board)
  if (!table_index %in% deps$list_tables(con)) {
    data.frame(name = c(), description = c())
  }
  else {
    DBI::dbGetQuery(board$con, paste0("SELECT * FROM ", table_index))
  }
}

pin_retrieve.database <- function(board, name) {

}

pin_remove.database <- function(board, name) {

}
