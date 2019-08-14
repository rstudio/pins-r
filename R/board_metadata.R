board_metadata_to_text <- function(metadata, text) {
  if (!is.character(text)) text <- ""

  trimmed <- trimws(text)

  if (nchar(trimmed) == 0)
    spacer <- ""
  else if (substr(trimmed, nchar(trimmed), nchar(trimmed)) == ".")
    spacer <- " "
  else
    spacer <- ". "

  if (identical(metadata$type, "table")) {
    paste0(text, spacer, "A table pin with ", metadata$rows, " rows and ", metadata$cols, " columns.")
  }
  else if (identical(metadata$type, "files")) {
    paste0(text, spacer, "A files pin with ", metadata$extension, " extension.")
  }
  else {
    text
  }
}

board_metadata_from_text <- function(text) {
  if (is.null(text)) text <- ""

  patterns <- list(
    table = list(
      regex = "A table pin with ([0-9]+) rows and ([0-9]+) columns",
      metadata = function(match) list(type = "table", rows = as.integer(match[2]), cols = as.integer(match[3]))
    ),
    files = list(
      regex = "A files pin with ([a-z]+) extension",
      metadata = function(match) list(type = "files", extension = match[2])
    )
  )

  for (pattern in patterns) {
    if (grepl(pattern$regex, text)) {
      matches <- regexec(pattern$regex, text)

      return(pattern$metadata(regmatches(text, matches)[[1]]))
    }
  }

  list()
}

board_metadata_remove <- function(text) {
  gsub("A (table|tiles) pin with .*", "", text)
}
