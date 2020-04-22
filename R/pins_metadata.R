pins_merge_custom_metadata <- function(metadata, custom_metadata) {
  fixed_fields <- c("rows",
                    "cols",
                    "name",
                    "description")

  for (entry in names(custom_metadata)) {
    if (identical(entry, "columns")) {
      fixed_columnn_fields <- c("name", "type")

      # convert to list of columns
      if (is.vector(metadata$columns)) {
        metadata$columns <- lapply(seq_along(metadata$columns), function(e) list(name = names(metadata$columns)[[e]], type = metadata$columns[[e]]))
      }

      if (is.data.frame(custom_metadata$columns)) {
        custom_metadata$columns <- custom_metadata$columns %>%
          jsonlite::toJSON() %>% jsonlite::fromJSON(simplifyDataFrame = FALSE)
      }

      for (column in custom_metadata$columns) {
        found_idx <- Filter(function(e) identical(metadata$columns[[e]]$name, column$name), seq_along(metadata$columns))

        if (identical(length(found_idx), 1L)) {
          for (field_name in names(column)) {
            if (!field_name %in% fixed_columnn_fields) {
              metadata$columns[[found_idx]][[field_name]] <- column[[field_name]]
            }
          }
        }
      }
    }
    else if (!entry %in% fixed_fields) {
      metadata[[entry]] <- custom_metadata[[entry]]
    }
  }

  metadata
}
