library(htmltools)

pins_site_gallery_entry <- function(entry) {
  div(
    class = "row use-case",
    a(href = entry$href,
      img(src = entry$image)
    ),
    div(
      h3(entry$title),
      entry$description,
      br(),
      a(class = "pin-link",
        href = entry$app,
        "Preview Pin")
    )
  )
}

pins_site_gallery <- function(entries) {
  rows <- tagList()

  for (entry in entries) {
    rows <- tagAppendChild(rows, pins_site_gallery_entry(entry))
  }

  rows
}
