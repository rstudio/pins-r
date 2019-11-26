
for (file in dir("vignettes", recursive = TRUE, full.names = TRUE, pattern = "*.Rmd")) {
  content <- readLines(file)
  content <- gsub(".png)", ".jpg)", content, fixed = TRUE)
  writeLines(content, file)
}

image_files <- dir("vignettes/images", full.names = T)
for (file in image_files) {
  message("Processing ", file)

  target_file <- paste(tools::file_path_sans_ext(file), ".jpg")
  system2("convert", c(file, "-resize", "20%", target_file))

  unlink(file)
}

# R CMD build --resave-data pins
