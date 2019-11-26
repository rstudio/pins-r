# R CMD build --resave-data pins

for (file in dir("vignettes", recursive = TRUE, full.names = TRUE, pattern = "*.Rmd")) {
  content <- readLines(file)
  content <- gsub(".png)", ".jpg)", content, fixed = TRUE)
  writeLines(content, file)
}

for (file in dir("vignettes/images", full.names = T)) {
  system2("convert", c(file, "-resize", "20%", file))
}
