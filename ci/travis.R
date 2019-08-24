parent_dir <- dir(".", full.names = TRUE)
pins_package <- parent_dir[grepl("pins_", parent_dir)]
install.packages(pins_package, repos = NULL, type = "source")

on.exit(setwd(".."))
setwd("tests")
source("testthat.R")
