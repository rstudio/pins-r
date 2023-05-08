library(pins)
args <- commandArgs(trailingOnly = TRUE)

board <- board_folder(args[1])
res <- board %>% pin_read("mtcars-py")
testthat::expect_equal(res, datasets::mtcars, ignore_attr = TRUE)
