library(testthat)
library(pins)

# When testing from R CMD check, don't write into standard config directories
Sys.setenv(R_USER_CACHE_DIR = tempfile(), R_USER_DATA_DIR = tempfile())

test_check("pins")
