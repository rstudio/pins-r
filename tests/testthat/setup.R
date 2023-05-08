options(pins.verbose = FALSE)
options(pins.quiet = TRUE)

httpbin <- webfakes::local_app_process(
  if (rlang::is_installed("webfakes")) webfakes::httpbin_app(),
  .local_envir = testthat::teardown_env()
)

redact_port <- function(snapshot) {
  snapshot <- gsub(httpbin$get_port(), "<port>", snapshot, fixed = TRUE)
}
