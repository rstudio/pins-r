options(pins.verbose = FALSE)
options(pins.quiet = TRUE)

httpbin <- webfakes::local_app_process(
  if (rlang::is_installed("webfakes")) webfakes::httpbin_app(),
  .local_envir = testthat::teardown_env()
)

httpbin_port <- if (rlang::is_installed("webfakes")) httpbin$get_port()

redact_port <- function(snapshot) {
  snapshot <- gsub(httpbin_port, "<port>", snapshot, fixed = TRUE)
}
