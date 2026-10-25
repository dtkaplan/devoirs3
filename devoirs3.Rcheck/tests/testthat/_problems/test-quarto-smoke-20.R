# Extracted from test-quarto-smoke.R:20

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "devoirs3", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
quarto <- Sys.which("quarto")
skip_if(quarto == "", "Quarto is not installed")
package_dir <- normalizePath(test_path("..", ".."), mustWork = TRUE)
qmd_file <- test_path("fixtures", "quarto-smoke.qmd")
package_lib <- tempfile("devoirs3-lib-")
output_dir <- tempfile("devoirs3-quarto-")
dir.create(package_lib)
dir.create(output_dir)
install_output <- system2(
    file.path(R.home("bin"), "R"),
    c("CMD", "INSTALL", "--library", shQuote(package_lib), shQuote(package_dir)),
    stdout = TRUE,
    stderr = TRUE
  )
install_status <- attr(install_output, "status")
if (!is.null(install_status) && install_status != 0) {
    stop(paste(install_output, collapse = "\n"))
  }
