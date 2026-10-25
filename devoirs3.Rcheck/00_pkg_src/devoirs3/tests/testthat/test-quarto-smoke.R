test_that("a minimal Quarto document renders with the package installed", {
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

  old_r_libs <- Sys.getenv("R_LIBS", unset = NA_character_)
  on.exit({
    if (is.na(old_r_libs)) {
      Sys.unsetenv("R_LIBS")
    } else {
      Sys.setenv(R_LIBS = old_r_libs)
    }
  }, add = TRUE)
  extra_libs <- if (is.na(old_r_libs) || old_r_libs == "") {
    character()
  } else {
    strsplit(old_r_libs, .Platform$path.sep, fixed = TRUE)[[1]]
  }
  Sys.setenv(R_LIBS = paste(c(package_lib, extra_libs), collapse = .Platform$path.sep))

  render_output <- system2(
    quarto,
    c("render", shQuote(qmd_file), "--to", "html", "--output-dir", shQuote(output_dir)),
    stdout = TRUE,
    stderr = TRUE
  )
  render_status <- attr(render_output, "status")
  if (!is.null(render_status) && render_status != 0) {
    stop(paste(render_output, collapse = "\n"))
  }

  rendered_file <- file.path(output_dir, "quarto-smoke.html")
  expect_true(file.exists(rendered_file), info = paste(render_output, collapse = "\n"))
  rendered_html <- paste(readLines(rendered_file, warn = FALSE), collapse = "\n")
  expect_match(rendered_html, "DEVOIRS3_SMOKE_OK", fixed = TRUE)
})
