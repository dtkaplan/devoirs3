test_that("a minimal Quarto document renders with the package installed", {
  quarto <- Sys.which("quarto")
  skip_if(quarto == "", "Quarto is not installed")

  test_root <- normalizePath(test_path("..", ".."), mustWork = TRUE)
  package_dir <- if (dir.exists(file.path(test_root, "R"))) {
    test_root
  } else {
    file.path(test_root, "00_pkg_src", "devoirs3")
  }
  package_dir <- normalizePath(package_dir, mustWork = TRUE)
  qmd_file <- test_path("fixtures", "quarto-smoke.qmd")
  package_lib <- tempfile("devoirs3-lib-")
  output_dir <- tempfile("devoirs3-quarto-")
  dir.create(package_lib)
  dir.create(output_dir)
  on.exit(unlink(c(package_lib, output_dir), recursive = TRUE), add = TRUE)

  old_r_libs <- Sys.getenv("R_LIBS", unset = NA_character_)
  on.exit({
    if (is.na(old_r_libs)) {
      Sys.unsetenv("R_LIBS")
    } else {
      Sys.setenv(R_LIBS = old_r_libs)
    }
  }, add = TRUE)
  install_output <- system2(
    file.path(R.home("bin"), "R"),
    c("CMD", "INSTALL", paste0("--library=", shQuote(package_lib)), shQuote(package_dir)),
    stdout = TRUE,
    stderr = TRUE
  )
  install_status <- attr(install_output, "status")
  if (!is.null(install_status) && install_status != 0) {
    stop(paste(install_output, collapse = "\n"))
  }
  Sys.setenv(R_LIBS = paste(c(package_lib, .libPaths()), collapse = .Platform$path.sep))

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
  expect_match(rendered_html, "DEVOIRS3_CHOICES_OK", fixed = TRUE)
  expect_false(grepl("smokeChoices", rendered_html, fixed = TRUE))
  expect_match(rendered_html, "data-devoirs3-widget=\"text\"", fixed = TRUE)
  expect_match(rendered_html, "data-docid=\"quarto-smoke\"", fixed = TRUE)
  expect_match(rendered_html, "data-label=\"response\"", fixed = TRUE)
  expect_match(rendered_html, "data-devoirs3-widget=\"selection\"", fixed = TRUE)
  expect_match(rendered_html, "data-label=\"choice\"", fixed = TRUE)
  expect_match(rendered_html, "data-feedback-enabled=\"true\"", fixed = TRUE)
  expect_match(rendered_html, "data-devoirs3-collector=\"true\"", fixed = TRUE)
  expect_match(rendered_html, "data-devoirs3-labels=\"true\"", fixed = TRUE)
  expect_match(rendered_html, "data-feedback=\"Correct: $x^2$\"", fixed = TRUE)
  expect_match(rendered_html, "function normalizeMathDelimiters(text)", fixed = TRUE)
  expect_match(rendered_html, 'widget.dataset.multiple === "true" && !event.target.checked', fixed = TRUE)
  expect_match(rendered_html, "typesetPromise([feedback])", fixed = TRUE)
  expect_match(rendered_html, "font-size: 6pt", fixed = TRUE)
  expect_match(rendered_html, "background-color: #f1f7fc", fixed = TRUE)
  expect_match(rendered_html, "margin-right: 1em", fixed = TRUE)
  expect_match(rendered_html, "font-style: italic", fixed = TRUE)
  expect_match(rendered_html, "color: #666", fixed = TRUE)
  expect_match(rendered_html, "data-devoirs3-css=", fixed = TRUE)
  expect_match(rendered_html, "document.createElement('style')", fixed = TRUE)
  expect_false(grepl("<p>.devoirs3-feedback", rendered_html, fixed = TRUE))
  expect_match(rendered_html, "localStorage.getItem", fixed = TRUE)
  expect_match(rendered_html, "function createSubmission()", fixed = TRUE)
  expect_match(rendered_html, "schemaVersion: 1", fixed = TRUE)
  expect_match(rendered_html, "button.textContent = \"Copy answers\"", fixed = TRUE)
  expect_match(rendered_html, 'document.createTextNode("Labels?")', fixed = TRUE)
  expect_match(rendered_html, "fallback.readOnly = true", fixed = TRUE)
})
