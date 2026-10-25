.devoirs3_state <- new.env(parent = emptyenv())
.devoirs3_state$doc_id <- NULL
.devoirs3_state$labels <- character()
.devoirs3_state$assets_emitted <- FALSE
.devoirs3_state$feedback_default <- TRUE
.devoirs3_state$collector <- TRUE
.devoirs3_state$labels_enabled <- TRUE

.devoirs3_assets_markup <- function() {
  js_file <- system.file("devoirs3.js", package = "devoirs3")
  css_file <- system.file("widget.css", package = "devoirs3")
  if (!nzchar(js_file)) {
    js_file <- file.path("inst", "devoirs3.js")
  }
  if (!nzchar(css_file)) {
    css_file <- file.path("inst", "widget.css")
  }
  if (!file.exists(js_file)) {
    stop("Could not locate the devoirs3 JavaScript asset.", call. = FALSE)
  }
  if (!file.exists(css_file)) {
    stop("Could not locate the devoirs3 widget stylesheet.", call. = FALSE)
  }

  script <- paste(readLines(js_file, warn = FALSE), collapse = "\n")
  stylesheet <- paste(readLines(css_file, warn = FALSE), collapse = "\n")
  script <- paste(
    "const style = document.createElement('style');",
    "style.textContent = document.currentScript.dataset.devoirs3Css;",
    "document.head.appendChild(style);",
    script,
    sep = "\n"
  )
  assets <- htmltools::tags$script(
    `data-devoirs3-css` = stylesheet,
    `data-devoirs3-collector` = tolower(as.character(.devoirs3_state$collector)),
    `data-devoirs3-labels` = tolower(as.character(.devoirs3_state$labels_enabled)),
    htmltools::HTML(script)
  )
  as.character(htmltools::renderTags(assets)$html)
}

#' Initialize a devoirs3 document
#'
#' Registers the shared browser behavior and starts a new render-time label
#' registry. Call this once near the beginning of a Quarto document.
#'
#' @param docID A non-empty ID unique to the assignment on its hosting origin.
#' @param feedback Whether selection feedback is enabled by default.
#' @param collector Whether to add the answer collection button.
#' @param labels Whether to add the labels visibility checkbox.
#'
#' @return Invisibly returns `NULL`.
#' @export
devoirs_start <- function(docID, feedback = TRUE, collector = TRUE, labels = TRUE) {
  if (!is.character(docID) || length(docID) != 1L || is.na(docID) || !nzchar(docID)) {
    stop("`docID` must be a single non-empty character string.", call. = FALSE)
  }
  if (!is.logical(feedback) || length(feedback) != 1L || is.na(feedback)) {
    stop("`feedback` must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.logical(collector) || length(collector) != 1L || is.na(collector)) {
    stop("`collector` must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.logical(labels) || length(labels) != 1L || is.na(labels)) {
    stop("`labels` must be TRUE or FALSE.", call. = FALSE)
  }

  .devoirs3_state$doc_id <- docID
  .devoirs3_state$labels <- character()
  .devoirs3_state$assets_emitted <- FALSE
  .devoirs3_state$feedback_default <- feedback
  .devoirs3_state$collector <- collector
  .devoirs3_state$labels_enabled <- labels
  invisible(NULL)
}

#' Create a persistent free-text question
#'
#' Returns accessible HTML for a text-entry question. Call
#' [devoirs_start()] once before adding question elements in a Quarto render.
#'
#' @param label A non-empty ID unique among question elements in the document.
#' @param question The human-readable question prompt used as the control's
#'   accessible name.
#' @param placeholder Optional placeholder text.
#' @param width Number of columns for the text area.
#' @param height Number of rows for the text area.
#' @param inline If `TRUE`, place the controls in an inline wrapper.
#'
#' @return HTML suitable for an inline or block Quarto expression.
#' @export
devoirs_text <- function(label, question, placeholder = "", width = 40L,
                         height = 3L, inline = FALSE) {
  if (is.null(.devoirs3_state$doc_id)) {
    stop("Call `devoirs_start()` before creating question elements.", call. = FALSE)
  }
  if (!is.character(label) || length(label) != 1L || is.na(label) || !nzchar(label)) {
    stop("`label` must be a single non-empty character string.", call. = FALSE)
  }
  if (label %in% .devoirs3_state$labels) {
    stop(sprintf("Label '%s' is duplicated. All labels must be unique within the document.", label), call. = FALSE)
  }
  if (!is.character(question) || length(question) != 1L || is.na(question) || !nzchar(question)) {
    stop("`question` must be a single non-empty character string.", call. = FALSE)
  }
  if (!is.character(placeholder) || length(placeholder) != 1L || is.na(placeholder)) {
    stop("`placeholder` must be a single character string.", call. = FALSE)
  }
  if (!is.numeric(width) || length(width) != 1L || is.na(width) || width < 1 || width != as.integer(width)) {
    stop("`width` must be a positive whole number.", call. = FALSE)
  }
  if (!is.numeric(height) || length(height) != 1L || is.na(height) || height < 1 || height != as.integer(height)) {
    stop("`height` must be a positive whole number.", call. = FALSE)
  }
  if (!is.logical(inline) || length(inline) != 1L || is.na(inline)) {
    stop("`inline` must be TRUE or FALSE.", call. = FALSE)
  }

  .devoirs3_state$labels <- c(.devoirs3_state$labels, label)
  input_id <- paste0("devoirs3-text-", paste(sprintf("%02x", as.integer(charToRaw(enc2utf8(label)))), collapse = ""))
  wrapper <- if (inline) htmltools::tags$span else htmltools::tags$div
  widget <- wrapper(
    class = "devoirs3-text-widget",
    `data-devoirs3-widget` = "text",
    `data-docid` = .devoirs3_state$doc_id,
    `data-label` = label,
    htmltools::tags$label(
      class = "devoirs3-visually-hidden",
      `for` = input_id,
      question
    ),
    htmltools::tags$textarea(
      id = input_id,
      class = "devoirs3-text-input",
      rows = as.integer(height),
      cols = as.integer(width),
      placeholder = placeholder,
      `data-devoirs3-input` = ""
    ),
    htmltools::tags$button(
      type = "button",
      class = "devoirs3-clear-button",
      `data-devoirs3-clear` = "",
      `aria-label` = paste("Clear answer for", question),
      "Clear"
    ),
    htmltools::tags$span(
      class = "devoirs3-label",
      `data-devoirs3-label` = "",
      hidden = TRUE,
      label
    )
  )

  rendered <- as.character(htmltools::renderTags(widget)$html)
  if (!.devoirs3_state$assets_emitted) {
    rendered <- paste0(.devoirs3_assets_markup(), rendered)
    .devoirs3_state$assets_emitted <- TRUE
  }
  knitr::asis_output(rendered)
}