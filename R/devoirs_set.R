#' Compact inline generation of fixed selection question elements
#'
#' @param choices Vector containing the member of the set to use as distractors
#' @param correct Number or character string with the <correct> answer.
#' @param n integer, number of distractors
#' @param label character string. File-unique id for this problem.
#' @param sort logical value. Force sorting on character strings. (Numerical choices are sorted by default.)
#' @param inline logical value. Place the items in an inline format.
#' @param feedback Whether to display feedback. `NULL` inherits the current
#'   mode set by [feedback_mode()] or [devoirs_start()].
#' @export
devoirs_set3 <- function(
    choices = 1:5,
    correct = 3,
    n = 5,
    label = NULL,
    sort = is.numeric(correct),
    inline = TRUE,
    feedback = NULL
) {
  if (!is.atomic(choices) || !is.null(dim(choices)) || anyNA(choices)) {
    stop("choices must be a non-missing atomic vector", call. = FALSE)
  }
  if (!is.atomic(correct) || !length(correct) || anyNA(correct)) {
    stop("correct must be a non-empty, non-missing atomic vector", call. = FALSE)
  }
  if (!is.numeric(n) || length(n) != 1L || is.na(n) || !is.finite(n) ||
      n < 0 || n != as.integer(n)) {
    stop("n must be one non-negative integer", call. = FALSE)
  }
  if (!is.logical(sort) || length(sort) != 1L || is.na(sort)) {
    stop("sort must be TRUE or FALSE", call. = FALSE)
  }
  if (!is.logical(inline) || length(inline) != 1L || is.na(inline)) {
    stop("inline must be TRUE or FALSE", call. = FALSE)
  }

  correct <- unique(correct)
  if (is.null(label)) {
    label <- paste0(sample(c(letters, LETTERS, as.character(0:9)), 5L, replace = TRUE), collapse = "")
  }
  if (!is.character(label) || length(label) != 1L || is.na(label) || !nzchar(label)) {
    stop("label must be one non-empty character string", call. = FALSE)
  }

  selected_choices <- choices
  if (length(selected_choices) > n) {
    selected_choices <- selected_choices[sample.int(length(selected_choices), as.integer(n))]
  }
  missing_correct <- correct[!correct %in% selected_choices]
  answer_values <- c(selected_choices, missing_correct)
  if (!length(answer_values)) {
    stop("at least one choice is required", call. = FALSE)
  }
  if (sort) {
    answer_values <- base::sort(answer_values)
  }

  answers <- lapply(answer_values, function(value) {
    list(
      text = as.character(value),
      correct = value %in% correct
    )
  })
  render_selection <- devoirs3::devoirs_selection(
    label = label,
    question = "Placeholder for now",
    answers = answers,
    multiple = length(correct) > 1L
  )
  render_selection(inline = inline, feedback = feedback)
}

#' Create an inline true-or-false question
#'
#' @param correct Whether the first choice is correct.
#' @param label A non-empty ID unique among question elements in the document.
#' @param format Two choice labels separated by optional middle words, with
#'   optional ending punctuation.
#' @param wrong_hint Feedback shown for an incorrect choice.
#' @param right_hint Feedback shown for a correct choice.
#' @param feedback Whether to display feedback. `NULL` inherits the current
#'   mode set by [feedback_mode()] or [devoirs_start()].
#'
#' @return HTML suitable for an inline Quarto expression.
#' @export
devoirs_TF <- function(
    correct,
    label,
    format = "True or False?",
    wrong_hint = "Sorry",
    right_hint = "Nice!",
    feedback = NULL
) {
  if (is.null(.devoirs3_state$doc_id)) {
    stop("Call `devoirs_start()` before creating question elements.", call. = FALSE)
  }
  if (!is.logical(correct) || length(correct) != 1L || is.na(correct)) {
    stop("`correct` must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.character(label) || length(label) != 1L || is.na(label) || !nzchar(label)) {
    stop("`label` must be a single non-empty character string.", call. = FALSE)
  }
  if (!is.character(format) || length(format) != 1L || is.na(format) || !nzchar(trimws(format))) {
    stop("`format` must be a non-empty character string.", call. = FALSE)
  }
  if (grepl("[\t\r\n]", format)) {
    stop("`format` must use spaces to separate words.", call. = FALSE)
  }
  if (!is.character(wrong_hint) || length(wrong_hint) != 1L || is.na(wrong_hint)) {
    stop("`wrong_hint` must be a single character string.", call. = FALSE)
  }
  if (!is.character(right_hint) || length(right_hint) != 1L || is.na(right_hint)) {
    stop("`right_hint` must be a single character string.", call. = FALSE)
  }
  if (is.null(feedback)) {
    feedback <- .devoirs3_state$feedback_default
  }
  if (!is.logical(feedback) || length(feedback) != 1L || is.na(feedback)) {
    stop("`feedback` must be TRUE or FALSE.", call. = FALSE)
  }
  if (label %in% .devoirs3_state$labels) {
    stop(sprintf("Label '%s' is duplicated. All labels must be unique within the document.", label), call. = FALSE)
  }

  format <- trimws(format)
  punctuation <- NULL
  final_character <- substr(format, nchar(format), nchar(format))
  if (final_character %in% c("?", "!", ".", ",")) {
    punctuation <- final_character
    format <- trimws(substr(format, 1L, nchar(format) - 1L))
  }
  if (!nzchar(format) || grepl("[?!.,]$", format)) {
    stop("`format` may have at most one ending punctuation character.", call. = FALSE)
  }
  words <- strsplit(format, " +")[[1L]]
  if (length(words) < 2L) {
    stop("`format` must contain at least two words for the choice labels.", call. = FALSE)
  }

  first_text <- words[[1L]]
  second_text <- words[[length(words)]]
  middle_text <- if (length(words) > 2L) {
    paste(words[seq.int(2L, length(words) - 1L)], collapse = " ")
  } else {
    ""
  }
  separator <- if (nzchar(middle_text)) paste0(" ", middle_text, " ") else " "
  question <- paste(paste(words, collapse = " "), "question")
  answers <- list(
    list(text = first_text, correct = correct, feedback = if (correct) right_hint else wrong_hint),
    list(text = second_text, correct = !correct, feedback = if (correct) wrong_hint else right_hint)
  )
  option_ids <- .devoirs3_option_ids(label, answers)
  .devoirs3_state$labels <- c(.devoirs3_state$labels, label)

  group_id <- paste0("devoirs3-selection-", paste(sprintf("%02x", as.integer(charToRaw(enc2utf8(label)))), collapse = ""))
  option_controls <- lapply(seq_along(answers), function(index) {
    answer <- answers[[index]]
    htmltools::tags$label(
      class = "devoirs3-choice",
      htmltools::tags$input(
        type = "radio",
        name = group_id,
        id = paste0(group_id, "-", index),
        value = option_ids[[index]],
        `data-devoirs3-option` = "",
        `data-option-id` = option_ids[[index]],
        `data-feedback` = answer$feedback
      ),
      htmltools::tags$span(answer$text)
    )
  })
  widget <- htmltools::tags$span(
    class = "devoirs3-selection-widget devoirs3-selection-inline",
    role = "group",
    `aria-label` = question,
    `data-devoirs3-widget` = "selection",
    `data-docid` = .devoirs3_state$doc_id,
    `data-label` = label,
    `data-feedback-enabled` = tolower(as.character(feedback)),
    `data-multiple` = "false",
    htmltools::tags$span(class = "devoirs3-visually-hidden", question),
    option_controls[[1L]],
    separator,
    option_controls[[2L]],
    punctuation,
    htmltools::tags$span(
      class = "devoirs3-feedback",
      role = "status",
      `aria-live` = "polite",
      `aria-atomic` = "true",
      `data-devoirs3-feedback` = "",
      hidden = TRUE
    ),
    htmltools::tags$button(
      type = "button",
      class = "devoirs3-clear-button",
      `data-devoirs3-clear-selection` = "",
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
