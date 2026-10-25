.devoirs3_with_seed <- function(seed, code) {
  old_kind <- RNGkind()
  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (had_seed) {
    old_seed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  }
  on.exit({
    do.call(RNGkind, as.list(old_kind))
    if (had_seed) {
      assign(".Random.seed", old_seed, envir = .GlobalEnv)
    } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
  }, add = TRUE)

  RNGkind("Mersenne-Twister", "Inversion", "Rejection")
  set.seed(as.integer(seed))
  force(code)
}

.devoirs3_option_ids <- function(label, answers) {
  count <- length(answers)
  indexes <- seq_len(count) - 1L
  prefixes <- paste0(
    LETTERS[(indexes %% 26L) + 1L],
    LETTERS[(indexes %/% 26L) + 1L]
  )

  suffixes <- .devoirs3_with_seed(rolling_hash(label), {
    vowels <- c("a", "e", "i", "o", "u")
    consonants <- setdiff(letters, vowels)
    vapply(answers, function(answer) {
      vowel_count <- sample(if (answer$correct) c(1L, 3L) else c(0L, 2L), 1L)
      suffix <- sample(letters, 5L, replace = TRUE)
      vowel_positions <- if (vowel_count > 0L) {
        sample(seq_len(5L), vowel_count)
      } else {
        integer()
      }
      consonant_positions <- setdiff(seq_len(5L), vowel_positions)
      if (length(vowel_positions)) {
        suffix[vowel_positions] <- sample(vowels, length(vowel_positions), replace = TRUE)
      }
      if (length(consonant_positions)) {
        suffix[consonant_positions] <- sample(consonants, length(consonant_positions), replace = TRUE)
      }
      paste0(suffix, collapse = "")
    }, character(1))
  })

  paste0(prefixes, suffixes)
}

#' Create a single- or multiple-choice question
#'
#' Returns a render function that emits a labelled radio-button group or
#' checkbox group and its feedback region.
#'
#' @param label A non-empty ID unique among question elements in the document.
#' @param question The human-readable prompt and accessible group name.
#' @param answers A non-empty list of records with `text`, `correct`, and
#'   optional `feedback` fields.
#' @param multiple If `TRUE`, allow one or more choices; otherwise allow one.
#'
#' @return A function that renders the question in Quarto output.
#' @export
devoirs_selection <- function(label, question, answers, multiple = FALSE) {
  if (is.null(.devoirs3_state$doc_id)) {
    stop("Call `devoirs_start()` before creating question elements.", call. = FALSE)
  }
  if (!is.character(label) || length(label) != 1L || is.na(label) || !nzchar(label)) {
    stop("`label` must be a single non-empty character string.", call. = FALSE)
  }
  if (!is.character(question) || length(question) != 1L || is.na(question) || !nzchar(question)) {
    stop("`question` must be a single non-empty character string.", call. = FALSE)
  }
  if (!is.list(answers) || length(answers) < 1L || length(answers) > 676L) {
    stop("`answers` must be a list containing between 1 and 676 choices.", call. = FALSE)
  }
  if (!is.logical(multiple) || length(multiple) != 1L || is.na(multiple)) {
    stop("`multiple` must be TRUE or FALSE.", call. = FALSE)
  }

  answers <- lapply(seq_along(answers), function(index) {
    answer <- answers[[index]]
    if (!is.list(answer) || is.null(names(answer)) ||
        !is.character(answer$text) || length(answer$text) != 1L || is.na(answer$text) || !nzchar(answer$text)) {
      stop(sprintf("Choice %d must have a non-empty character `text` field.", index), call. = FALSE)
    }
    if (!is.logical(answer$correct) || length(answer$correct) != 1L || is.na(answer$correct)) {
      stop(sprintf("Choice %d must have a logical `correct` field.", index), call. = FALSE)
    }
    if (is.null(answer$feedback)) {
      answer$feedback <- if (answer$correct) "Correct!" else "Sorry."
    }
    if (!is.character(answer$feedback) || length(answer$feedback) != 1L || is.na(answer$feedback)) {
      stop(sprintf("Choice %d `feedback` must be a single character string.", index), call. = FALSE)
    }
    answer
  })

  option_ids <- .devoirs3_option_ids(label, answers)
  for (index in seq_along(answers)) {
    answers[[index]]$optionId <- option_ids[[index]]
  }

  function(inline = FALSE, feedback = NULL) {
    if (label %in% .devoirs3_state$labels) {
      stop(sprintf("Label '%s' is duplicated. All labels must be unique within the document.", label), call. = FALSE)
    }
    if (!is.logical(inline) || length(inline) != 1L || is.na(inline)) {
      stop("`inline` must be TRUE or FALSE.", call. = FALSE)
    }
    if (is.null(feedback)) {
      feedback <- .devoirs3_state$feedback_default
    }
    if (!is.logical(feedback) || length(feedback) != 1L || is.na(feedback)) {
      stop("`feedback` must be TRUE or FALSE.", call. = FALSE)
    }

    .devoirs3_state$labels <- c(.devoirs3_state$labels, label)
    group_id <- paste0("devoirs3-selection-", paste(sprintf("%02x", as.integer(charToRaw(enc2utf8(label)))), collapse = ""))
    input_type <- if (multiple) "checkbox" else "radio"
    option_controls <- lapply(seq_along(answers), function(index) {
      answer <- answers[[index]]
      option_id <- answer$optionId
      htmltools::tags$label(
        class = "devoirs3-choice",
        htmltools::tags$input(
          type = input_type,
          name = group_id,
          id = paste0(group_id, "-", index),
          value = option_id,
          `data-devoirs3-option` = "",
          `data-option-id` = option_id,
          `data-feedback` = answer$feedback
        ),
        htmltools::tags$span(answer$text)
      )
    })
    fieldset_class <- if (inline) {
      "devoirs3-selection-widget devoirs3-selection-inline"
    } else {
      "devoirs3-selection-widget"
    }
    feedback_tag <- if (inline) htmltools::tags$span else htmltools::tags$div
    wrapper <- if (inline) htmltools::tags$span else htmltools::tags$fieldset
    group_label <- if (inline) {
      htmltools::tags$span(class = "devoirs3-visually-hidden", question)
    } else {
      htmltools::tags$legend(class = "devoirs3-visually-hidden", question)
    }
    widget <- wrapper(
      class = fieldset_class,
      role = if (inline) "group" else NULL,
      `aria-label` = if (inline) question else NULL,
      `data-devoirs3-widget` = "selection",
      `data-docid` = .devoirs3_state$doc_id,
      `data-label` = label,
      `data-feedback-enabled` = tolower(as.character(feedback)),
      `data-multiple` = tolower(as.character(multiple)),
      group_label,
      option_controls,
      feedback_tag(
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
}