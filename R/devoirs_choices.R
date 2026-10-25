#' Define choices in a Quarto YAML chunk
#'
#' `devoirs_choices` is a knitr chunk engine for declaring a question's answer
#' choices as YAML in a Quarto document. The chunk does not print output; its
#' label must be a valid R variable name and is used to store the parsed choices
#' in the knitting environment. Pass that variable to
#' [devoirs_selection()]'s `answers` argument.
#'
#' @name devoirs_choices
#' @details
#' The YAML root must be a non-empty sequence. Each sequence item is either a
#' non-empty string (an incorrect choice with default feedback) or a mapping
#' whose key is the choice text and whose optional fields are `correct` and
#' `feedback`. Quote choice text used as a mapping key so YAML treats it as
#' text, not as a boolean, number, or other special value.
#'
#' `correct` must be a YAML boolean and defaults to `false`. `feedback` must be
#' a string; if omitted, it defaults to `Correct!` for a correct choice and
#' `Sorry.` otherwise. Unknown fields, empty choice text, and malformed YAML
#' produce an error that identifies the chunk or choice.
#'
#' ````qmd
#' ```{r include=FALSE}
#' library(devoirs3)
#' devoirs_start(docID = "geography")
#' ```
#'
#' ```{devoirs_choices}
#' #| label: capitalChoices
#' - "Paris":
#'     correct: true
#'     feedback: Correct!
#' - "Lyon":
#'     feedback: Not the capital.
#' - "Marseille"
#' ```
#'
#' ```{r}
#' capital <- devoirs_selection(
#'   label = "capital-question",
#'   question = "Which city is the capital of France?",
#'   answers = capitalChoices
#' )
#' ```
#'
#' `r capital()`
#' ````
#'
#' `capital` is a render function; place it in the document with an inline
#' expression such as `` `r capital()` ``. Choice order is preserved. A
#' `devoirs_selection()` question accepts at most 676 choices.
#'
#' @seealso [devoirs_selection()]
NULL

.devoirs3_choice_error <- function(label, message, index = NULL) {
  location <- if (is.null(index)) {
    sprintf("{devoirs_choices} chunk '%s'", label)
  } else {
    sprintf("choice %d in {devoirs_choices} chunk '%s'", index, label)
  }
  stop(sprintf("Invalid %s: %s", location, message), call. = FALSE)
}

.devoirs3_choices_engine <- function(options) {
  label <- options$label
  if (!is.character(label) || length(label) != 1L || is.na(label) ||
      !nzchar(label) || make.names(label) != label) {
    stop("A {devoirs_choices} chunk requires a valid R variable name as its label.", call. = FALSE)
  }

  parsed <- tryCatch(
    yaml::yaml.load(paste(options$code, collapse = "\n"), eval.expr = FALSE),
    error = function(error) {
      .devoirs3_choice_error(label, paste("invalid YAML:", conditionMessage(error)))
    }
  )
  if (is.character(parsed)) {
    parsed <- as.list(parsed)
  }
  if (!is.list(parsed) || !length(parsed)) {
    .devoirs3_choice_error(label, "the YAML root must be a non-empty sequence")
  }

  choices <- lapply(seq_along(parsed), function(index) {
    item <- parsed[[index]]
    if (is.character(item) && length(item) == 1L && !is.na(item) && nzchar(item)) {
      return(list(text = item, correct = FALSE, feedback = "Sorry."))
    }

    if (!is.list(item) || is.null(names(item)) || anyNA(names(item)) ||
        any(!nzchar(names(item)))) {
      .devoirs3_choice_error(label, "each item must be a non-empty string or a choice mapping", index)
    }

    metadata_fields <- intersect(names(item), c("correct", "feedback"))
    text_fields <- setdiff(names(item), c("correct", "feedback"))
    if (length(text_fields) == 1L && length(metadata_fields)) {
      text <- text_fields[[1L]]
      metadata <- item[metadata_fields]
    } else if (length(item) == 1L) {
      text <- names(item)[[1L]]
      metadata <- item[[1L]]
      if (is.null(metadata)) metadata <- list()
    } else {
      .devoirs3_choice_error(label, "a mapping must have one choice-text key and optional correct/feedback fields", index)
    }
    if (!is.character(text) || length(text) != 1L || !nzchar(text) || !is.list(metadata)) {
      .devoirs3_choice_error(label, "choice text must be a non-empty string and choice metadata must be a mapping", index)
    }

    fields <- names(metadata)
    if (is.null(fields) && length(metadata)) {
      .devoirs3_choice_error(label, "choice metadata must use named fields", index)
    }
    if (!is.null(fields) && (anyNA(fields) || any(!nzchar(fields)) || anyDuplicated(fields))) {
      .devoirs3_choice_error(label, "choice metadata fields must have unique, non-empty names", index)
    }
    unknown_fields <- setdiff(fields, c("correct", "feedback"))
    if (length(unknown_fields)) {
      .devoirs3_choice_error(label, sprintf("unknown metadata field '%s'", unknown_fields[[1L]]), index)
    }

    correct <- FALSE
    if ("correct" %in% fields) {
      correct <- metadata[["correct"]]
      if (!is.logical(correct) || length(correct) != 1L || is.na(correct)) {
        .devoirs3_choice_error(label, "`correct` must be a YAML boolean", index)
      }
    }

    feedback <- if (correct) "Correct!" else "Sorry."
    if ("feedback" %in% fields) {
      feedback <- metadata[["feedback"]]
      if (!is.character(feedback) || length(feedback) != 1L || is.na(feedback)) {
        .devoirs3_choice_error(label, "`feedback` must be a string", index)
      }
    }

    list(text = text, correct = correct, feedback = feedback)
  })

  assign(label, choices, envir = knitr::knit_global())
  ""
}

.onLoad <- function(libname, pkgname) {
  knitr::knit_engines$set(devoirs_choices = .devoirs3_choices_engine)
}