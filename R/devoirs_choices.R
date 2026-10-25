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