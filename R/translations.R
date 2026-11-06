#' Translations from original {devoirs} to this current {devoirs3}
#'
#' Utilities to translate .qmd files from the old package to the new. Not likely to be used
#' by anyone except the author of {devoirs3}. These two functions supervise the many helpers.
#' - `translate_one_file()`
#' - `translate_chapter()`
#'
#' They are NOT exported, since they are not needed in ordinary use.
#'
#' @rdname translate
#' @export
translate <- function() {
  "See internal functions."
}
.mcq_line_content <- function(lines) {
  sub("(?:\r\n|\n|\r)$", "", lines, perl = TRUE)
}

.mcq_parse_choice <- function(text) {
  text <- trimws(text)
  correct <- FALSE
  feedback <- NULL

  marker <- "[correct hint:"
  marker_start <- regexpr(marker, text, fixed = TRUE)[[1L]]
  if (marker_start > 0L && endsWith(text, "]")) {
    correct <- TRUE
    feedback <- substr(text, marker_start + nchar(marker), nchar(text) - 1L)
    text <- substr(text, 1L, marker_start - 1L)
  } else if (endsWith(text, "[correct]")) {
    correct <- TRUE
    text <- substr(text, 1L, nchar(text) - nchar("[correct]"))
  } else {
    marker <- "[hint:"
    marker_start <- regexpr(marker, text, fixed = TRUE)[[1L]]
    if (marker_start > 0L && endsWith(text, "]")) {
      feedback <- substr(text, marker_start + nchar(marker), nchar(text) - 1L)
      text <- substr(text, 1L, marker_start - 1L)
    }
  }

  text <- trimws(text)
  if (!nzchar(text)) {
    stop("choice text is empty", call. = FALSE)
  }
  if (grepl("[correct", text, fixed = TRUE) || grepl("[hint:", text, fixed = TRUE)) {
    stop("correct or hint markup must appear at the end of a choice", call. = FALSE)
  }

  list(
    text = text,
    correct = correct,
    feedback = if (is.null(feedback)) NULL else trimws(feedback)
  )
}

.mcq_parse_chunk <- function(body_lines) {
  body <- .mcq_line_content(body_lines)
  directives <- body[startsWith(trimws(body), "#|")]
  option_names <- character()
  label <- NULL
  inline <- FALSE

  for (directive in directives) {
    directive <- trimws(directive)
    directive_body <- trimws(substring(directive, 3L))
    colon <- regexpr(":", directive_body, fixed = TRUE)[[1L]]
    if (colon < 1L) {
      stop("malformed chunk option", call. = FALSE)
    }

    option_name <- trimws(substr(directive_body, 1L, colon - 1L))
    option_value <- trimws(substr(directive_body, colon + 1L, nchar(directive_body)))
    option_names <- c(option_names, option_name)

    if (identical(option_name, "label")) {
      label <- option_value
    } else if (identical(option_name, "inline")) {
      if (!tolower(option_value) %in% c("true", "false")) {
        stop("inline must be true or false", call. = FALSE)
      }
      inline <- identical(tolower(option_value), "true")
    } else {
      stop(sprintf("unsupported chunk option '%s'", option_name), call. = FALSE)
    }
  }

  if (sum(option_names == "label") != 1L || is.null(label) || !nzchar(label)) {
    stop("each mcq chunk must have exactly one non-empty label", call. = FALSE)
  }
  if (sum(option_names == "inline") > 1L) {
    stop("inline may be specified only once", call. = FALSE)
  }

  choices <- list()
  current_choice <- NULL
  for (line in body) {
    trimmed <- trimws(line)
    if (startsWith(trimmed, "#|")) {
      next
    }
    if (!nzchar(trimmed)) {
      next
    }

    prefix_match <- regexec(
      "^[[:space:]]*([[:digit:]]+|[[:alpha:]])[.][[:space:]]*(.*)$",
      line,
      perl = TRUE
    )
    prefix_parts <- regmatches(line, prefix_match)[[1L]]
    if (length(prefix_parts)) {
      if (!is.null(current_choice)) {
        choices[[length(choices) + 1L]] <- .mcq_parse_choice(current_choice)
      }
      current_choice <- prefix_parts[[3L]]
    } else if (is.null(current_choice)) {
      stop("text appears before the first numbered or lettered choice", call. = FALSE)
    } else {
      current_choice <- paste(current_choice, trimmed)
    }
  }

  if (!is.null(current_choice)) {
    choices[[length(choices) + 1L]] <- .mcq_parse_choice(current_choice)
  }
  if (!length(choices)) {
    stop("mcq chunk has no choices", call. = FALSE)
  }

  list(label = label, inline = inline, choices = choices)
}

.mcq_yaml_quote <- function(text) {
  paste0("'", gsub("'", "''", text, fixed = TRUE), "'")
}

.mcq_log_unsupported <- function(chunk_number, start_line, reason, chunk_lines) {
  project_root <- getwd()
  log_path <- file.path(project_root, "devoirs3", "case-log.qmd")
  dir.create(dirname(log_path), recursive = TRUE, showWarnings = FALSE)
  if (!file.exists(log_path)) {
    writeBin(charToRaw("# Unsupported mcq conversion cases\n"), log_path)
  }

  entry <- c(
    "",
    sprintf("## Lines input, chunk %d (line %d)", chunk_number, start_line),
    "",
    "- Source: `Lines` argument",
    paste0("- Reason: ", reason),
    "",
    "````qmd",
    paste(chunk_lines, collapse = "\n"),
    "````",
    ""
  )
  cat(paste(entry, collapse = "\n"), file = log_path, append = TRUE)
  log_path
}

mcq_to_devoirs_choices <- function(Lines) {
  if (!is.character(Lines) || anyNA(Lines)) {
    stop("Lines must be a character vector without missing values", call. = FALSE)
  }
  if (any(grepl("\n", Lines, fixed = TRUE) | grepl("\r", Lines, fixed = TRUE))) {
    stop("each element of Lines must contain one line without line breaks", call. = FALSE)
  }
  if (!length(Lines)) {
    return(Lines)
  }

  line_content <- .mcq_line_content(Lines)
  openings <- which(trimws(line_content) == "```{mcq}")
  if (!length(openings)) {
    return(Lines)
  }

  output_parts <- list()
  output_part_count <- 0L
  seen_question_labels <- character()
  seen_choice_variables <- character()
  chunk_number <- 0L
  line_number <- 1L

  while (line_number <= length(Lines)) {
    if (!identical(trimws(line_content[[line_number]]), "```{mcq}")) {
      output_part_count <- output_part_count + 1L
      output_parts[[output_part_count]] <- Lines[[line_number]]
      line_number <- line_number + 1L
      next
    }

    chunk_number <- chunk_number + 1L
    closing_offsets <- if (line_number < length(Lines)) {
      which(trimws(line_content[(line_number + 1L):length(Lines)]) == "```")
    } else {
      integer()
    }
    if (!length(closing_offsets)) {
      chunk_lines <- Lines[line_number:length(Lines)]
      log_path <- .mcq_log_unsupported(
        chunk_number,
        line_number,
        "mcq chunk has no closing code fence",
        chunk_lines
      )
      stop(sprintf("unsupported mcq chunk logged to %s", log_path), call. = FALSE)
    }

    closing_offset <- closing_offsets[[1L]]
    closing_line <- line_number + closing_offset
    chunk_lines <- Lines[line_number:closing_line]
    body_lines <- if (closing_line > line_number + 1L) {
      Lines[(line_number + 1L):(closing_line - 1L)]
    } else {
      character()
    }
    parsed <- tryCatch(
      .mcq_parse_chunk(body_lines),
      error = function(error) {
        log_path <- .mcq_log_unsupported(
          chunk_number,
          line_number,
          conditionMessage(error),
          chunk_lines
        )
        stop(sprintf("unsupported mcq chunk logged to %s", log_path), call. = FALSE)
      }
    )

    if (parsed$label %in% seen_question_labels) {
      log_path <- .mcq_log_unsupported(
        chunk_number,
        line_number,
        sprintf("question label '%s' is duplicated in this document", parsed$label),
        chunk_lines
      )
      stop(sprintf("unsupported mcq chunk logged to %s", log_path), call. = FALSE)
    }
    seen_question_labels <- c(seen_question_labels, parsed$label)

    choice_variable <- make.names(paste0("choices_", parsed$label))
    if (choice_variable %in% seen_choice_variables) {
      choice_variable <- make.unique(c(seen_choice_variables, choice_variable), sep = "_")[[length(seen_choice_variables) + 1L]]
    }
    seen_choice_variables <- c(seen_choice_variables, choice_variable)

    choice_lines <- c("```{devoirs_choices}", paste0("#| label: ", choice_variable))
    for (choice in parsed$choices) {
      choice_lines <- c(choice_lines, paste0("- ", .mcq_yaml_quote(choice$text), ":"))
      choice_lines <- c(choice_lines, paste0("    correct: ", tolower(as.character(choice$correct))))
      if (!is.null(choice$feedback)) {
        choice_lines <- c(choice_lines, paste0("    feedback: ", .mcq_yaml_quote(choice$feedback)))
      }
    }
    choice_lines <- c(choice_lines, "```")

    correct_count <- sum(vapply(parsed$choices, `[[`, logical(1), "correct"))
    selection_lines <- c(
      "```{r echo=FALSE}",
      "Q <- devoirs_selection(",
      paste0("  label = ", encodeString(parsed$label, quote = "\""), ","),
      "  question = \"Placeholder for now\",",
      paste0("  answers = ", choice_variable, ","),
      paste0("  multiple = ", if (correct_count > 1L) "TRUE" else "FALSE"),
      ")",
      paste0("Q(inline = ", if (parsed$inline) "TRUE" else "FALSE", ")"),
      "```"
    )
    replacement <- c(choice_lines, "", selection_lines)
    output_part_count <- output_part_count + 1L
    output_parts[[output_part_count]] <- replacement
    line_number <- closing_line + 1L
  }

  unlist(output_parts, use.names = FALSE)
}

.set_split_arguments <- function(text) {
  if (!nzchar(trimws(text))) {
    return(character())
  }

  text_length <- nchar(text, type = "chars")
  starts <- 1L
  separators <- integer()
  stack <- character()
  quote <- ""
  escaped <- FALSE
  in_comment <- FALSE

  for (position in seq_len(text_length)) {
    character <- substr(text, position, position)
    if (in_comment) {
      if (character == "\n") {
        in_comment <- FALSE
      }
      next
    }
    if (nzchar(quote)) {
      if (escaped) {
        escaped <- FALSE
      } else if (character == "\\") {
        escaped <- TRUE
      } else if (character == quote) {
        quote <- ""
      }
      next
    }
    if (character == "#") {
      in_comment <- TRUE
    } else if (character %in% c("\"", "'", "`")) {
      quote <- character
    } else if (character %in% c("(", "[", "{")) {
      stack <- c(stack, character)
    } else if (character %in% c(")", "]", "}")) {
      if (length(stack)) {
        stack <- stack[-length(stack)]
      }
    } else if (character == "," && !length(stack)) {
      separators <- c(separators, position)
    }
  }

  ends <- c(separators - 1L, text_length)
  starts <- c(starts, separators + 1L)
  Map(function(start, end) substr(text, start, end), starts, ends) |>
    unlist(use.names = FALSE)
}

.set_find_call_end <- function(source, open_position) {
  source_length <- nchar(source, type = "chars")
  stack <- "("
  quote <- ""
  escaped <- FALSE
  in_comment <- FALSE

  for (position in seq.int(open_position + 1L, source_length)) {
    character <- substr(source, position, position)
    if (in_comment) {
      if (character == "\n") {
        in_comment <- FALSE
      }
      next
    }
    if (nzchar(quote)) {
      if (escaped) {
        escaped <- FALSE
      } else if (character == "\\") {
        escaped <- TRUE
      } else if (character == quote) {
        quote <- ""
      }
      next
    }
    if (character == "#") {
      in_comment <- TRUE
    } else if (character %in% c("\"", "'", "`")) {
      quote <- character
    } else if (character %in% c("(", "[", "{")) {
      stack <- c(stack, character)
    } else if (character %in% c(")", "]", "}")) {
      if (!length(stack)) {
        return(NA_integer_)
      }
      stack <- stack[-length(stack)]
      if (!length(stack)) {
        return(position)
      }
    }
  }

  NA_integer_
}

.set_rewrite_call <- function(call) {
  new_name <- "devoirs3::devoirs_set3"
  open_position <- regexpr("\\(", call, perl = TRUE)[[1L]]
  close_position <- nchar(call, type = "chars")
  arguments_text <- substr(call, open_position + 1L, close_position - 1L)
  arguments <- .set_split_arguments(arguments_text)
  argument_names <- vapply(arguments, function(argument) {
    match <- regexec("^[[:space:]]*([[:alpha:].][[:alnum:]_.]*)[[:space:]]*=(?!=)", argument, perl = TRUE)
    parts <- regmatches(argument, match)[[1L]]
    if (length(parts)) parts[[2L]] else ""
  }, character(1))
  id_index <- which(argument_names == "id")

  if (length(id_index)) {
    arguments[[id_index[[1L]]]] <- sub(
      "^([[:space:]]*)id([[:space:]]*=(?!=))",
      "\\1label\\2",
      arguments[[id_index[[1L]]]],
      perl = TRUE
    )
  } else {
    positional_id <- length(arguments) > 1L &&
      !nzchar(argument_names[[1L]]) &&
      "choices" %in% argument_names &&
      grepl("^(['\"]).*\\1$", trimws(arguments[[1L]]), perl = TRUE)
    if (positional_id) {
      leading_space <- sub("^([[:space:]]*).*$", "\\1", arguments[[1L]], perl = TRUE)
      arguments[[1L]] <- paste0(leading_space, "label = ", trimws(arguments[[1L]]))
    } else {
      generated_label <- paste(sample(c(letters, 0:9), 5L, replace = FALSE), collapse = "")
      arguments <- c(paste0('label = "', generated_label, '"'), arguments)
    }
  }

  if (length(arguments)) {
    arguments <- vapply(arguments, .set_rewrite_r_source, character(1))
    rewritten_arguments <- paste(arguments, collapse = ",")
  } else {
    rewritten_arguments <- ""
  }
  paste0(new_name, "(", rewritten_arguments, ")")
}

.set_rewrite_r_source <- function(source) {
  target <- "devoirs::devoirs_set"
  source_length <- nchar(source, type = "chars")
  position <- 1L
  quote <- ""
  escaped <- FALSE
  in_comment <- FALSE

  while (position <= source_length) {
    character <- substr(source, position, position)
    if (in_comment) {
      if (character == "\n") {
        in_comment <- FALSE
      }
      position <- position + 1L
      next
    }
    if (nzchar(quote)) {
      if (escaped) {
        escaped <- FALSE
      } else if (character == "\\") {
        escaped <- TRUE
      } else if (character == quote) {
        quote <- ""
      }
      position <- position + 1L
      next
    }
    if (character == "#") {
      in_comment <- TRUE
      position <- position + 1L
      next
    }
    if (character %in% c("\"", "'", "`")) {
      quote <- character
      position <- position + 1L
      next
    }

    target_end <- position + nchar(target, type = "chars") - 1L
    preceding <- if (position > 1L) substr(source, position - 1L, position - 1L) else ""
    if (substr(source, position, target_end) == target &&
        !grepl("[[:alnum:]_.:]", preceding)) {
      open_position <- target_end + 1L
      while (open_position <= source_length &&
             grepl("[[:space:]]", substr(source, open_position, open_position))) {
        open_position <- open_position + 1L
      }
      if (substr(source, open_position, open_position) == "(") {
        close_position <- .set_find_call_end(source, open_position)
        if (!is.na(close_position)) {
          call <- substr(source, position, close_position)
          replacement <- .set_rewrite_call(call)
          source <- paste0(
            substr(source, 1L, position - 1L),
            replacement,
            substr(source, close_position + 1L, source_length)
          )
          source_length <- nchar(source, type = "chars")
          position <- position + nchar(replacement, type = "chars")
          next
        }
      }
    }
    position <- position + 1L
  }

  source
}

set_to_set3 <- function(Lines) {
  if (!is.character(Lines) || anyNA(Lines)) {
    stop("Lines must be a character vector without missing values", call. = FALSE)
  }
  if (any(grepl("\n", Lines, fixed = TRUE) | grepl("\r", Lines, fixed = TRUE))) {
    stop("each element of Lines must contain one line without line breaks", call. = FALSE)
  }
  if (!length(Lines)) {
    return(Lines)
  }

  line_content <- .mcq_line_content(Lines)
  line_ending <- function(line) {
    if (endsWith(line, "\r\n")) "\r\n" else if (endsWith(line, "\n")) "\n" else if (endsWith(line, "\r")) "\r" else ""
  }
  result <- Lines
  line_number <- 1L

  while (line_number <= length(result)) {
    opening_line <- line_content[[line_number]]
    is_r_fence <- grepl("^[[:space:]]*(`{3,}|~{3,})[[:space:]]*(\\{r([,[:space:]}]|$)|r([,[:space:]]|$))", opening_line, perl = TRUE)
    if (is_r_fence) {
      fence <- regmatches(opening_line, regexpr("`{3,}|~{3,}", opening_line, perl = TRUE))
      fence_character <- substr(fence, 1L, 1L)
      fence_pattern <- paste0("^[[:space:]]*", if (fence_character == "`") "`" else "~", "{", nchar(fence, type = "chars"), ",}[[:space:]]*$")
      closing_line <- line_number + 1L
      while (closing_line <= length(result) && !grepl(fence_pattern, line_content[[closing_line]], perl = TRUE)) {
        closing_line <- closing_line + 1L
      }
      body_end <- min(closing_line - 1L, length(result))
      if (body_end >= line_number + 1L) {
        body_indices <- seq.int(line_number + 1L, body_end)
        source <- paste(line_content[body_indices], collapse = "\n")
        rewritten <- .set_rewrite_r_source(source)
        rewritten_lines <- strsplit(rewritten, "\n", fixed = TRUE)[[1L]]
        if (endsWith(rewritten, "\n")) {
          rewritten_lines <- c(rewritten_lines, "")
        }
        if (length(rewritten_lines) != length(body_indices)) {
          stop("rewriting changed the number of R source lines", call. = FALSE)
        }
        result[body_indices] <- Map(
          paste0,
          rewritten_lines,
          vapply(result[body_indices], line_ending, character(1))
        ) |> unlist(use.names = FALSE)
        line_content[body_indices] <- rewritten_lines
      }
      line_number <- if (closing_line <= length(result)) closing_line + 1L else length(result) + 1L
      next
    }

    content <- line_content[[line_number]]
    marker_positions <- gregexpr("`r[[:space:]]+", content, perl = TRUE)[[1L]]
    if (marker_positions[[1L]] > 0L) {
      marker_lengths <- attr(marker_positions, "match.length")
      for (marker_index in rev(seq_along(marker_positions))) {
        marker_start <- marker_positions[[marker_index]]
        expression_start <- marker_start + marker_lengths[[marker_index]]
        closing_positions <- gregexpr("`", substring(content, expression_start), fixed = TRUE)[[1L]]
        if (closing_positions[[1L]] < 0L) {
          next
        }
        expression_end <- expression_start + closing_positions[[1L]] - 2L
        expression <- substr(content, expression_start, expression_end)
        rewritten <- .set_rewrite_r_source(expression)
        content <- paste0(
          substr(content, 1L, expression_start - 1L),
          rewritten,
          substr(content, expression_end + 1L, nchar(content, type = "chars"))
        )
      }
      result[[line_number]] <- paste0(content, line_ending(result[[line_number]]))
      line_content[[line_number]] <- content
    }
    line_number <- line_number + 1L
  }

  result
}

.Nov5_rewrite_call <- function(call, function_name, qualified) {
  function_end <- nchar(function_name, type = "chars")
  if (qualified) {
    function_end <- function_end + nchar("devoirs::", type = "chars")
  }
  open_position <- regexpr("\\(", call, perl = TRUE)[[1L]]
  close_position <- nchar(call, type = "chars")
  arguments_text <- substr(call, open_position + 1L, close_position - 1L)
  arguments <- .set_split_arguments(arguments_text)
  argument_names <- vapply(arguments, function(argument) {
    match <- regexec("^[[:space:]]*([[:alpha:].][[:alnum:]_.]*)[[:space:]]*=(?!=)", argument, perl = TRUE)
    parts <- regmatches(argument, match)[[1L]]
    if (length(parts)) parts[[2L]] else ""
  }, character(1))

  replacements <- if (identical(function_name, "devoirs_text")) {
    c(qid = "label", rows = "height", cols = "width", str = "placeholder")
  } else {
    c(id = "label", "wrong.hint" = "wrong_hint", "right.hint" = "right_hint")
  }
  for (argument_index in seq_along(arguments)) {
    replacement <- unname(replacements[argument_names[[argument_index]]])
    if (length(replacement) && !is.na(replacement)) {
      arguments[[argument_index]] <- sub(
        "^([[:space:]]*)[[:alpha:].][[:alnum:]_.]*([[:space:]]*=(?!=))",
        paste0("\\1", replacement, "\\2"),
        arguments[[argument_index]],
        perl = TRUE
      )
    }
  }

  paste0(
    "devoirs3::", function_name,
    substr(call, function_end + 1L, open_position - 1L),
    "(", paste(arguments, collapse = ","), ")"
  )
}

.Nov5_rewrite_source <- function(source) {
  function_names <- c("devoirs_text", "devoirs_TF")
  source_length <- nchar(source, type = "chars")
  position <- 1L

  while (position <= source_length) {
    preceding <- if (position > 1L) substr(source, position - 1L, position - 1L) else ""
    match_name <- NULL
    qualified <- FALSE

    for (function_name in function_names) {
      qualified_name <- paste0("devoirs::", function_name)
      qualified_end <- position + nchar(qualified_name, type = "chars") - 1L
      if (substr(source, position, qualified_end) == qualified_name &&
          !grepl("[[:alnum:]_.:]", preceding)) {
        match_name <- function_name
        qualified <- TRUE
        break
      }

      function_end <- position + nchar(function_name, type = "chars") - 1L
      if (substr(source, position, function_end) == function_name &&
          !grepl("[[:alnum:]_.:]", preceding)) {
        match_name <- function_name
        break
      }
    }

    if (is.null(match_name)) {
      position <- position + 1L
      next
    }

    name_length <- nchar(match_name, type = "chars") +
      if (qualified) nchar("devoirs::", type = "chars") else 0L
    open_position <- position + name_length
    while (open_position <= source_length &&
           grepl("[[:space:]]", substr(source, open_position, open_position))) {
      open_position <- open_position + 1L
    }
    if (substr(source, open_position, open_position) != "(") {
      position <- position + name_length
      next
    }

    close_position <- .set_find_call_end(source, open_position)
    if (is.na(close_position)) {
      position <- position + name_length
      next
    }
    call <- substr(source, position, close_position)
    arguments_text <- substr(call, open_position - position + 2L, nchar(call, type = "chars") - 1L)
    if (!nzchar(trimws(arguments_text))) {
      position <- close_position + 1L
      next
    }

    replacement <- .Nov5_rewrite_call(call, match_name, qualified)
    source <- paste0(
      substr(source, 1L, position - 1L), replacement,
      substr(source, close_position + 1L, source_length)
    )
    source_length <- nchar(source, type = "chars")
    position <- position + nchar(replacement, type = "chars")
  }

  source
}

Nov5 <- function(Lines) {
  if (!is.character(Lines) || anyNA(Lines)) {
    stop("Lines must be a character vector without missing values", call. = FALSE)
  }
  if (any(grepl("\n", Lines, fixed = TRUE) | grepl("\r", Lines, fixed = TRUE))) {
    stop("each element of Lines must contain one line without line breaks", call. = FALSE)
  }
  if (!length(Lines)) {
    return(Lines)
  }

  source <- paste(Lines, collapse = "\n")
  rewritten <- .Nov5_rewrite_source(source)
  output_lines <- strsplit(paste0(rewritten, "\nNov5-line-boundary"), "\n", fixed = TRUE)[[1L]]
  output_lines[-length(output_lines)]
}

translate_one_file <- function(path) {
  if (!is.character(path) || length(path) != 1L || is.na(path) || !nzchar(path)) {
    stop("path must be one non-empty relative file path", call. = FALSE)
  }
  if (grepl("^(/|~|[[:alpha:]]:)", path) || grepl("\\\\", path)) {
    stop("path must be relative to the project root and use forward slashes", call. = FALSE)
  }

  path_parts <- strsplit(path, "/", fixed = TRUE)[[1L]]
  if (any(path_parts %in% c("", ".", "..")) || identical(path_parts[[1L]], "devoirs3")) {
    stop("path must identify a source file outside devoirs3/", call. = FALSE)
  }
  if (!grepl("[.]qmd$", path, ignore.case = TRUE)) {
    stop("path must identify a .qmd file", call. = FALSE)
  }

  source_path <- do.call(file.path, as.list(path_parts))
  if (!file.exists(source_path) || dir.exists(source_path)) {
    stop(sprintf("source file does not exist: %s", path), call. = FALSE)
  }
  project_root <- normalizePath(getwd(), mustWork = TRUE)
  normalized_source <- normalizePath(source_path, mustWork = TRUE)
  if (!startsWith(normalized_source, paste0(project_root, .Platform$file.sep))) {
    stop("source file must be inside the project root", call. = FALSE)
  }

  output_path <- file.path("devoirs3", do.call(file.path, as.list(path_parts)))
  output_directory <- dirname(output_path)
  if (!dir.exists(output_directory)) {
    dir.create(output_directory, recursive = TRUE, showWarnings = FALSE)
  }
  if (!dir.exists(output_directory)) {
    stop(sprintf("could not create output directory: %s", output_directory), call. = FALSE)
  }
  if (dir.exists(output_path)) {
    stop(sprintf("output path is a directory: %s", output_path), call. = FALSE)
  }

  copy_source <- function() {
    copied <- file.copy(source_path, output_path, overwrite = TRUE, copy.mode = TRUE, copy.date = TRUE)
    if (!copied) {
      stop(sprintf("could not copy source file to %s", output_path), call. = FALSE)
    }
  }

  source_lines <- tryCatch(
    readLines(source_path, warn = FALSE),
    error = function(error) error
  )
  translation_error <- if (inherits(source_lines, "error")) source_lines else NULL
  translated_lines <- NULL
  if (is.null(translation_error)) {
    translated_lines <- tryCatch(
      Nov5(set_to_set3(mcq_to_devoirs_choices(source_lines))),
      error = function(error) {
        translation_error <<- error
        NULL
      }
    )
  }

  if (!is.null(translation_error)) {
    copy_source()
    warning(
      sprintf("translation failed for %s; copied source unchanged: %s", path, conditionMessage(translation_error)),
      call. = FALSE
    )
  } else if (identical(translated_lines, source_lines)) {
    copy_source()
  } else {
    writeLines(translated_lines, output_path, useBytes = TRUE)
  }

  output_path
}

translate_chapter <- function(chapter) {
  if (!is.character(chapter) || length(chapter) != 1L || is.na(chapter) || !nzchar(chapter)) {
    stop("chapter must be one non-empty directory name", call. = FALSE)
  }
  if (grepl("^(/|~|[[:alpha:]]:)", chapter) || grepl("/", chapter, fixed = TRUE) ||
      grepl("\\\\", chapter) || chapter %in% c(".", "..", "devoirs3")) {
    stop("chapter must name a source directory directly under the project root", call. = FALSE)
  }
  if (!dir.exists(chapter)) {
    stop(sprintf("source chapter directory does not exist: %s", chapter), call. = FALSE)
  }

  project_root <- normalizePath(getwd(), mustWork = TRUE)
  normalized_chapter <- normalizePath(chapter, mustWork = TRUE)
  if (!startsWith(normalized_chapter, paste0(project_root, .Platform$file.sep))) {
    stop("source chapter directory must be inside the project root", call. = FALSE)
  }

  output_directory <- file.path("devoirs3", chapter)
  if (!dir.exists(output_directory)) {
    dir.create(output_directory, recursive = TRUE, showWarnings = FALSE)
  }
  if (!dir.exists(output_directory)) {
    stop(sprintf("could not create output directory: %s", output_directory), call. = FALSE)
  }

  relative_files <- list.files(
    chapter,
    pattern = "[.]qmd$",
    recursive = TRUE,
    full.names = FALSE,
    all.files = TRUE
  )
  relative_files <- relative_files[!dir.exists(file.path(chapter, relative_files))]
  source_paths <- if (length(relative_files)) {
    file.path(chapter, relative_files)
  } else {
    character()
  }
  vapply(source_paths, translate_one_file, character(1), USE.NAMES = FALSE)
}
