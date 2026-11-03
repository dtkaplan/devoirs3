#' Compact inline generation of fixed selection question elements
#'
#' @param choices Vector containing the member of the set to use as distractors
#' @param correct Number or character string with the <correct> answer.
#' @param n integer, number of distractors
#' @param label character string. File-unique id for this problem.
#' @param sort logical value. Force sorting on character strings. (Numerical choices are sorted by default.)
#' @param inline logical value. Place the items in an inline format.
#' @export
devoirs_set <-

devoirs_set3 <- function(
    choices = 1:5,
    correct = 3,
    n = 5,
    label = NULL,
    sort = is.numeric(correct),
    inline = TRUE
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
  render_selection(inline = inline)
}
