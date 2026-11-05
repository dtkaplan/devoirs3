test_that("selection questions render accessible native controls", {
  startup <- as.character(devoirs3::devoirs_start("selection-unit"))
  render <- devoirs3::devoirs_selection(
    label = "q1",
    question = "Choose <one> & only one",
    answers = list(
      list(text = "A & B", correct = TRUE),
      list(text = "Neither", correct = FALSE, feedback = "Try again")
    )
  )
  widget <- as.character(render(inline = TRUE, feedback = FALSE))

  expect_match(widget, "<span class=\"devoirs3-selection-widget devoirs3-selection-inline\"", fixed = TRUE)
  expect_match(widget, "role=\"group\"", fixed = TRUE)
  expect_match(widget, "aria-label=\"Choose &lt;one&gt; &amp; only one\"", fixed = TRUE)
  expect_match(widget, "Choose &lt;one&gt; &amp; only one", fixed = TRUE)
  expect_match(widget, "type=\"radio\"", fixed = TRUE)
  expect_match(widget, "A &amp; B", fixed = TRUE)
  expect_match(widget, "data-feedback-enabled=\"false\"", fixed = TRUE)
  expect_match(widget, "<span class=\"devoirs3-feedback\"", fixed = TRUE)
  expect_match(widget, "data-devoirs3-clear-selection", fixed = TRUE)
  expect_match(widget, "data-feedback=\"Correct!\"", fixed = TRUE)
  expect_match(startup, "font-size: 6pt", fixed = TRUE)
  expect_match(startup, "background-color: #f1f7fc", fixed = TRUE)
})

test_that("selection choice labels render inline Markdown and math safely", {
  devoirs3::devoirs_start("selection-markdown")
  widget <- as.character(devoirs3::devoirs_selection(
    label = "marked-up",
    question = "Choose one",
    answers = list(
      list(text = "*emphasized* and **strong**", correct = TRUE),
      list(text = "$x^2$", correct = FALSE),
      list(text = "$\\pi$", correct = FALSE),
      list(text = "<script>alert(1)</script>", correct = FALSE)
    )
  )())

  expect_match(widget, "<em>emphasized</em> and <strong>strong</strong>", fixed = TRUE)
  expect_match(widget, "class=\"math inline\">&#92;(x^2&#92;)", fixed = TRUE)
  expect_match(widget, "class=\"math inline\">&#92;(&#92;pi&#92;)", fixed = TRUE)
  expect_match(widget, "&lt;script&gt;alert(1)&lt;/script&gt;", fixed = TRUE)
  expect_false(grepl("<script>", widget, fixed = TRUE))
})

test_that("selection feedback renders inline Markdown and math safely", {
  devoirs3::devoirs_start("selection-feedback-markdown")
  widget <- as.character(devoirs3::devoirs_selection(
    label = "feedback-markup",
    question = "Choose one",
    answers = list(
      list(text = "Yes", correct = TRUE, feedback = "**Correct!** and $x^2$"),
      list(text = "No", correct = FALSE, feedback = "<script>alert(1)</script>")
    )
  )())

  expect_match(widget, "data-feedback-html=\"&lt;strong&gt;Correct!&lt;/strong&gt;", fixed = TRUE)
  expect_match(widget, "&lt;span class=&quot;math inline&quot;&gt;&amp;#92;(x^2&amp;#92;)", fixed = TRUE)
  expect_false(grepl("<script>", widget, fixed = TRUE))
})

test_that("multiple selection questions render checkboxes and inherit feedback", {
  devoirs3::devoirs_start("multiple-unit", feedback = FALSE)
  render <- devoirs3::devoirs_selection(
    label = "q2",
    question = "Choose all",
    multiple = TRUE,
    answers = list(list(text = "One", correct = TRUE))
  )
  widget <- as.character(render())

  expect_match(widget, "type=\"checkbox\"", fixed = TRUE)
  expect_match(widget, "data-multiple=\"true\"", fixed = TRUE)
  expect_match(widget, "data-feedback-enabled=\"false\"", fixed = TRUE)
  expect_match(widget, "<div class=\"devoirs3-feedback\"", fixed = TRUE)
})

test_that("feedback mode is inherited and can be overridden for set3 questions", {
  devoirs3::devoirs_start("feedback-mode", feedback = FALSE)
  expect_false(devoirs3::feedback_mode())

  enabled <- as.character(devoirs3::devoirs_set3(
    choices = 1:2, correct = 1, n = 2, label = "set3-enabled", feedback = TRUE
  ))
  inherited_off <- as.character(devoirs3::devoirs_set3(
    choices = 1:2, correct = 1, n = 2, label = "set3-inherited-off"
  ))
  expect_match(enabled, "data-feedback-enabled=\"true\"", fixed = TRUE)
  expect_match(inherited_off, "data-feedback-enabled=\"false\"", fixed = TRUE)

  expect_invisible(devoirs3::feedback_mode(TRUE))
  expect_true(devoirs3::feedback_mode())
  inherited_on <- as.character(devoirs3::devoirs_set3(
    choices = 1:2, correct = 1, n = 2, label = "set3-inherited-on"
  ))
  expect_match(inherited_on, "data-feedback-enabled=\"true\"", fixed = TRUE)

  devoirs3::devoirs_start("feedback-mode-reset", feedback = FALSE)
  expect_false(devoirs3::feedback_mode())
  expect_error(devoirs3::feedback_mode(NA), "`arg` must be TRUE or FALSE")
  expect_error(devoirs3::feedback_mode(1), "`arg` must be TRUE or FALSE")
})

test_that("block selection prompts remain accessible without duplicating visible prose", {
  devoirs3::devoirs_start("block-selection")
  render <- devoirs3::devoirs_selection(
    label = "q-block",
    question = "Prompt already in the document",
    answers = list(list(text = "A", correct = TRUE))
  )
  widget <- as.character(render(inline = FALSE))

  expect_match(widget, "<legend class=\"devoirs3-visually-hidden\">Prompt already in the document</legend>", fixed = TRUE)
})

test_that("the collector setting is validated and included in the script", {
  expect_error(devoirs3::devoirs_start("invalid-collector", collector = NA), "`collector`")
  expect_error(devoirs3::devoirs_start("invalid-labels", labels = NA), "`labels`")

  startup <- as.character(devoirs3::devoirs_start("collector-disabled", collector = FALSE, labels = FALSE))
  render <- devoirs3::devoirs_selection(
    label = "question",
    question = "Choose one",
    answers = list(list(text = "A", correct = TRUE))
  )
  widget <- as.character(render())

  expect_match(startup, "data-devoirs3-collector=\"false\"", fixed = TRUE)
  expect_match(startup, "data-devoirs3-labels=\"false\"", fixed = TRUE)

  startup <- as.character(devoirs3::devoirs_start("labels-only", collector = FALSE, labels = TRUE))
  labels_only <- as.character(devoirs3::devoirs_selection(
    label = "question",
    question = "Choose one",
    answers = list(list(text = "A", correct = TRUE))
  )())
  expect_match(startup, "data-devoirs3-collector=\"false\"", fixed = TRUE)
  expect_match(startup, "data-devoirs3-labels=\"true\"", fixed = TRUE)
})

test_that("option IDs encode position and correctness without changing RNG state", {
  answers <- lapply(seq_len(28), function(index) {
    list(text = paste("Choice", index), correct = index %% 2L == 0L)
  })
  set.seed(816)
  original_seed <- .Random.seed

  devoirs3::devoirs_start("option-id-unit")
  render <- devoirs3::devoirs_selection("stable-label", "Prompt", answers)
  first_html <- as.character(render())
  expect_identical(.Random.seed, original_seed)

  ids <- regmatches(first_html, gregexpr('value="[A-Z]{2}[a-z]{5}"', first_html))[[1L]]
  ids <- sub('value="([A-Z]{2}[a-z]{5})"', "\\1", ids)
  expect_length(ids, 28L)
  expect_identical(substr(ids[1:4], 1L, 2L), c("AA", "BA", "CA", "DA"))
  expect_identical(substr(ids[27:28], 1L, 2L), c("AB", "BB"))

  vowel_counts <- vapply(ids, function(option_id) {
    sum(strsplit(substr(option_id, 3L, 7L), "")[[1L]] %in% c("a", "e", "i", "o", "u"))
  }, integer(1))
  expect_true(all(vowel_counts[seq(2L, 28L, by = 2L)] %in% c(1L, 3L)))
  expect_true(all(vowel_counts[seq(1L, 27L, by = 2L)] %in% c(0L, 2L)))

  devoirs3::devoirs_start("option-id-unit")
  repeated <- as.character(devoirs3::devoirs_selection("stable-label", "Prompt", answers)())
  repeated_ids <- regmatches(repeated, gregexpr('value="[A-Z]{2}[a-z]{5}"', repeated))[[1L]]
  repeated_ids <- sub('value="([A-Z]{2}[a-z]{5})"', "\\1", repeated_ids)
  expect_identical(repeated_ids, ids)
})

test_that("selection labels share the document-wide duplicate registry", {
  devoirs3::devoirs_start("duplicate-selection")
  devoirs3::devoirs_text("shared", "Text prompt")
  render <- devoirs3::devoirs_selection(
    "shared", "Selection prompt", list(list(text = "A", correct = TRUE))
  )

  expect_error(render(), "Label 'shared' is duplicated")
})

test_that("selection arguments are validated", {
  devoirs3::devoirs_start("selection-validation")

  expect_error(devoirs3::devoirs_selection("q", "Prompt", list()), "between 1 and 676")
  expect_error(devoirs3::devoirs_selection("q", "Prompt", rep(list(list(text = "A", correct = TRUE)), 677)), "between 1 and 676")
  expect_error(devoirs3::devoirs_selection("q", "Prompt", list(list(text = "A", correct = 1))), "logical `correct`")
  expect_error(devoirs3::devoirs_selection("q", "Prompt", list(list(text = "A", correct = TRUE)), multiple = NA), "`multiple`")
})

test_that("true-or-false questions render inline selection behavior", {
  devoirs3::devoirs_start("tf-render", feedback = FALSE)
  widget <- as.character(devoirs3::devoirs_TF(
    correct = FALSE,
    label = "tf-question",
    format = "right or wrong?",
    wrong_hint = "Try <again>",
    right_hint = "Good & right",
    feedback = TRUE
  ))

  expect_match(widget, "<span class=\"devoirs3-selection-widget devoirs3-selection-inline\"", fixed = TRUE)
  expect_match(widget, "aria-label=\"right or wrong question\"", fixed = TRUE)
  expect_match(widget, "type=\"radio\"", fixed = TRUE)
  expect_length(regmatches(widget, gregexpr("type=\"radio\"", widget, fixed = TRUE))[[1L]], 2L)
  expect_match(widget, "</label>\\s+or\\s+<label")
  expect_match(widget, "\\?\\s+<span class=\"devoirs3-feedback\"")
  expect_match(widget, "data-feedback-enabled=\"true\"", fixed = TRUE)
  expect_match(widget, "data-feedback=\"Try &lt;again&gt;\"", fixed = TRUE)
  expect_match(widget, "data-feedback=\"Good &amp; right\"", fixed = TRUE)
  expect_match(widget, "data-devoirs3-clear-selection", fixed = TRUE)
  expect_match(widget, "data-devoirs3-widget=\"selection\"", fixed = TRUE)
})

test_that("true-or-false questions share label uniqueness and validate arguments", {
  devoirs3::devoirs_start("tf-validation")
  devoirs3::devoirs_text("shared-label", "Text prompt")

  expect_error(devoirs3::devoirs_TF(TRUE, "shared-label"), "duplicated")
  expect_error(devoirs3::devoirs_TF(1, "q"), "`correct`")
  expect_error(devoirs3::devoirs_TF(TRUE, ""), "`label`")
  expect_error(devoirs3::devoirs_TF(TRUE, "q", format = "True"), "at least two words")
  expect_error(devoirs3::devoirs_TF(TRUE, "q", format = "True\tor False"), "use spaces")
  expect_error(devoirs3::devoirs_TF(TRUE, "q", wrong_hint = NA_character_), "`wrong_hint`")
  expect_error(devoirs3::devoirs_TF(TRUE, "q", feedback = 1), "`feedback`")
})