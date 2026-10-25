test_that("text question emits accessible, escaped markup", {
  devoirs3::devoirs_start("text-unit")
  widget <- as.character(devoirs3::devoirs_text(
    label = "short-answer",
    question = "Explain <why> & how",
    placeholder = "Type <here>",
    width = 32,
    height = 4,
    inline = TRUE
  ))

  expect_match(widget, "<span class=\"devoirs3-text-widget\"", fixed = TRUE)
  expect_match(widget, "Explain &lt;why&gt; &amp; how", fixed = TRUE)
  expect_match(widget, "Type &lt;here&gt;", fixed = TRUE)
  expect_match(widget, "data-docid=\"text-unit\"", fixed = TRUE)
  expect_match(widget, "data-label=\"short-answer\"", fixed = TRUE)
  expect_match(widget, "data-devoirs3-label=\"\" hidden=\"TRUE\"", fixed = TRUE)
  expect_match(widget, "rows=\"4\"", fixed = TRUE)
  expect_match(widget, "cols=\"32\"", fixed = TRUE)
  expect_match(widget, "Clear answer for Explain &lt;why&gt; &amp; how", fixed = TRUE)
})

test_that("text question labels are unique within a render", {
  devoirs3::devoirs_start("duplicate-test")
  devoirs3::devoirs_text("same-label", "First prompt")

  expect_error(
    devoirs3::devoirs_text("same-label", "Second prompt"),
    "Label 'same-label' is duplicated"
  )
})

test_that("text question arguments are validated", {
  devoirs3::devoirs_start("argument-test")

  expect_error(devoirs3::devoirs_text("", "Prompt"), "label")
  expect_error(devoirs3::devoirs_text("q1", "Prompt", width = 0), "width")
  expect_error(devoirs3::devoirs_text("q2", "Prompt", height = 1.5), "height")
})
