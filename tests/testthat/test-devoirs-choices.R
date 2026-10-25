test_that("the choices engine is registered and assigns normalized choices", {
  expect_true(is.function(knitr::knit_engines$get("devoirs_choices")))

  label <- "devoirsChoicesTest"
  environment <- knitr::knit_global()
  if (exists(label, envir = environment, inherits = FALSE)) {
    rm(list = label, envir = environment)
  }
  on.exit(rm(list = label, envir = environment), add = TRUE)

  output <- devoirs3:::.devoirs3_choices_engine(list(
    label = label,
    code = c('- "TRUE":', "    correct: true", "    feedback: Fantastic!", '- "False"')
  ))

  expect_identical(output, "")
  expect_identical(get(label, envir = environment), list(
    list(text = "TRUE", correct = TRUE, feedback = "Fantastic!"),
    list(text = "False", correct = FALSE, feedback = "Sorry.")
  ))
})

test_that("the choices engine validates chunk labels and choice data", {
  engine <- devoirs3:::.devoirs3_choices_engine

  expect_error(engine(list(label = "not valid", code = "- A")), "valid R variable name")
  expect_error(engine(list(label = "emptyChoices", code = "[]")), "non-empty sequence")
  expect_error(engine(list(label = "invalidCorrect", code = c("- A:", "    correct: \"true\""))), "correct.*YAML boolean")
  expect_error(engine(list(label = "unknownField", code = c("- A:", "    answer: true"))), "unknown metadata field")
  expect_error(engine(list(label = "invalidFeedback", code = c("- A:", "    feedback: null"))), "feedback.*string")
})