test_that("rolling_hash is deterministic for the same input", {
  expect_identical(devoirs3:::rolling_hash("abc"), 2946)
  expect_identical(devoirs3:::rolling_hash("abc"), devoirs3:::rolling_hash("abc"))
})
