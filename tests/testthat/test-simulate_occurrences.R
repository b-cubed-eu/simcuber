test_that("sample_occurrences() fails on invalid input argument", {
  expect_error(
    sample_occurrences(data.frame(5:9))
  )
})
