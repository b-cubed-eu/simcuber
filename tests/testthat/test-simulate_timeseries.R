# Unit tests
test_that("output length is correct", {
  # length of output > 1: case with temporal_function function
  expect_length(simulate_timeseries(50, 10, simulate_random_walk, 0.05), 10)

  # length of output is 1: case with temporal_function function
  expect_length(simulate_timeseries(50, 1, simulate_random_walk, 0.05), 1)

  # length of output > 1: case without temporal_function function
  expect_length(simulate_timeseries(50, 10), 10)

  # length of output is 1: case without temporal_function function
  expect_length(simulate_timeseries(50, 1), 1)
})
