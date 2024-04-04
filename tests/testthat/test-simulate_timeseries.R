# Unit tests
## expect errors
test_that("arguments are of the right class", {
  # initial_average_abundance must be a positive integer
  expect_error(simulate_timeseries(initial_average_abundance = "a"),
    class = "simcuber_error_wrong_argument_type"
  )
  expect_error(simulate_timeseries(initial_average_abundance = 0),
    class = "simcuber_error_wrong_argument_type"
  )
  # n_time_points must be a positive integer
  expect_error(simulate_timeseries(n_time_points = "a"),
    class = "simcuber_error_wrong_argument_type"
  )
  expect_error(simulate_timeseries(n_time_points = 0),
    class = "simcuber_error_wrong_argument_type"
  )
  # temporal_autocorr must be NA or a function
  expect_error(simulate_timeseries(temporal_autocorr = "a"),
    class = "simcuber_error_wrong_argument_type"
  )
  expect_error(simulate_timeseries(temporal_autocorr = 1),
    class = "simcuber_error_wrong_argument_type"
  )
  # seed must be an numeric vector of length 1
  expect_error(simulate_timeseries(seed = "a"),
    class = "simcuber_error_wrong_argument_type"
  )
})

test_that("output length is correct", {
  # length of output > 1: case with temporal_autocorr function
  expect_length(simulate_timeseries(50, 10, simulate_random_walk, 0.05), 10)

  # length of output is 1: case with temporal_autocorr function
  expect_length(simulate_timeseries(50, 1, simulate_random_walk, 0.05), 10)

  # length of output > 1: case without temporal_autocorr function
  expect_length(simulate_timeseries(50, 10), 10)

  # length of output is 1: case without temporal_autocorr function
  expect_length(simulate_timeseries(50, 1), 1)
})
