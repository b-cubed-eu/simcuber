# Unit tests
## expect errors
test_that("arguments are of the right class", {
  # initial_average_abundance must be a positive integer
  expect_error(simulate_timeseries(initial_average_abundance = "a"),
               regexp = "`initial_average_abundance` must be a positive integer.",
               fixed = TRUE)
  expect_error(simulate_timeseries(initial_average_abundance = 0),
               regexp = "`initial_average_abundance` must be a positive integer.",
               fixed = TRUE)
  # n_time_points must be a positive integer
  expect_error(simulate_timeseries(n_time_points = "a"),
               regexp = "`n_time_points` must be a positive integer.",
               fixed = TRUE)
  expect_error(simulate_timeseries(n_time_points = 0),
               regexp = "`n_time_points` must be a positive integer.",
               fixed = TRUE)
  # temporal_autocorr must be NA or a function
  expect_error(simulate_timeseries(temporal_autocorr = "a"),
               regexp = "`temporal_autocorr` must be `NA` or a function.",
               fixed = TRUE)
  expect_error(simulate_timeseries(temporal_autocorr = 1),
               regexp = "`temporal_autocorr` must be `NA` or a function.",
               fixed = TRUE)
  # seed must be an numeric vector of length 1
  expect_error(simulate_timeseries(seed = "a"),
               regexp = "`seed` must be an numeric vector of length 1.",
               fixed = TRUE)
  # n_time_points is 1, temporal_autocorr must be NA
  expect_error(
    simulate_timeseries(n_time_points = 1,
                        temporal_autocorr = simulate_random_walk),
    regexp = "When `n_time_points` is 1, `temporal_autocorr` must be NA.",
    fixed = TRUE
  )
})

test_that("output length is correct", {
  expect_length(simulate_timeseries(50, 10, simulate_random_walk, 0.05), 10)
  expect_length(simulate_timeseries(50, 1), 1)
  })
