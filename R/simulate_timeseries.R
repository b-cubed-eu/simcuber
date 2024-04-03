#' Simulate timeseries for species abundances
#'
#' The function simulates a timeseries for the abundance of a species.
#'
#' @param initial_average_abundance A positive integer value indicating the
#' average number of occurrences to be simulated within the extend of `polygon`
#' at the first time point. This value will be used as mean of a Poisson
#'  distribution (lambda parameter).
#' @param n_time_points A positive integer value indicating the number of time
#' points to simulate.
#' @param temporal_autocorr `NA`, or a function which generates
#' a trend in abundance over time. Only used if `time_points > 1`. When there
#' are multiple time points the function will by default use the internal
#' `simulate_random_walk()` function.
#' @param ... Additional argument to be passed to the `temporal_autocorr`
#' function.
#' @param seed A positive numeric value. The seed for random number generation
#' to make results reproducible. If `NA` (the default), no seed is used.
#'
#' @returns A vector of integers of length n_time_points with the number of
#' occurrences.
#'
#' @export
#'
#' @examples
#'
#' timeseries <- simulate_timeseries(
#'   initial_average_abundance = 50,
#'   n_time_points = 10,
#'   temporal_autocorr = simulate_random_walk,
#'   sd_step = 0.1)
#'

simulate_timeseries <- function(
    initial_average_abundance = 50,
    n_time_points = 10,
    temporal_autocorr = ifelse(n_time_points ==  1, NA, simulate_random_walk),
    ...,
    seed = NA) {
  # Checks
  # Check if initial_average_abundance is a positive integer
  if (!is.numeric(initial_average_abundance) | initial_average_abundance <= 0) {
      cli::cli_abort(c(
      "{.var initial_average_abundance} must be a positive integer.",
      "x" = paste("You've supplied a {.cls {class(initial_average_abundance)}}",
                  "value of {initial_average_abundance}."))
      )
  }
  # Check if n_time_points is a positive integer
  if (!is.numeric(n_time_points) | n_time_points <= 0) {
      cli::cli_abort(c(
      "{.var n_time_points} must be a positive integer.",
      "x" = paste("You've supplied a {.cls {class(n_time_points)}}",
                  "value of {n_time_points}."))
  )
  }
  # Check if temporal_autocorr is NA or a function
  if (!is.na(temporal_autocorr) & !is.function(temporal_autocorr)) {
      cli::cli_abort(c(
      "{.var temporal_autocorr} must be `NA` or a function.",
      "x" = paste("You've supplied a {.cls {class(temporal_autocorr)}}",
                  "value of {temporal_autocorr}."))
      )
  }
  # Set seed if provided
  if (!is.na(seed)) {
    if (is.numeric(seed)) {
      set.seed(seed)
    } else {
      cli::cli_abort(c(
        "{.var seed} must be an numeric vector of length 1.",
            "x" = paste("You've supplied a {.cls {class(seed)}} vector",
                        "of length {length(seed)}."))
            )
        }
    }

  # Check type of temporal_autocorr
  # If temporal_autocorr is a function, use it to generate the timeseries
  if (is.function(temporal_autocorr)) {
    # Collect additional arguments
    length_pars <- length(list(...))

    # If arguments are empty, pass nothing to the function
    if (length_pars == 0) {
      # Generate timeseries using the provided function
      lambdas <- temporal_autocorr(
        initial_average_abundance,
        n_time_points,
        seed = seed)
    } else {
      # Generate timeseries using the provided function
      lambdas <- temporal_autocorr(
        initial_average_abundance,
        n_time_points,
        ...,
        seed = seed)
    }
    timeseries <- rpois(n_time_points, lambdas)
  } else {
    # When it's NA, generate timeseries using a Poisson distribution
    timeseries <- rpois(n_time_points, initial_average_abundance)
  }

  return(timeseries)
}
