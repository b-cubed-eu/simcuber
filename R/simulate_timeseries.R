#' Simulate timeseries for species abundances
#'
#' The function simulates a timeseries for the abundance of a species.
#'
#' @param initial_average_abundance A positive integer value indicating the
#' average number of occurrences to be simulated within the extend of `polygon`
#' at the first time point. This value will be used as mean of a Poisson
#' distribution (lambda parameter).
#' @param n_time_points A positive integer value indicating the number of time
#' points to simulate.
#' @param temporal_autocorr `NA` (default), or a function which generates
#' a trend in abundance over time. Only used if `time_points > 1`. By default,
#' the function will sample `n_time_points` times from a Poisson
#' distribution with average (lambda) `initial_average_abundance`. When a
#' function is specified (e.g. the internal `simulate_random_walk()` function)
#' `n_time_points` average abundances (lambdas) are calculated using
#' `initial_average_abundance` and any additional arguments passed.
#' See examples.
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
#' library(simcuber)
#' library(ggplot)
#'
#' ## 1. Use the function simulate_random_walk()
#' simulate_timeseries(
#'   initial_average_abundance = 50,
#'   n_time_points = 10,
#'   temporal_autocorr = simulate_random_walk,
#'   sd_step = 1,
#'   seed = 123)
#'
#' ## 2. Visualising multiple draws
#' # Set seed for reproducibility
#' set.seed(123)
#'
#' # Draw n_sim abundances from Poisson distribution using random walk
#' n_sim <- 10
#' n_time_points <- 50
#' sd_step <- 1
#' list_abundances <- vector("list", length = n_sim)
#'
#' # Loop n_sim times over simulate_timeseries()
#' for (i in seq_len(n_sim)) {
#'   abundances <- simulate_timeseries(
#'     initial_average_abundance = 50,
#'     n_time_points = n_time_points,
#'     temporal_autocorr = simulate_random_walk,
#'     sd_step = sd_step)
#'
#'   list_abundances[[i]] <- data.frame(
#'     time = seq_along(abundances),
#'     abundance = abundances,
#'     sim = i
#'     )
#' }
#'
#' # Combine list of dataframes
#' data_abundances <- do.call(rbind.data.frame, list_abundances)
#'
#' # Plot the simulated abundances over time using ggplot2
#' ggplot(data_abundances, aes(x = time, y = abundance, colour = factor(sim))) +
#'   geom_line() +
#'   labs(x = "Time", y = "Species abundance",
#'   title = paste(n_sim, "simulated abundances using random walk",
#'                 "with sd =", sd_step)) +
#'   scale_y_continuous(limits = c(0, NA)) +
#'   scale_x_continuous(breaks = seq(0, n_time_points, 5)) +
#'   theme_minimal() +
#'   theme(legend.position = "")
#'
#' ## 3. Using your own function
#' # You can also specify your own trend function, e.g. this linear function
#' my_own_linear_function <- function(
#'    initial_average_abundance = initial_average_abundance,
#'    n_time_points = n_time_points,
#'    coef) {
#'   # Calculate new average abundances over time
#'   time <- seq_len(n_time_points) - 1
#'   lambdas <- initial_average_abundance + (coef * time)
#'
#'   # Identify where the lambda values become 0 or lower
#'   zero_or_lower_index <- which(lambdas <= 0)
#'
#'   # If any lambda becomes 0 or lower, set all subsequent lambdas to 0
#'   if (length(zero_or_lower_index) > 0) {
#'     zero_or_lower_indices <- zero_or_lower_index[1]:n_time_points
#'     lambdas[zero_or_lower_indices] <- 0
#'   }
#'
#'   # Return average abundances
#'   return(lambdas)
#' }
#'
#' # Set seed for reproducibility
#' set.seed(123)
#'
#' # Draw n_sim abundances from Poisson distribution using our own function
#' n_sim <- 10
#' n_time_points <- 50
#' slope <- 1
#' list_abundances <- vector("list", length = n_sim)
#'
#' # Loop n_sim times over simulate_timeseries()
#' for (i in seq_len(n_sim)) {
#'   abundances <- simulate_timeseries(
#'     initial_average_abundance = 50,
#'     n_time_points = n_time_points,
#'     temporal_autocorr = my_own_linear_function,
#'     coef = slope)
#'
#'   list_abundances[[i]] <- data.frame(
#'     time = seq_along(abundances),
#'     abundance = abundances,
#'     sim = i
#'     )
#' }
#'
#' # Combine list of dataframes
#' data_abundances <- do.call(rbind.data.frame, list_abundances)
#'
#' # Plot the simulated abundances over time using ggplot2
#' ggplot(data_abundances, aes(x = time, y = abundance, colour = factor(sim))) +
#'   geom_line() +
#'   labs(x = "Time", y = "Species abundance",
#'   title = paste(n_sim, "simulated abundances using our own linear function",
#'                 "with slope", slope)) +
#'   scale_y_continuous(limits = c(0, NA)) +
#'   scale_x_continuous(breaks = seq(0, n_time_points, 5)) +
#'   theme_minimal() +
#'   theme(legend.position = "")

simulate_timeseries <- function(
    initial_average_abundance = 50,
    n_time_points = 10,
    temporal_autocorr = NA,
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
  if (suppressWarnings(!is.na(temporal_autocorr)) &
      !is.function(temporal_autocorr)) {
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
        ...)
    }
    timeseries <- rpois(n_time_points, lambdas)
  } else {
    # When it's NA, generate timeseries using a Poisson distribution
    timeseries <- rpois(n_time_points, initial_average_abundance)
  }

  return(timeseries)
}
