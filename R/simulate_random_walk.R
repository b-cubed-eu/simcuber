#' Simulates a random walk
#'
#' The function simulates occurrences of a species in a temporal extent.
#'
#' @param initial_average_abundance A positive integer value indicating the
#' average number of occurrences to be simulated within the extend of `polygon`
#' at time point 1. This value will be used as mean of a Poisson distribution
#' (lambda parameter).
#' @param n_time_points A positive integer value indicating the number of time
#' points to simulate.
#' @param pars A list of numeric values indicating parameters of
#' the random walk.
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
#'
#' timeseries <- simulate_random_walk(50, 10, c(0.1))

simulate_random_walk <- function(
    initial_average_abundance = 50,
    n_time_points = 10,
    pars = c(0.05),
    seed = NA) {
  # Checks
  # Check if pars is a numeric vector with length 1
  if (!is.numeric(pars) | length(pars) != 1) {
    cli::cli_abort(c(
      "{.var pars} must be an numeric vector of length 1.",
      "x" = paste("You've supplied a {.cls {class(pars)}} vector",
                  "of length {length(pars)}.")) 
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
  
  # Initialize an empty vector to store average abundance values
  lambdas <- numeric(n_time_points)
  
  # Set the initial abundance
  lambdas[1] <- initial_average_abundance
  
  # Unpack parameters
  sd_step <- pars[1]

  # Generate random steps and accumulate them
  for (i in 2:n_time_points) {
    step <- rnorm(1, mean = 0, sd = sd_step)
    lambdas[i] <- lambdas[i - 1] + step
  }
  
  # Identify where the lambda values become 0 or lower
  zero_or_lower_index <- which(lambdas <= 0)
  
  # If any lambda becomes 0 or lower, set all subsequent lambdas to 0
  if (length(zero_or_lower_index) > 0) {
    zero_or_lower_indices <- zero_or_lower_index[1]:n_time_points
    lambdas[zero_or_lower_indices] <- 0
  }
  
  # Return samples from Poisson
  return(rpois(n_time_points, lambdas))
}