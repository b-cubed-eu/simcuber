#' Function to generate temporally autocorrelated occurrences


#' @param initial_average_abundance A positive integer value indicating the
#' average number of occurrences to be simulated within the extend of `polygon`
#' at time point 1. This value will be used as mean of a Poisson distribution
#' (lambda parameter).
#' @param n_time_points A positive integer value indicating the number of time
#' points to simulate.
#' @param population_change A positive or negative numerical value fo the population growth rate
#' @param n_samp number of samples to take

#' @return a matrix of number of occurrences with columns as time steps and rows as samples
#' @examples
#'
<<<<<<< HEAD
#' library(tidyverse)
#' set.seed(123)
=======
#' library(dplyr)
#' library(ggplot2)
>>>>>>> 796d996c3d554dc4896fc41f9b64f745523b62e1
#' occ <- custom_occurences(initial_average_abundance = 20, n_time_points = 10,
#'                          population_change = 0.2, n_samp = 100)
#'
#' occ %>%
#'   data.frame() %>%
#'   mutate(samp_id = 1:nrow(.) %>% as.factor()) %>%
#'   pivot_longer(-samp_id, names_to = "time", values_to = "n_occurrence") %>%
#'   mutate(time = str_remove(time, pattern = "X") %>% as.numeric()) %>%
#'   ggplot(aes(x = time, y = n_occurrence,col = samp_id)) +
#'   geom_line() +
#'   theme(legend.position = "none")


custom_occurences <- function(
    initial_n_occ = NA,
    n_time_points = NA,
    population_change = NA,
    n_samp = NA) {

  if (length(initial_n_occ) != 1) {
    cli::cli_abort(c(
      "{.var id_col} must be a character vector of length 1.",
      "x" = paste(
        "You've supplied a {.cls {class(id_col)}} vector",
        "of length {length(id_col)}."
      )
    ))
  }

# create empty matrix
  occ <- numeric(n_time_points)

  # set initial abundance
<<<<<<< HEAD
  occ[,1] <- rpois(1, initial_n_occ)
=======
  abundances[,1] <- stats::rpois(n_samp, initial_average_abundance)
>>>>>>> 796d996c3d554dc4896fc41f9b64f745523b62e1

  # loop over time points
  for (i in 2:n_time_points) {
    # Occurences at time t is dependent on the occurences at time t-1
    occ[i] <- occ[i - 1] + occ[i - 1] * population_change
  }

  return(occ)
}


occ <- custom_occurences(initial_average_abundance = 20, n_time_points = 10,
                         n_samp = 100, population_change = runif(1, min = -5, m))
