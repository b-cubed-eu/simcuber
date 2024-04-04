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
#' library(dplyr)
#' library(ggplot2)
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
    initial_average_abundance = NA,
    n_time_points = NA,
    population_change = NA,
    n_samp = NA) {

  stopifnot("initial_average_abundance must be given" = is.numeric(initial_average_abundance))
  stopifnot("n_time_points must be given" = is.numeric(n_time_points))
  stopifnot("population_change must be given" = is.numeric(population_change))
  stopifnot("n_samp must be given" = is.numeric(n_samp))

# create empty matrix
  abundances <- matrix(nrow = n_samp,
                       ncol = n_time_points)

  # set initial abundance
  abundances[,1] <- rpois(n_samp, initial_average_abundance)

  # loop over time points
  for (i in 2:n_time_points) {
    # Abundance at time t is dependent on the abundance at time t-1
    abundances[,i] <- abundances[,i - 1] + abundances[,i - 1] * population_change
  }

  return(abundances)
}
