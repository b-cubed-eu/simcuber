#' Function to generate temporally autocorrelated occurrences


#' @param initial_n_occ A positive integer value indicating the
#' average number of occurrences to be simulated within the extend of `polygon`
#' at time point 1. This value will be used as mean of a Poisson distribution
#' (lambda parameter).
#' @param n_time_points A positive integer value indicating the number of time
#' points to simulate.
#' @param pop_change A positive or negative numerical value fo the population growth rate
#' @param n_samp number of samples to take
#' @param pop_stoch if TRUE it is possible to sample the population change rate
#' from a normal distribution with user defined mean and sd
#' @return a matrix of number of occurrences with columns as time steps and rows as samples
#' @examples
#'
#' library(tidyverse)
#' occ <- custom_occurrences(initial_n_occ = 20, n_time_points = 10,
#'                          pop_change = 0.2, n_samp = 100)
#'
#' occ <- custom_occurrences(initial_n_occ = 20, n_time_points = 10,
#'                          pop_change = 0.2, pop_stoch = T, mean = -0.1, sd = 0.5)
#' occ %>%
#'   data.frame() %>%
#'   mutate(samp_id = 1:nrow(.) %>% as.factor()) %>%
#'   pivot_longer(-samp_id, names_to = "time", values_to = "n_occurrence") %>%
#'   mutate(time = str_remove(time, pattern = "X") %>% as.numeric()) %>%
#'   ggplot(aes(x = time, y = n_occurrence,col = samp_id)) +
#'   geom_line() +
#'   theme(legend.position = "none")
#'
#'


custom_occurrences <- function(
    initial_n_occ = NA,
    n_time_points = NA,
    pop_change = NA,
    pop_stoch = FALSE,
    n_samp = NA,
    ...) {

  if (length(initial_n_occ) != 1 & is.numeric(initial_n_occ)) {
    cli::cli_abort(c(
      "{.var id_col} must be a numeric vector of length 1.",
      "x" = paste(
        "You've supplied a {.cls {class(id_col)}} vector",
        "of length {length(id_col)}."
      )
    ))
  }
  if (length(n_time_points) != 1 & is.numeric(n_time_points)) {
    cli::cli_abort(c(
      "{.var id_col} must be a numeric vector of length 1.",
      "x" = paste(
        "You've supplied a {.cls {class(id_col)}} vector",
        "of length {length(id_col)}."
      )
    ))
  }

# create empty matrix
  occ <- matrix(ncol = n_time_points,
                    nrow = n_samp)

  # set initial abundance
  occ[,1] <- rpois(n_samp, initial_n_occ)

  # loop over time points
  for (i in 2:n_time_points) {
    if(pop_stoch == FALSE){
      # Occurrences at time t is dependent on the occurrences at time t-1
      occ[,i] <- occ[,i - 1] + occ[,i - 1] * pop_change
    } else
      occ[,i] <- occ[,i - 1] + occ[,i - 1] * rnorm(1, ...)
    }

  return(occ)
}
