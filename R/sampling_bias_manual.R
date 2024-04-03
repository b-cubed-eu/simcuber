#' Bias the sampling manually via a grid.
#'
#' The function uses a sampling bias that is manually provided by the user.
#' The user provides a grid layer in which each cell contains the
#' probability to be sampled to bias_weights.
#'
#' @param grid An sf object with POLYGON geometry (usually a grid) to which
#' observations should be designated.
#'
sampling_bias_manual <- function(
    grid) {
  # ...
}

