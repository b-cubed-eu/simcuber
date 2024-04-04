#' Bias the sampling manually via a grid.
#'
#' The function uses a sampling bias that is manually provided by the user.
#' The user provides a grid layer (bias_weights) in which each cell contains the
#' probability to be sampled.
#'
#' @param occurrences_sf An sf object with POINT geometry.
#' @param bias_weights A raster layer (sf object with POLYGON geometry,
#' or SpatRaster object). The raster of bias weights to be applied to the
#' sampling of occurrences. Higher weights mean a higher probability of sampling.
#' Weights can be numeric values between 0 and 1 or positive integers that will
#' be rescaled to values between 0 and 1.
#'
#' @returns ...
#'
#' @export
#' @examples
#' # Set seed for reproducibility
#' set.seed(123)
#'
#' # Simulate some occurrence data with coordinates and time points
#' num_points <- 10
#' occurrences <- data.frame(
#'   lon = runif(num_points, min = -180, max = 180),
#'   lat = runif(num_points, min = -90, max = 90),
#'   time_point = 0
#'   )
#'
#' # Convert the occurrence data to an sf object
#' # Can be used as occurrences input argument
#' occurrences_sf <- st_as_sf(occurrences, coords = c("lon", "lat"))
#'
#' grid <- st_sf(st_make_grid(occurrences_sf) %>% st_sf)
#' grid$weights <- runif(nrow(grid), min = 0, max = 1)
#' bias_weights <- grid


#####
sampling_bias_manual <- function(occurrences_sf,bias_weights) {

  # Check the size of the grid, if too small, raise error

  # Check if rescaling is needed


}



