#' Generate a sampling bias via a polygon
#'
#' This function adds a sampling bias weight column containing the sample
#' probability based on sampling bias within a polygon.
#'
#'
#' inputs - bias.area, bias.strength, occurrences
#' outputs - occurrences with a bias.weight column added (a bias_weight column containing the sampling probability based on sampling bias)
#'
#' The argument bias.strength indicates the strength of the bias. For example, a value of 50 will
#' result in 50 times more samples within the bias.area than outside. Conversely, a value of 0.5 will
#' result in half less samples within the bias.area than outside.
#'
#' @param observations An sf object with POINT geometry and a
#' `coordinateUncertaintyInMeters` column. If this column is not present, the
#' function will assume no (zero meters) uncertainty around the observation
#' points.
#'
#' @returns An sf object with POINT geometry containing the locations of the
#' sampled occurrences and a `coordinateUncertaintyInMeters` column containing
#' the coordinate uncertainty for each observation.
#'
#' @export
#'
#' @examples
#'
#' library(simcuber)
#' library(sf)
#'
#' set.seed(123)
#'
#' # Create four random points
#' n_points <- 4
#' xlim <- c(3841000, 3842000)
#' ylim <- c(3110000, 3112000)
#' coordinate_uncertainty <- rgamma(n_points, shape = 5, rate = 0.1)
#'
#' observations_sf <- data.frame(
#'   lat = runif(n_points, ylim[1], ylim[2]),
#'   long = runif(n_points, xlim[1], xlim[2]),
#'   coordinateUncertaintyInMeters = coordinate_uncertainty
#' ) %>%
#'   st_as_sf(coords = c("long", "lat"), crs = 3035)
#'
#' # Sample points within uncertainty circles according to uniform rules
#' sample_from_uniform_circle(
#'   observations = observations_sf,
#'   seed = 123
#' )
apply_polygon_sample_bias <- function(occurrences, bias_area, bias_strength, seed = NA) {
  require(sf)

  ### Start checks
  ### End checks

  # Find occurrences inside polygon
  in_bias_area <- st_within(occurrences, bias_area)

  # Calculate sampling probability based on bias value
  sampling_probability <- ifelse(
    point.in.polygon(spatial_points$x, spatial_points$y, polygon@polygons[[1]]@coords[,1], polygon@polygons[[1]]@coords[,2]) == 1,
    bias_value, 1
  )

  return(occurrences)
}
