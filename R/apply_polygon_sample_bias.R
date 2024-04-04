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
#' # Create polygon overlapping two of the points
#' selected_observations <- st_union(observations_buffered[2:3,])
#' bias_area <- st_convex_hull(selected_multipolygon) %>%
#'   st_buffer(dist = 100) %>%
#'   st_as_sf()
#'
#' bias_strength <- 2
#'
#' apply_polygon_sample_bias(observations, bias_area, bias_strength)
#'
#'
#' )
apply_polygon_sample_bias <- function(observations, bias_area, bias_strength) {
  require(sf)

  ### Start checks
  ### End checks

  # Find observations inside polygon
  in_bias_area <- observations %>%
    st_within(bias_area, sparse = FALSE)

  # Calculate sampling probability based on bias strength
  bias_weights_outside_polygon <- 1 / (1 + bias_strength)
  bias_weights_inside_polygon <- bias_strength / (1 + bias_strength)

  observations <- observations %>%
  mutate(bias_weights = ifelse(in_bias_area, bias_weights_inside_polygon, bias_weights_outside_polygon))

  return(observations)
}
