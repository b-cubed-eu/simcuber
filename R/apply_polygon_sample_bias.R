#' Generate a sampling bias via a polygon
#'
#' This function adds a sampling bias weight column containing the sample
#' probability based on sampling bias within a polygon.
#'
#' @param observations An sf object with POINT geometry.
#'
#' @param bias_area An sf object with POLYGON geometry. The area in which the
#' sampling will be biased.
#'
#' @param bias_strength A positive numeric value. The strength of the bias to
#' be applied in the biased area (as a multiplier). Above 1, area will be
#' oversampled. Below 1, area will be undersampled. For example, a value of 50
#' will result in 50 times more samples within the bias_area than outside.
#' Conversely, a value of 0.5 will result in half less samples within the
#' bias_area than outside.
#'
#' @returns An sf object with POINT geometry with a bias_weight column
#' containing the sampling probability based on sampling bias.
#'
#' @importFrom cli cli_abort
#' @importFrom magrittr %>%
#' @importFrom sf st_within
#' @importFrom dplyr mutate
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
#' # Create bias_area polygon overlapping two of the points
#' selected_observations <- st_union(observations_sf[2:3,])
#' bias_area <- st_convex_hull(selected_observations) %>%
#'   st_buffer(dist = 100) %>%
#'   st_as_sf()
#'
#' bias_strength <- 2
#'
#' apply_polygon_sample_bias(observations_sf, bias_area, bias_strength)
#'
apply_polygon_sample_bias <- function(observations,
                                      bias_area,
                                      bias_strength = 1) {

  ### Start checks
  # 1. check input classes
  if (!"sf" %in% class(observations)) {
    cli::cli_abort(c(
      "{.var observations} must be an sf object.",
      "x" = "You've supplied a {.cls {class(observations)}} object."
    ))
  }

  if (!"sf" %in% class(bias_area)) {
    cli::cli_abort(c(
      "{.var bias_area} must be an sf object.",
      "x" = "You've supplied a {.cls {class(bias_area)}} object."
    ))
  }

  if (!"numeric" %in% class(bias_strength)) {
    cli::cli_abort(c(
      "{.var bias_strength} must be a numeric object.",
      "x" = "You've supplied a {.cls {class(bias_strength)}} object."
    ))
  }

  # 2. check input lengths
  if (length(bias_strength) != 1) {
    cli::cli_abort(c(
      "{.var bias_strength} must be a numeric vector of length 1.",
      "x" = paste(
        "You've supplied a {.cls {class(bias_strength)}} vector",
        "of length {length(bias_strength)}."
      )
    ))
  }

  ### End checks

  # Combine polygons into multipolygon
  bias_area <- sf::st_union(bias_area)

  # Find observations inside polygon
  in_bias_area <- observations %>%
    sf::st_within(bias_area, sparse = FALSE)

  # Calculate sampling probability based on bias strength
  bias_weights_outside_polygon <- 1 / (1 + bias_strength)
  bias_weights_inside_polygon <- bias_strength / (1 + bias_strength)

  #create bias_weight column
  observations <- observations %>%
    dplyr::mutate(bias_weight = ifelse(in_bias_area,
                                       bias_weights_inside_polygon,
                                       bias_weights_outside_polygon))

  return(observations)
}
