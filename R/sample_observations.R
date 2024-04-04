#' Sample observations from a larger occurrence dataset
#'
#' The function samples observations from occurrences based on detection
#' probability and sampling bias.
#'
#' @param occurrences An sf object with POINT geometry.
#' @param detection_probability A numeric value between 0 and 1, corresponding
#' to the probability of detection of the species.
#' @param sampling_bias `"no_bias"`, `"polygon"` or `"manual"`. The method used
#' to generate a sampling bias. `"polygon"`: bias the sampling in a polygon.
#' Provide your polygon to `bias_area`. Provide bias strength to
#' `bias_strength`. `"manual"`: bias the sampling manually via a raster.
#' Provide your raster layer in which each cell contains the probability to be
#' sampled to `bias_weights`.
#' @param bias_area `NA` or an sf object with POLYGON geometry. Only used if
#' `sampling_bias = "polygon"`. The area in which the sampling will be biased.
#' @param bias_strength `NA` or a positive numeric value. Only used if
#' `sampling_bias = "polygon"`. The strength of the bias to be applied in the
#' biased area (as a multiplier). Above 1, area will be oversampled. Below 1,
#' area will be undersampled. For example, a value of 50 will result in 50
#' times more samples within the `bias_area` than outside. Conversely, a value
#' of 0.5 will result in half less samples within the `bias_area` than outside.
#' @param bias_weights `NA` or a raster layer (sf object with POLYGON geometry,
#' or SpatRaster object). Only used if `sampling_bias = "manual"`. The raster
#' of bias weights to be applied to the sampling of occurrences. Higher weights
#' mean a higher probability of sampling. Weights can be numeric values between
#' 0 and 1 or positive integers that will be rescaled to values between 0 and 1.
#' @param coordinate_uncertainty_meters A positive numeric value or vector with
#' length `nrow(occurrences)` describing the uncertainty in meters around each
#' observation.
#' @param seed A positive numeric value. The seed for random number generation
#' to make results reproducible. If `NA` (the default), no seed is used.
#'
#' @returns An sf object with POINT geometry containing the locations of the
#' sampled observations, a `detection_probability` column containing the
#' detection probability for each observation (will be the same for all), and a
#' `bias_weight` column containing the sampling probability based on sampling
#' bias, a `sampling_probability` column containing the combined sampling
#' probability from detection probability and sampling bias for each
#' observation.
#'
#' @export
#'
#' @examples
#'
#' # add example here ...
#'
#' # This is just to create an example polygon for input
#' library(sf)
#'
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

sample_observations <- function(
    occurrences,
    detection_probability = 1,
    sampling_bias = c("no_bias", "polygon", "manual"),
    bias_area = NA,
    bias_strength = 1,
    seed = NA) {
  # Add detection probability
  occurrences$detection_probability <- detection_probability

  # Create and merge bias weight with occurrences
  if (length(sampling_bias) > 1) {
    sampling_bias <- sampling_bias[1]
  }
  if (sampling_bias == "polygon") {
    occurrences <- apply_polygon_sample_bias(
      observations = occurrences,
      bias_area = bias_area,
      bias_strength = bias_strength)
  } else if (sampling_bias == "manual") {
    cli::cli_abort("Manual option still in development!")
  } else {
    occurrences$bias_weights <- 1
  }

  # Combine detection and bias probabilities and sample observations
  occurrences <- occurrences %>%
    dplyr::mutate(
      sampling_probability = detection_probability * bias_weights
    ) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(sample_status = stats::rbinom(1, 1, sampling_probability))

  # Filter observations
  observations <- occurrences %>%
    dplyr::filter(sample_status == 1) %>%
    dplyr::select(time_point, detection_probability, bias_weights,
                  sampling_probability, geometry)

  # Return the observed occurrences
  return(observations)
}
