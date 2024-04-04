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
#' detection probability for each observation (will be the same for all), a
#' `bias_weight` column containing the sampling probability based on sampling
#' bias, a `sampling_probability` column containing the combined sampling
#' probability from detection probability and sampling bias, and a
#' `coordinateUncertaintyInMeters` column containing the coordinate uncertainty
#' for each observation.
#'
#' @export
#'
#' @examples
#'
#' # add example here ...
#'
#' # This is just to create an example polygon for input
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
#' points_sf <- st_as_sf(occurrences, coords = c("lon", "lat"))



sample_observations <- function(
    occurrences,
    detection_probability = 1,
    sampling_bias = c("no_bias", "polygon", "manual"),
    bias_area = NA,
    bias_strength = NA,
    bias_weights = NA,
    coordinate_uncertainty_meters = 25,
    seed = NA) {

  ### Start checks

  # 1. check input classes
  # To do: checks for bias_area, bias_strength and bias_weights
  if (!("sf" %in% class(occurrences))) {
    cli::cli_abort(c(
      "{.var occurrences} must be an sf object",
      "x" = "You've supplied a {.cls {class(occurrences)}} object."
    ))
  }
  if (!is.numeric(detection_probability)) {
    cli::cli_abort(c(
      "{.var detection_probability} must be a numeric value between 0 and 1",
      "x" = "You've supplied a {.cls {class(detection_probability)}} object."
    ))
  }
  if (!is.character(sampling_bias)) {
    cli::cli_abort(c(
      "{.var sampling_bias} must be a character vector of length 1.",
      "x" = "You've supplied a {.cls {class(sampling_bias)}} vector"
    ))
  }
  if (!is.numeric(coordinate_uncertainty_meters)) {
    cli::cli_abort(c(
      "{.var coordinate_uncertainty_meters} must be a positive numeric value",
      "x" = "You've supplied a {.cls {class(coordinate_uncertainty_meters)}}
      object."
    ))
  }
  if (!is.integer(seed) & !is.na(seed)) {
    cli::cli_abort(c(
      "{.var seed} must be NA or a positive integer",
      "x" = "You've supplied a {.cls {class(occurrences)}} object."
    ))
  }

  # 2. other checks
  # detection_probability is a numeric value between 0 and 1
  if ((!(0 <= detection_probability) & !(detection_probability <= 1))) {
    cli::cli_abort(c(
      "{.var detection_probability} must be a numeric value between 0 and 1",
      "x" = "You've supplied {.cls {class(detection_probability)}} as
      {.var detection_probability}."
    ))
  }
# seed is a positive value
if (seed <= 0) {
  cli::cli_abort(c(
    "{.var seed} must be a positive integer",
    "x" = "You've supplied {seed} as {.var seed}."
  ))
}

  ### End checks

  occurrences <- occurrences %>%
    dplyr::mutate(detection_probability = detection_probability)
  print(detection_probability)

  # ...
}
