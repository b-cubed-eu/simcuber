#' Generate a sampling bias via a grid.
#'
#' The function uses a sampling bias that is manually provided by the user.
#' The user provides a grid layer (bias_weights) in which each cell contains the
#' probability to be sampled.
#'
#' @param occurrences_sf An sf object with POINT geometry.
#' @param bias_weights A raster layer (sf object with POLYGON geometry). The
#' raster of bias weights to be applied to the sampling of occurrences. This sf
#' object should contain a `bias_weight` and `geometry` column. Higher weights
#' indicate a higher probability of sampling. Weights must be numeric values
#' between 0 and 1 OR positive integers that will be rescaled to values between
#' 0 and 1.
#'
#' @returns An sf object with POINT geometry with a bias_weight column
#' containing the sampling probability based on sampling bias.
#'
#' @export
#'
#' @importFrom cli cli_abort
#' @importFrom sf st_intersection st_filter st_agr
#'
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
#' grid$bias_weight <- runif(nrow(grid), min = 0, max = 1)
#' bias_weights <- grid

sampling_bias_manual <- function(occurrences_sf, bias_weights) {
  ### Start checks
  # 1. check input classes
  if (!("sf" %in% class(occurrences))) {
    cli::cli_abort(c(
      "{.var occurrences} must be an sf object.",
      "x" = "You've supplied a {.cls {class(occurrences)}} object."
    ))
  }
  if (!"sf" %in% class(bias_weights)) {
    cli::cli_abort(c(
      "{.var bias_weights} must be an sf object.",
      "x" = "You've supplied a {.cls {class(bias_weights)}} object."
    ))
  }

  # 2. Other checks
  # Check if bias_weights has a column named bias_weight
  if (!("bias_weight" %in% names(bias_weights))) {
    cli::cli_abort(c(
      "{.var bias_weights} must have a column named `bias_weight`.",
      "x" = "You've supplied a grid {.var bias_weights} that has column names
      {names(bias_weights)}."
    ))
  }

  # Check if all occurrences (points) are in the grid
  points_in_grid <- sf::st_filter(occurrences_sf, bias_weights)
  if (!identical(points_in_grid, occurrences_sf)) {
    cli::cli_abort(c(
      "{.var bias_weights} must be a grid that encompasses all occurrences.",
      "x" = "You've supplied a grid that does not encompass all occurrences."
    ))
  }

  # Check if the values of bias_weights$bias_weight are positive
  if (!is.numeric(bias_weights$bias_weight)) {
    cli::cli_abort(c(
      paste("The column `bias_weight` must consist of numeric values between 0",
            "and 1 OR positive integers."),
      "x" = "The column `bias_weight` does not contain numeric values."
    ))
  }
  if (!(all(bias_weights$bias_weight >= 0))) {
    cli::cli_abort(c(
      paste("The column `bias_weight` must consist of numeric values between 0",
            "and 1 OR positive integers."),
      "x" = "The column `bias_weight` has negative values."
    ))
  }
  if (max(bias_weights$bias_weight) > 1) {
    if (!all(bias_weights$bias_weight %% 1 == 0)) {
      cli::cli_abort(c(
        paste("The column `bias_weight` must consist of numeric values between",
              "0 and 1 OR positive integers."),
        "x" = paste("The column `bias_weight` has values larger than 1 but",
                    "they are not all integers.")
      ))
    }
  }
  ### End checks

  # Rescale bias_weight if needed
  maxweight <- max(bias_weights$bias_weight)
  if (maxweight > 1) {
    bias_weights$bias_weight <- bias_weights$bias_weight / maxweight
  }

  # Intersection
  sf::st_agr(occurrences_sf) <- "constant"
  sf::st_agr(bias_weights) <- "constant"
  weighted_occurrences <- sf::st_intersection(occurrences_sf, bias_weights)

  return(weighted_occurrences)
}
