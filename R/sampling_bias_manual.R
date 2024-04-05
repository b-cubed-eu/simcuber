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
#' # Load packages
#' library(sf)
#' library(dplyr)
#' library(ggplot2)
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
#' occurrences_sf <- st_as_sf(occurrences, coords = c("lon", "lat"))
#'
#' # Create raster grid
#' grid <- st_make_grid(occurrences_sf) %>%
#'   st_sf()
#'
#' # Bias weights between 0 and 1
#' grid1 <- grid %>%
#'   mutate(bias_weight = runif(nrow(grid), min = 0, max = 1))
#'
#' sampling_bias_manual(occurrences_sf, grid1)
#'
#' # Bias weights larger than 1
#' grid2 <- grid %>%
#'   mutate(bias_weight = rpois(nrow(grid), 5))
#'
#' occurrence_bias_sf <- sampling_bias_manual(occurrences_sf, grid2)
#' occurrence_bias_sf
#'
#' # Visualise where the bias is
#' ggplot() +
#'  geom_sf(data = grid2) +
#'  geom_sf_text(data = grid2, aes(label = bias_weight)) +
#'  geom_sf(data = occurrence_bias_sf, aes(colour = bias_weight)) +
#'  scale_color_gradient(trans = "reverse")

sampling_bias_manual <- function(occurrences_sf, bias_weights) {
  ### Start checks
  # 1. check input classes
  if (!("sf" %in% class(occurrences_sf))) {
    cli::cli_abort(c(
      "{.var occurrences_sf} must be an sf object.",
      "x" = "You've supplied a {.cls {class(occurrences_sf)}} object."
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

  # Check if crs is the same
  if (sf::st_crs(occurrences_sf) != sf::st_crs(bias_weights)) {
    cli::cli_abort("sf::st_crs(occurrences_sf) == sf::st_crs(bias_weights) is not TRUE")
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

  # Explicitly assume that the attribute is constant throughout the geometry
  sf::st_agr(occurrences_sf) <- "constant"
  sf::st_agr(bias_weights) <- "constant"

  # Take intersection to add bias weights to occurrence points
  weighted_occurrences <- sf::st_intersection(occurrences_sf, bias_weights)

  return(weighted_occurrences)
}
