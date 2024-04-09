#' Sample from a circle using the Uniform distribution
#'
#' The function samples occurrences of a species within the uncertainty circle
#' around each observation assuming a Uniform distribution.
#'
#' @param observations An sf object with POINT geometry and a
#' `coordinateUncertaintyInMeters` column. If this column is not present, the
#' function will assume no (zero meters) uncertainty around the observation
#' points.
#' @param seed A positive numeric value. The seed for random number generation
#' to make results reproducible. If `NA` (the default), no seed is used.
#'
#' @returns An sf object with POINT geometry containing the locations of the
#' sampled occurrences and a `coordinateUncertaintyInMeters` column containing
#' the coordinate uncertainty for each observation.
#'
#' @export
#'
#' @import dplyr
#' @import sf
#' @importFrom stats runif
#' @importFrom rlang .data
#' @importFrom cli cli_abort cli_warn
#'
#' @examples
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
sample_from_uniform_circle <- function(
    observations,
    seed = NA) {
  ### Start checks
  # 1. check input lengths
  if (length(seed) != 1) {
    cli::cli_abort(c(
      "{.var seed} must be a numeric vector of length 1.",
      "x" = paste(
        "You've supplied a {.cls {class(seed)}} vector",
        "of length {length(seed)}."
      )
    ))
  }

  # 2. check input classes
  if (!"sf" %in% class(observations)) {
    cli::cli_abort(c(
      "{.var observations} must be an sf object",
      "x" = "You've supplied a {.cls {class(observations)}} object."
    ))
  }
  ### End checks

  # Set seed if provided
  if (!is.na(seed)) {
    if (is.numeric(seed)) {
      withr::local_seed(seed)
    } else {
      cli::cli_abort(c(
        "{.var seed} must be a numeric vector of length 1.",
        "x" = paste(
          "You've supplied a {.cls {class(seed)}} vector",
          "of length {length(seed)}."
        )
      ))
    }
  }

  # Set uncertainty to zero if column not present in data
  if (!"coordinateUncertaintyInMeters" %in% names(observations)) {
    observations$coordinateUncertaintyInMeters <- 0
    cli::cli_warn(
      paste(
        "No column {.var coordinateUncertaintyInMeters} present!",
        "Assuming no uncertainty around observations."
      )
    )
  }

  # Get random angle and radius
  uncertainty_points <-
    observations %>%
    dplyr::mutate(
      random_angle = stats::runif(nrow(observations), 0, 2 * pi),
      random_r = sqrt(stats::runif(nrow(observations), 0, 1)) *
        .data$coordinateUncertaintyInMeters
    )

  # Calculate new point
  new_points <-
    uncertainty_points %>%
    dplyr::mutate(
      x_new = sf::st_coordinates(.data$geometry)[, 1] +
        .data$random_r * cos(.data$random_angle),
      y_new = sf::st_coordinates(.data$geometry)[, 2] +
        .data$random_r * sin(.data$random_angle)
    ) %>%
    sf::st_drop_geometry() %>%
    sf::st_as_sf(
      coords = c("x_new", "y_new"),
      crs = sf::st_crs(observations)
    ) %>%
    dplyr::select("coordinateUncertaintyInMeters")

  return(new_points)
}
