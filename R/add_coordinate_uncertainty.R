#' Add coordinate uncertainty to observations
#'
#' Adds a column to the observations sf object with the coordinate uncertainty
#' in meters.
#'
#'
#' @param observations An sf object with POINT geometry.
#' @param coords_uncertainty_meters A numeric value or a vector of numeric
#' values indicating the coordinate uncertainty associated with observations.
#'
#' @return An sf object with POINT geometry with an additional column
#' `coordinateUncertaintyInMeters`.
#'
#' @export
#'
#' @import sf
#' @importFrom cli cli_abort
#'
#' @family main
#'
#' @examples
#'
#' library(sf)
#' library(dplyr)
#'
#' set.seed(123)
#'
#' # Create four random points
#' n_points <- 4
#' xlim <- c(3841000, 3842000)
#' ylim <- c(3110000, 3112000)
#' observations_sf <- data.frame(
#'   lat = runif(n_points, ylim[1], ylim[2]),
#'     long = runif(n_points, xlim[1], xlim[2])) %>%
#'     st_as_sf(coords = c("long", "lat"), crs = 3035)
#'
#'  # provide a fixed uncertainty for all points
#'  add_coordinate_uncertainty(
#'    observations_sf,
#'    coords_uncertainty_meters = 1000
#'    )
#'
#' # add variability in uncertainty. For example, using gamma distribution
#' add_coordinate_uncertainty(
#'   observations_sf,
#'   coords_uncertainty_meters = rgamma(n_points, shape = 5, rate = 0.1)
#' )

add_coordinate_uncertainty <- function(
    observations,
    coords_uncertainty_meters = 25) {

  ## checks
  ## is it sf object
  if (!inherits(observations, "sf")) {
    cli::cli_abort(c(
      "{.var observations}  must be an object of class 'sf'",
      "x" = paste(
        "You've supplied an object of class {.cls {class(observations)}}"
      )
    ))
  }
  ## check if coords_uncertainty_meters is numeric
  if (!is.numeric(coords_uncertainty_meters)) {
    cli::cli_abort(
      "{.var coords_uncertainty_meters must be a numeric value}"
    )
  }

  ## is geometry type POINT?
  is_point <- sf::st_geometry_type(observations, by_geometry = FALSE) == "POINT"
  if (!is_point) {
    cli::cli_abort(c(
      "{.var observations} must be a 'sf' object with POINT geometry",
      paste("x" = "You've supplied an 'sf' object of geometry type {.cls",
            "{sf::st_geometry_type(observations, by_geometry = FALSE)}}")
      )
    )
   }

  ## number of points in sf object and the coords_uncertainty_meters must be the
  ## same when coords_uncertainty_meters is larger than 1
  if (length(coords_uncertainty_meters) != 1) {
    size_match <- length(coords_uncertainty_meters) == nrow(observations)

    if (!size_match) {
      cli::cli_abort(
        c(
          paste("{.var coords_uncertainty_meters} has diferent length than the",
                "number of rows in {.var observations}"),
          "x" = paste("You've supplied {.var coords_uncertainty_meters} of",
                      "length {length(coords_uncertainty_meters)}",
                      "but {.var observations} has {nrow(observations)} rows.")
        )
     )
    }
  }

  observations$coordinateUncertaintyInMeters <- coords_uncertainty_meters

  return(observations)
}
