#' Add coordinate uncertainty
#'
#' Adds a collum to the occurrences sf object with the coordinate uncertainty in meters
#'
#'
#' @param occurrences A sf object with POINT geometry
#' @param coords_uncertainty_meters a value or a vector of numeric values
#'
#' @return
#' A sf object with POINT geometry with an additional column named 'coordinateUncertaintyInMeters'
#' @export
#'
#' @examples
#'
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
#'
#' @importFrom sf st_geometry_type
#' @importFrom cli cli_abort
#'
add_coordinate_uncertainty <- function(
    occurrences,
    coords_uncertainty_meters = 25) {

  ## checks
  ## is it sf object
  if (!inherits(occurrences, "sf")) {
    cli::cli_abort(c(
      "{.var occurrences}  must be an object of class 'sf'",
      "x" = paste(
        "You've supplied an object of class {.cls {class(occurrences)}}"
      )
    ))
  }
  ## check if coords_uncertainty_meters is numeric
  if(!is.numeric(coords_uncertainty_meters)) {
    cli::cli_abort(
      "{.var coords_uncertainty_meters must be a numeric value}"
    )
  }

  ## is geometry type POINT?
  is_point <- sf::st_geometry_type(occurrences, by_geometry = FALSE) == "POINT"
  if(!is_point) {
    cli::cli_abort(c(
      "{.var occurrences} must be a 'sf' object with POINT geometry",
      "x" = "You've supplied an 'sf' object of geometry type {.cls {sf::st_geometry_type(occurrences, by_geometry = FALSE)}}"
      )
    )
   }

  ## number of points in sf object and the coords_uncertainty_meters must be the
  ## same when coords_uncertainty_meters is larger than 1
  if(length(coords_uncertainty_meters) != 1){
    size_match <- length(coords_uncertainty_meters) == nrow(occurrences)

    if(!size_match){
      cli::cli_abort(
        c(
          "{.var coords_uncertainty_meters} has diferent length than the number of rows in {.var occurrences}",
          "x" = paste("You've supplied {.var coords_uncertainty_meters} of length {length(coords_uncertainty_meters)}",
                      "but {.var occurrences} has {nrow(occurrences)} rows.")
        )
     )

    }
  }

  occurrences$coordinateUncertaintyInMeters <- coords_uncertainty_meters

  return(occurrences)
}
