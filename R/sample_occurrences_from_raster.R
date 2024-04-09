#' Sample occurrences from spatial random field
#'
#' Draws occurrences (points) from a spatial random field (raster)
#'
#' @param rs a raster object (terra)
#' @param ts vector with the number of occurrences by time step
#'
#' @return An sf object with POINT geometry
#'
#' @export
#'
#' @import sf
#' @importFrom terra spatSample global
#'
#' @examples
#' library(terra)
#' library(sf)
#'
#' r <- rast(ncol = 50, nrow = 50, xmin = 0, xmax = 50, ymin = 0, ymax = 50)
#' values(r) <- 1:ncell(r)
#' timeseries <- c(20, 40, 60)
#'
#' pts_occ <- sample_occurrences_from_raster(rs = r, ts = timeseries)
#'
#' plot(r)
#' plot(pts_occ, add = TRUE, color = "black")
#'

sample_occurrences_from_raster <- function(
    rs,
    ts) {
  # checks
  # check if rs is a terra raster
  if (!"SpatRaster" %in% class(rs)) {
    cli::cli_abort(c("{.var rs} is not a SpatRaster."))
  }

  # check if ts is a numeric vector
  if (!is.numeric(ts)) {
    cli::cli_abort(c("{.var ts} must be an numeric vector"))
  }

  # centre the values of the raster (mean = 0)
  rs_mean <- terra::global(rs, "mean", na.rm = TRUE)[, 1]
  rs2 <- rs - rs_mean

  # increase contrast between high and low values
  a <- 30 # a = 1 -> logistic  a > 1  => steeper sigmoid (higher contrast)
  rs3 <- 1 / (1 + exp(-a * rs2))

  # For each time step sample points from the raster
  # Should be recoded: with lapply? or map?

  occ_pf <- NULL

  for (t in 1:length(ts)) {
    occ_p <- terra::spatSample(
      x = rs3, size = ts[t], method = "weights",
      replace = TRUE, as.points = TRUE
    )
    occ_sf <- sf::st_as_sf(occ_p)
    occ_sf$time_point <- t
    occ_pf <- rbind(occ_pf, occ_sf)
  }

  # points need to be shifted randomly (uniform within the raster cell size)
  # For the moment the points are all at the center of the raster cells

  return(occ_pf)
}
