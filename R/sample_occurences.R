#' Sample occurences from spatial random field
#'
#' Draws occurences (points) from a spatial random field (raster)
#'
#' @param rs a raster object (terra)
#' @param ts vector with the number of occurences by time step
#'
#'@return
#'An sf object with POINT geometry
#'@export
#'
#' @importFrom sf st_geometry_type
#' @importFrom spatstat spatSample

sample_occurences <- function(
    rs,
    ts){

  # checks
  # check if rs is a terra raster
  if (!is(rs, "SpatRaster")){
    cli::cli_abort(c("{.var rs} is not a SpatRaster."))
  }

  # check if ts is a integer vector with values >= 0
  if (!is.na(ts)) {
    if (!is.numeric(n)) {
      cli::cli_abort(c(
        "{.var ts} must be an numeric vector"
        ))
    }
  }

  # rescale the values of the raster between 0 and 1
  rsminmax <- minmax(rs)
  rs2 <- (rs - rsminmax[1])/(rsminmax[2] - rsminmax[1])

  # For each time step sample points from the raster
  for
  occ_p <- spatSample(x = rs2, size = ts[1], method = "weights",
                     replace = TRUE,  as.points = TRUE)

  return(occ_p)

}
