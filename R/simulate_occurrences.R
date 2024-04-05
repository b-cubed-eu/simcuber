#' Simulate occurrences within spatiotemporal scope
#'
#' The function simulates occurrences of a species within a given spatial
#' and/or temporal extend.
#'
#' @param plgn An sf object with POLYGON geometry indicating the spatial
#' extend to simulate occurrences.
#' @param initial_average_abundance A positive integer value indicating the
#' average number of occurrences to be simulated within the extend of `polygon`
#' at time point 1. This value will be used as mean of a Poisson distribution
#' (lambda parameter).
#' @param spatial_autocorr `"random"`, `"clustered"`, `"regular"` or a numeric
#' value between -1 and 1 representing Moran's I. `"random"` corresponds to 0,
#' `"clustered"` to 0.9 and `"regular"` to -0.9.
#' @param n_time_points A positive integer value indicating the number of time
#' points to simulate.
#' @param temporal_function `NA` (default), or a function which generates
#' a trend in abundance over time. Only used if `n_time_points > 1`. By default,
#' the function will sample `n_time_points` times from a Poisson
#' distribution with average (lambda) `initial_average_occurrences`. When a
#' function is specified (e.g. the internal `simulate_random_walk()` function).
#' @param spatiotemporal_autocorr A numeric value between indicating the
#' strength of spatiotemporal autocorrelation.
#' @param seed A positive numeric value. The seed for random number generation
#' to make results reproducible. If `NA` (the default), no seed is used.
#'
#' @returns An sf object with POINT geometry containing the locations of the
#' simulated occurrences and a `time_point` column containing the time point
#' associated with each occurrence.
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
#' # Define the coordinates of the polygon vertices
#' polygon <- st_polygon(list(rbind(c(0, 0),
#'                                  c(1, 0),
#'                                  c(1, 1),
#'                                  c(0, 1),
#'                                  c(0, 0))))
#' # Convert the polygon to an sf object
#' # Can be used as polygon input argument
#' polygon_sf <- st_sfc(polygon)

simulate_occurrences <- function(
    plgn,
    initial_average_abundance = 50,
    spatial_autocorr = c("random", "clustered", "regular"),
    n_time_points = 10,
    temporal_function = NA,
    spatiotemporal_autocorr = NA,
    seed = NA) {

  # Do some tests
  # to be done check plgn is a sf polygon

  # Simulate the timeseries
  ts <- simulate_timeseries(
    initial_average_occurrences = initial_average_abundance,
    n_time_points = n_time_points,
    temporal_function = temporal_function,
    seed = seed)

  # Create the random field
  boxplgn <- st_bbox(plgn)
  plgn_maxr <- max(boxplgn[3] - boxplgn[1], boxplgn[4] - boxplgn[2])
  res <- plgn_maxr / 100

  rs_pattern <- create_spatial_pattern(polygon = plgn,
                                       resolution = res,
                                       spatial_pattern = spatial_autocorr,
                                       seed = NA,
                                       n_sim = 1)

  # Sample occurrences from raster
  occ <- sample_occurrences(
    rs = rs_pattern,
    ts = ts)

  # Return the occurences (sf point geometry)
  return(occ)
}
