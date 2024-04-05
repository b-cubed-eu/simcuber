#' Create spatial pattern
#'
#' It creates a raster with a spatial pattern for the area of a polygon.
#'
#' @param polygon an sf object of geometry type POLYGON
#' @param resolution a numeric value defining the resolution of the raster cell
#' @param spatial_pattern define the spatial pattern. It could be a character
#'   string "random" or "clustered", in which "random" is the default. The user
#'   is able to provide a numeric value between 1 and 100. 1 is "random" and 5
#'   is "clustered. As large the number more broad is the size of the clusters
#'   area.
#' @param seed integer. set a seed to randomization
#' @param n_sim number of simulations. Each simulation is a different layer in
#'   the raster.
#'
#' @return an object of class SpatRaster
#' @export
#'
#' @examples
#'
#' library(sf)
#'
#' plgn <- st_polygon(list(cbind(c(5,10,8,2,3,5), c(2,1,7,9,5,2))))
#' plot(plgn)
#' # random pattern by default
#' spat_random <- create_spatial_pattern(
#'   plgn,
#'   resolution = 0.1,
#'   seed = 123)
#'
#' plot(spat_random, main = "random pattern")
#'
#' # built in clustered pattern
#'
#' spat_clust<- create_spatial_pattern(
#'   plgn,
#'   resolution = 0.1,
#'   spatial_pattern = "clustered",
#'   seed = 123)
#'
#' plot(spat_clust, main = "built in clustered pattern")
#'
#' # user defined spatial pattern
#' ## small scale cluster
#'
#' spat_small <- create_spatial_pattern(
#'   plgn,
#'   resolution = 0.1,
#'   spatial_pattern = 5,
#'   seed = 123)
#'
#' plot(spat_small, main = "small scale clustered pattern")
#'
#' ## medium scale cluster (the built in clustered pattern)
#' spat_medium <- create_spatial_pattern(
#'   plgn,
#'   resolution = 0.1,
#'   spatial_pattern = 10,
#'   seed = 123)
#'
#' plot(spat_medium, main = "medium scale clustered pattern")
#'
#' ## large scale cluster
#' spat_large <- create_spatial_pattern(
#'   plgn,
#'   resolution = 0.1,
#'   spatial_pattern = 100,
#'   seed = 123)
#'
#' plot(spat_large, main = "large scale clustered pattern")
#'
#'
#' @import terra
#' @import sf
#' @import gstat
#' @importFrom dplyr between mutate across starts_with
#' @importFrom cli cli_abort
#' @importFrom withr local_seed
#' @importFrom vegan decostand
#'
create_spatial_pattern <- function(
  polygon,
  resolution,
  spatial_pattern = c("random", "clustered"),
  seed = NA,
  n_sim = 1
){

  if (length(seed) != 1) {
    cli::cli_abort(c(
      "{.var seed} must be a numeric vector of length 1.",
      "x" = paste(
        "You've supplied a {.cls {class(seed)}} vector",
        "of length {length(seed)}."
      )
    ))
  }

  # create a reference raster with same extent as the polygon and user defined
  # resolution
  templ <- terra::rast(terra::vect(polygon), res = resolution)
  poly_raster <- terra::rasterize(terra::vect(plgn), templ)

  dfxy <- as.data.frame(poly_raster, xy = TRUE)

  # define the spatial pattern ----
  if(is.character(spatial_pattern)){

    if(spatial_pattern[1] == "random"){
      multiplier <- 1
    }
    if(spatial_pattern[1] == "clustered"){
      multiplier <- 10
    }

    if(!any(spatial_pattern[1] %in% c("random", "clustered"))){
      cli::cli_abort(
        c(paste(
          "When class of {.var spatial_pattern} is",
          "{.cls {class(spatial_pattern)}} you should provide 'random' or
          'clustered' as string"),
          "x" = "You've provided the string '{spatial_pattern}' in
          {.var spatial_pattern}"
        )
      )
    }

    ## should have a stopper when is not one of the options

  }

  #
  if(is.numeric(spatial_pattern)){
    # value should be between 1 and 100
    if(dplyr::between(spatial_pattern, 1, 100)){
      multiplier <- spatial_pattern
    }else{
      cli::cli_abort("")
    }
  }

  range_size <- resolution * multiplier

  ## generate the pattern

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

  # !idea: we can allow the user to provide a gstat object with other vgm model
  gstat_model <- gstat::gstat(
    formula = z~1,
    locations = ~x+y,
    dummy = T,
    beta = 1,
    model = gstat::vgm(
      psill = 0.5,
      model = "Sph",
      range = range_size,
      nugget = 0),
    nmax = 2)

  dfxy_pred <- stats::predict(gstat_model, newdata = dfxy, nsim = n_sim)


  # standardize values between 0 and 1
  dfxy_std <- dfxy_pred %>%
    dplyr::mutate(
      dplyr::across(
        dplyr::starts_with("sim"),
        ~vegan::decostand(.x, "range")
        )
    )

  r <- terra::rast(dfxy_std)

  r
}
