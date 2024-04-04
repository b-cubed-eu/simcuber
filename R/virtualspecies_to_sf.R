#' Create virtual species
#'
#' The function create a given number of virtual species and returns
#' an sf object with the virtual species occurrences as well as its environmental
#' suitability over space
#'
#'
#' @param nspecies numeric values indicate the number of desired virtual species
#' @param covariates_stack raster stack of the ecological variables from which
#' the virtual species are created.
#' @param polygon An sf object with POLYGON geometry indicating the spatial
#' extend to simulate occurrences. If `NA` (the default), the virtual species is
#' generated in the entire area of the stack.
#' @param n_time_points A positive integer value indicating the number of time
#' points to simulate.
#' @param seed A positive numeric value. The seed for random number generation
#' to make results reproducible. If `NA` (the default), no seed is used.
#'
#' @returns a list of two object. 1) An sf object with POINT geometry containing the locations of the
#' virtual species occurrences and a `time_point` column containing the time point
#' associated with each occurrence. 2) A rasterstack object with the suitabity map
#' for each virtual species generated
#'
#' @export
#'
#' @examples
#'
#' library(simcuber)
#' library(sf)
#' library(tidyverse)
#' library(virtualspecies)
#' library(geodata)
#'
#' stack_bio <- worldclim_country(country = "Belgium", var = "bio", res=0.5, path=tempdir())
#'
#' out <- virtualspecies_to_sf(nsp = 1, covariates_stack = stack_bio)
#
stack_bio %>% nrow()
virtualspecies_to_sf <- function(
    nsp = 10,
    covariates_stack,
    polygon = NA,
    seed = NA){

  if(!is.na(polygon)){
    covariates_stack <- covariates_stack %>%
      terra::mask(polygon)
    if(nrow(covariates_stack))
      cli::cli_abort(c(
      "polygon must overlap the covariates_stack",
    ))
  }

  random.sp <- generateRandomSp(raster.stack = covariates_stack,
                                convert.to.PA = FALSE,
                                species.type = "multiplicative",
                                relations = "gaussian",
                                realistic.sp = TRUE,
                                plot = FALSE)

  new.pres <-convertToPA(random.sp,
                         beta = "random",
                         alpha = -0.05, plot = FALSE,
                         species.prevalence = 0.02)

  presence.points <- sampleOccurrences(new.pres,
                                       n = 40,
                                       type = "presence-absence",
                                       sample.prevalence = 0.5,
                                       detection.probability = 0.8,
                                       correct.by.suitability = TRUE,
                                       plot = TRUE)

  Outputs_i = data.frame(i, presence.points$sample.points)
  names(Outputs_i) <- c("species", "x", "y")


}
