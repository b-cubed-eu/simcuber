##' Create virtual species
#'
#' The function create a given number of virtual species and returns
#' an sf object with the virtual species occurrences as well as its environmental
#' suitability over space
#'
#' @param covariates_stack raster stack of the ecological variables from which
#' the virtual species are created.
#' @param polygon An sf object with POLYGON geometry indicating the spatial
#' extend to simulate occurrences. If `NA` (the default), the virtual species is
#' generated in the entire area of the stack.
#' @param n_time_points A positive integer value indicating the number of time
#' points to simulate (years).
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
#' output1 <- virtualspecies_to_sf(covariates_stack = stack_bio)
#' output2 <- virtualspecies_to_sf(covariates_stack = stack_bio, virtualsp_info = T)



virtualspecies_to_sf <- function(
    covariates_stack,
    polygon = NA,
    crs_c = 4326,
    seed = NA,
    first_year = 1990,
    last_year = 2020,
    n_time_points = 10,
    virtualsp_info = F){

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
  random.sp$details
  suitab_score <- random.sp$suitab.raster

  new.pres <- convertToPA(random.sp,
                          beta = "random",
                          plot = FALSE)

  # Presence as xy
  occurrences <- new.pres$pa.raster %>%
    terra::as.points() %>%
    st_as_sf() %>%
    filter(lyr.1 > 0) %>%
    st_transform(crs = crs_c)


  occurrences_ss <- occurrences %>%
    {ex <- extract(c(stack_bio, suitab_score),.)
    bind_cols(.,ex)}

  occurrences_ss <- occurrences_ss %>%
    dplyr::select(-lyr.1, -ID)

  occurrences_ss_time <- runif(nyears, first_year, last_year) %>%
    round(0) %>%
    sample(size = nrow(occurrences_ss), replace = T) %>%
    add_column(occurrences_ss, time_point = .) %>%
    dplyr::select(everything(), geometry)

  if(!virtualsp_info){
    return(virtual_occ = occurrences_ss_time)
  }else{
    return(list(virtual_occ = occurrences_ss_time, raster_ss = suitab_score, virtual_sp_info = random.sp$details))
  }

}



