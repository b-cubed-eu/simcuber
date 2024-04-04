#' Bias the sampling manually via a grid.
#'
#' The function uses a sampling bias that is manually provided by the user.
#' The user provides a grid layer in which each cell contains the
#' probability to be sampled to bias_weights.
#'
#' @param grid An sf object with POLYGON geometry (usually a grid) to which
#' observations should be designated.
#' @param bias_weights `NA` or a raster layer (sf object with POLYGON geometry,
#' or SpatRaster object). Only used if `sampling_bias = "manual"`. The raster
#' of bias weights to be applied to the sampling of occurrences. Higher weights
#' mean a higher probability of sampling. Weights can be numeric values between
#' 0 and 1 or positive integers that will be rescaled to values between 0 and 1.
#'
#' @returns ...
#'
#' @export
sampling_bias_manual <- function(
    grid, bias_weights) {
  # ...

}
