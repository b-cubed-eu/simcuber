#' Sample from a circle using the bivariate Normal distribution
#'
#' The function samples occurrences of a species within the uncertainty circle
#' around each observation assuming a bivariate Normal distribution with means
#' equal to the observation point and the variance equal to
#' (-`coordinateUncertaintyInMeters`^2) / (2 * log(1 - `p_norm`)) such that
#' `p_norm` % of all possible samples from this Normal distribution fall within
#' the uncertainty circle.
#'
#' @param observations An sf object with POINT geometry and a
#' `coordinateUncertaintyInMeters` column. If this column is not present, the
#' function will assume no (zero meters) uncertainty around the observation
#' points.
#' @param p_norm A numeric value between 0 and 1. The proportion of all
#' possible samples from a a bivariate Normal distribution that fall within the
#' uncertainty circle. If no value is given, the default `p_norm` value is 0.95.
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
#' @importFrom rlang .data
#' @importFrom cli cli_abort cli_warn
#' @importFrom mnormt rmnorm
#'
#' @family designation
#'
#' @examples
#' library(sf)
#' library(dplyr)
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
#' # Sample points within uncertainty circles according to normal rules
#' sample_from_binormal_circle(
#'   observations = observations_sf,
#'   p_norm = 0.95,
#'   seed = 123
#' )

sample_from_binormal_circle <- function(
    observations,
    p_norm = 0.95,
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
  if (length(p_norm) != 1) {
    cli::cli_abort(c(
      "{.var p_norm} must be a vector of length 1.",
      "x" = paste(
        "You've supplied a vector",
        "of length {length(p_norm)}."
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

  # 3. other checks
  # p_norm should be numeric between 0 and 1
  if (!is.numeric(p_norm)) {
    cli::cli_abort(c(
      "{.var p_norm} must be a numeric vector of length 1.",
      "x" = paste(
        "You've supplied a {.cls {class(aggregate)}} vector",
        "of length {length(aggregate)}."
      )
    ))
  }
  if (p_norm <= 0 || p_norm >= 1) {
    if (is.numeric(p_norm)) {
      cli::cli_abort(c(
        "{.var p_norm} must be a single value between 0 and 1.",
        "x" = "You've supplied the value(s) {p_norm}."
      ))
    }
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

    # New points are equal to original points in case of no uncertainty
    new_points <- observations %>%
      dplyr::select("coordinateUncertaintyInMeters")
  } else {
    # Calculate 2-dimensional means and variance-covariance matrices
    means <- sf::st_coordinates(observations$geometry)
    variances <- (-observations$coordinateUncertaintyInMeters^2) /
      (2 * log(1 - p_norm))
    varcovariances <- lapply(variances, function(var) {
      matrix(c(var, -1, -1, var), nrow = 2)
    })

    # Sample new points from bivariate Normal distribution
    new_points_list <- vector("list", length = nrow(observations))
    for (i in seq_len(nrow(observations))) {
      new_points_list[[i]] <- mnormt::rmnorm(
        1,
        mean = means[i, ], varcov = varcovariances[[i]]
      )
    }
    new_points_df <- do.call(rbind.data.frame, new_points_list)
    names(new_points_df) <- c("x_new", "y_new")

    # Create geometry and add uncertainties
    new_points <- cbind(
      new_points_df,
      coordinateUncertaintyInMeters = observations$coordinateUncertaintyInMeters
    ) %>%
      sf::st_as_sf(coords = c("x_new", "y_new"), crs = sf::st_crs(observations))
  }

  return(new_points)
}
