test_that("add_coordinate_uncertainty() returns sf object", {
  # create observations_sf object to be used as an input
  observations_sf <-
    data.frame(
      lat = runif(4, 3110000, 3112000),
      long = runif(4, 3841000, 3842000)
    ) %>%
    st_as_sf(coords = c("long", "lat"), crs = 3035)

  # Expect sf as class
  expect_s3_class(
    add_coordinate_uncertainty(observations_sf,
      coords_uncertainty_meters = 1000
    ),
    "sf"
  )
})

test_that("add_coordinate_uncertainty() add the right coordinate uncertainty", {
  # set the number of observations and coordinate uncertainty to test
  n_observations <- 19
  coords_uncertanty_set <- 4876
  # create observations_sf object to be used as an input
  observations_sf <-
    data.frame(
      lat = runif(n_observations, 3110000, 3112000),
      long = runif(n_observations, 3841000, 3842000)
    ) %>%
    st_as_sf(coords = c("long", "lat"), crs = 3035)

  expect_identical(
    add_coordinate_uncertainty(observations_sf,
      coords_uncertainty_meters = coords_uncertanty_set
    )$coordinateUncertaintyInMeters,
    rep(coords_uncertanty_set, n_observations)
  )
})

test_that("add_coordinate_uncertainty() can handle different uncertainties per point", {
  # set the number of observations and coordinate uncertainty to test
  n_observations <- 23
  coords_uncertanty_set <- sample(1000:4000, size = n_observations)
  # create observations_sf object to be used as an input
  observations_sf <-
    data.frame(
      lat = runif(n_observations, 3110000, 3112000),
      long = runif(n_observations, 3841000, 3842000)
    ) %>%
    st_as_sf(coords = c("long", "lat"), crs = 3035)

  expect_identical(
    add_coordinate_uncertainty(observations_sf,
                               coords_uncertainty_meters = coords_uncertanty_set
    )$coordinateUncertaintyInMeters,
    coords_uncertanty_set
  )
})

test_that("add_coordinate_uncertainty() returns error on wrong length of coords_uncertainty_meters", {
  # set the number of observations and coordinate uncertainty to test
  n_observations <- 7
  # create observations_sf object to be used as an input
  observations_sf <-
    data.frame(
      lat = runif(n_observations, 3110000, 3112000),
      long = runif(n_observations, 3841000, 3842000)
    ) %>%
    st_as_sf(coords = c("long", "lat"), crs = 3035)

  # Use an expectation per line of the error.
  expect_error(
    add_coordinate_uncertainty(
      observations_sf,
      coords_uncertainty_meters = rep(1234, n_observations + 1)
    ),
    regexp = "`coords_uncertainty_meters` has diferent length than the number of rows in `occurrences`",
    fixed = TRUE
  )

  expect_error(
    add_coordinate_uncertainty(
      observations_sf,
      coords_uncertainty_meters = rep(1234, n_observations + 1)
    ),
    regexp = "You've supplied `coords_uncertainty_meters` of length 8 but `occurrences` has 7 rows."
  )
})

test_that("add_coordinate_uncertainty() returns error on non sf occurrence input", {
  not_an_sf_object <- data.frame(1:5, 5, 6)

  # Use an expectation per line of the error.
  expect_error(
    add_coordinate_uncertainty(not_an_sf_object),
    regexp = "`occurrences` must be an object of class 'sf'",
    fixed = TRUE
  )
  expect_error(
    add_coordinate_uncertainty(not_an_sf_object),
    regexp = "You've supplied an object of class <data.frame>",
    fixed = TRUE
  )
})

test_that("add_coordinate_uncertainty() returns error when occurrence geompetry is different from POINT", {

  }
)

test_that("add_coordinate_uncertainty() returns error when coordinate uncertainty is not a number", {
  # create observations_sf object to be used as an input
  observations_sf <-
    data.frame(
      lat = runif(4, 3110000, 3112000),
      long = runif(4, 3841000, 3842000)
    ) %>%
    st_as_sf(coords = c("long", "lat"), crs = 3035)

  expect_error(
    add_coordinate_uncertainty(
      observations_sf,
      coords_uncertainty_meters = "not a number"
    ),
    regexp = "coords_uncertainty_meters must be a numeric value",
    fixed = TRUE
  )
})
