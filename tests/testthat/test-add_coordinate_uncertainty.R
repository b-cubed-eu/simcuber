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

test_that(
  "add_coordinate_uncertainty() returns error when occurrence geompetry is different from POINT",
  {

  }
)
