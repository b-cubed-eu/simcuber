# Set seed for reproducibility
withr:local_seed(123)

# Simulate some occurrence data with coordinates and time points
num_points <- 10
occurrences <- data.frame(
  lon = runif(num_points, min = -180, max = 180),
  lat = runif(num_points, min = -90, max = 90),
  time_point = 0
)
points_sf1 <- sf::st_as_sf(occurrences, coords = c("lon", "lat"))

## dataset with coordinateUncertaintyInMeters
coordinate_uncertainty <- rgamma(num_points, shape = 5, rate = 0.1)
points_sf2 <- points_sf1 %>%
  dplyr::mutate(coordinateUncertaintyInMeters = coordinate_uncertainty)

## dataset without geometry
points_sf3 <- points_sf2 %>%
  sf::st_drop_geometry()


# Unit tests
## expect errors
test_that("arguments are of the right class", {
  # occurrences is an sf object
  expect_error(sample_observations(occurrences = occurrences),
               regexp = "`occurrences` must be an sf object.",
               fixed = TRUE)
  expect_error(sample_observations(occurrences = points_sf3),
               regexp = "`occurrences` must be an sf object.",
               fixed = TRUE)
  expect_error(sample_observations(occurrences = "string"),
               regexp = "`occurrences` must be an sf object.",
               fixed = TRUE)

  # detection_probability is a numeric value
  expect_error(sample_observations(points_sf1, "1"),
               regexp = "`detection_probability` must be a numeric value between 0 and 1.",
               fixed = TRUE)
  expect_error(sample_observations(points_sf1,
                                   detection_probability = TRUE),
               regexp = "`detection_probability` must be a numeric value between 0 and 1.",
               fixed = TRUE)

  # # sampling_bias is a character vector
  # expect_error(sample_observations(points_sf1, 0.5, TRUE),
  #              regexp = "`points_sf2` must be a character vector of length 1",
  #              fixed = TRUE)

  # coordinate_uncertainty_meters is a numeric value
  expect_error(sample_observations(points_sf1,
                                   coordinate_uncertainty_meters = "1"),
               regexp = "`coordinate_uncertainty_meters` must be a positive numeric value.",
               fixed = TRUE)


  # bias_area ...

  # bias_strength ...

  # bias_weights ...

  # seed is a numeric value
  expect_error(sample_observations(points_sf1, seed = -7),
               regexp = "`seed` must be a positive integer.",
               fixed = TRUE)
expect_error(sample_observations(points_sf1, seed = -1),
             regexp = "`seed` must be a positive integer.",
             fixed = TRUE)
  expect_error(sample_observations(points_sf1, seed = 2.34),
               regexp = "`seed` must be NA or a positive integer.",
               fixed = TRUE)
 })
