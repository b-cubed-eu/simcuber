# Prepare example datasets
## number of points and extend
n_points <- 4
xlim <- c(3841000, 3842000)
ylim <- c(3110000, 3112000)

## dataset without coordinateUncertaintyInMeters
observations_sf1 <- data.frame(
  lat = runif(n_points, ylim[1], ylim[2]),
  long = runif(n_points, xlim[1], xlim[2])
  ) %>%
  st_as_sf(coords = c("long", "lat"), crs = 3035)

## dataset with coordinateUncertaintyInMeters
set.seed(123)
coordinate_uncertainty <- rgamma(n_points, shape = 5, rate = 0.1)
observations_sf2 <- observations_sf1 %>%
  mutate(coordinateUncertaintyInMeters = coordinate_uncertainty)

## dataset without geometry
observations_sf3 <- observations_sf2 %>%
  st_drop_geometry()

# Add buffer uncertainty in meters around points
observations_sf2_buffered <- observations_sf2 %>%
  sf::st_buffer(observations_sf2$coordinateUncertaintyInMeters)

# Create grid
grid_df1 <- sf::st_make_grid(
  observations_sf2_buffered,
  square = TRUE,
  cellsize = c(200, 200)
  ) %>%
  sf::st_sf()

grid_df2 <- grid_df1 %>%
  mutate(id = seq_len(nrow(grid_df1)))

## grid without geometry
grid_df3 <- grid_df1 %>%
  st_drop_geometry()

# Unit tests
## expect errors
test_that("arguments are of the right class", {
  # observations are sf dataframe
  expect_error(grid_designation(observations_sf3, grid_df1),
               regexp = "`observations` must be an sf object",
               fixed = TRUE)
  expect_error(grid_designation(observations = 2, grid_df1),
               regexp = "`observations` must be an sf object",
               fixed = TRUE)
  expect_error(grid_designation(observations = "string", grid_df1),
               regexp = "`observations` must be an sf object",
               fixed = TRUE)

  # grid is sf dataframe
  expect_error(grid_designation(observations_sf2, grid_df3),
              regexp = "`grid` must be an sf object",
              fixed = TRUE)
  expect_error(grid_designation(observations_sf2, grid = 2),
              regexp = "`grid` must be an sf object",
              fixed = TRUE)
  expect_error(grid_designation(observations_sf2, grid = "string"),
              regexp = "`grid` must be an sf object",
              fixed = TRUE)

  # id_col is string
  expect_error(grid_designation(observations_sf2, grid = grid_df1,
                                id_col = 3),
               regexp = "`id_col` must be a character vector of length 1.",
               fixed = TRUE)

  # randomisation is string
  expect_error(grid_designation(observations_sf2, grid = grid_df1,
                                randomisation = 3),
               regexp = "`randomisation` must be a character vector.",
               fixed = TRUE)

  # aggregate is logical
  expect_error(grid_designation(observations_sf2, grid = grid_df1,
                                aggregate = "TRUE"),
               regexp = "`aggregate` must be a logical vector of length 1.",
               fixed = TRUE)
})

test_that("arguments are of the right length", {
  # id_col has length 1
  expect_error(grid_designation(observations_sf2, grid = grid_df1,
                                id_col = c("col1", "col2")),
               regexp = "`id_col` must be a character vector of length 1.",
               fixed = TRUE)

  # aggregate has length 1
  expect_error(grid_designation(observations_sf2, grid = grid_df1,
                                aggregate = rep(TRUE, 3)),
               regexp = "`aggregate` must be a logical vector of length 1.",
               fixed = TRUE)
})

test_that("crs of observations and grid must match", {
  expect_error(
    grid_designation(observations_sf2,
                     grid = sf::st_transform(grid_df1, crs = 4326)),
    regexp = "sf::st_crs(observations) == sf::st_crs(grid) is not TRUE",
    fixed = TRUE)
})

test_that('randomisation should be one of "uniform", "normal"', {
  expect_error(
    grid_designation(observations_sf2, grid_df1, randomisation = "beta"),
    regexp = '`randomisation` should be one of "uniform", "normal".',
    fixed = TRUE)
})

## expect warnings
test_that("unique ids if id column is provided", {
  expect_warning(
    grid_designation(observations_sf2,
                     grid = grid_df1 %>%
                       mutate(id = 1),
                     id_col = "id"),
    regexp = "Column `id` does not contain unique ids for grid cells!",
    fixed = TRUE)
})

test_that("provided id column present in provided grid", {
  expect_warning(
    grid_designation(observations_sf2,
                     grid = grid_df1 %>%
                       mutate(id = seq_len(nrow(grid_df1))),
                     id_col = "identifier"),
    regexp = 'Column name "identifier" not present in provided grid!',
    fixed = TRUE)
})

## expected outputs
test_that("output class is correct", {
  # aggregate = TRUE
  suppressWarnings({
    expect_s3_class(grid_designation(observations_sf1, grid = grid_df1),
                    class = "sf")
    expect_s3_class(grid_designation(observations_sf1, grid = grid_df1),
                    class = "data.frame")
  })
  expect_s3_class(grid_designation(observations_sf2, grid = grid_df1),
                  class = "sf")
  expect_s3_class(grid_designation(observations_sf2, grid = grid_df1),
                  class = "data.frame")

  # aggregate = FALSE
  suppressWarnings({
    expect_s3_class(grid_designation(observations_sf1, grid = grid_df1,
                                     aggregate = FALSE),
                    class = "sf")
    expect_s3_class(grid_designation(observations_sf1, grid = grid_df1,
                                     aggregate = FALSE),
                    class = "data.frame")
  })
  expect_s3_class(grid_designation(observations_sf2, grid = grid_df1,
                                   aggregate = FALSE),
                  class = "sf")
  expect_s3_class(grid_designation(observations_sf2, grid = grid_df1,
                                   aggregate = FALSE),
                  class = "data.frame")
})

test_that("correct column names present", {
  # aggregate = TRUE, randomisation = "uniform"
  suppressWarnings({
    expect_contains(names(grid_designation(observations_sf1, grid = grid_df1)),
                    c("id", "n", "min_coord_uncertainty", "geometry"))
  })
  expect_contains(names(grid_designation(observations_sf2, grid = grid_df1)),
                  c("id", "n", "min_coord_uncertainty", "geometry"))
  expect_contains(
    names(grid_designation(
      observations_sf2,
      grid = grid_df1 %>%
        mutate(identifier = seq_len(nrow(grid_df1))),
      id_col = "identifier")),
    c("identifier", "n", "min_coord_uncertainty", "geometry"))

  # aggregate = TRUE, randomisation = "normal"
  suppressWarnings({
    expect_contains(names(grid_designation(observations_sf1, grid = grid_df1,
                                           randomisation = "normal")),
                    c("id", "n", "min_coord_uncertainty", "geometry"))
  })
  expect_contains(names(grid_designation(observations_sf2, grid = grid_df1,
                                         randomisation = "normal")),
                  c("id", "n", "min_coord_uncertainty", "geometry"))
  expect_contains(
    names(grid_designation(
      observations_sf2,
      grid = grid_df1 %>%
        mutate(identifier = seq_len(nrow(grid_df1))),
      id_col = "identifier",
      randomisation = "normal")),
    c("identifier", "n", "min_coord_uncertainty", "geometry"))

  # aggregate = FALSE, randomisation = "uniform"
  suppressWarnings({
    expect_contains(names(grid_designation(observations_sf1, grid = grid_df1,
                                           aggregate = FALSE)),
                    c("id", "coordinateUncertaintyInMeters", "geometry"))
  })
  expect_contains(names(grid_designation(observations_sf2, grid = grid_df1,
                                         aggregate = FALSE)),
                  c("id", "coordinateUncertaintyInMeters", "geometry"))
  expect_contains(
    names(grid_designation(
      observations_sf2,
      grid = grid_df1 %>%
        mutate(identifier = seq_len(nrow(grid_df1))),
      id_col = "identifier",
      aggregate = FALSE)),
    c("identifier", "coordinateUncertaintyInMeters", "geometry"))

  # aggregate = FALSE, randomisation = "normal"
  suppressWarnings({
    expect_contains(names(grid_designation(observations_sf1, grid = grid_df1,
                                           aggregate = FALSE,
                                           randomisation = "normal")),
                    c("id", "coordinateUncertaintyInMeters", "geometry"))
  })
  expect_contains(names(grid_designation(observations_sf2, grid = grid_df1,
                                         aggregate = FALSE,
                                         randomisation = "normal")),
                  c("id", "coordinateUncertaintyInMeters", "geometry"))
  expect_contains(
    names(grid_designation(
      observations_sf2,
      grid = grid_df1 %>%
        mutate(identifier = seq_len(nrow(grid_df1))),
      id_col = "identifier",
      aggregate = FALSE,
      randomisation = "normal")),
    c("identifier", "coordinateUncertaintyInMeters", "geometry"))
})

test_that("no minimal coordinate uncertainty for empty grid cells", {
  # randomisation = "uniform"
  suppressWarnings({
    expect_equal(sum(
                   grid_designation(observations_sf1,
                                    grid = grid_df1)$n == 0),
                 sum(is.na(
                   grid_designation(observations_sf1,
                                    grid = grid_df1)$min_coord_uncertainty))
                 )
  })
  expect_equal(sum(
    grid_designation(observations_sf2,
                     grid = grid_df1)$n == 0),
    sum(is.na(
      grid_designation(observations_sf2,
                       grid = grid_df1)$min_coord_uncertainty))
  )

  # randomisation = "normal"
  suppressWarnings({
    expect_equal(sum(
      grid_designation(observations_sf1,
                       grid = grid_df1,
                       randomisation = "normal")$n == 0),
      sum(is.na(
        grid_designation(observations_sf1,
                         grid = grid_df1,
                         randomisation = "normal")$min_coord_uncertainty))
    )
  })
  expect_equal(sum(
    grid_designation(observations_sf2,
                     grid = grid_df1,
                     randomisation = "normal")$n == 0),
    sum(is.na(
      grid_designation(observations_sf2,
                       grid = grid_df1,
                       randomisation = "normal")$min_coord_uncertainty))
  )
})

# Calculate all potential grid cells for the observations
sf::st_agr(observations_sf1) <- "constant"
sf::st_agr(observations_sf2_buffered) <- "constant"
sf::st_agr(grid_df2) <- "constant"
# No uncertainty
potential_gridcells_sf1 <- st_intersection(grid_df2, observations_sf1) %>%
  dplyr::pull(id)
# With uncertainty
potential_gridcells_sf2 <- st_intersection(grid_df2,
                                           observations_sf2_buffered) %>%
  dplyr::pull(id)

test_that("check possible outcomes for grid cell designation", {
  # aggregate = TRUE, randomisation = "uniform"
  suppressWarnings({
    expect_contains(potential_gridcells_sf1,
                    grid_designation(observations_sf1, grid = grid_df2,
                                     id_col = "id") %>%
                      dplyr::filter(n > 0) %>%
                      dplyr::pull(id))
  })
  expect_contains(potential_gridcells_sf2,
                  grid_designation(observations_sf2, grid = grid_df2,
                                   id_col = "id") %>%
                    dplyr::filter(n > 0) %>%
                    dplyr::pull(id))
  # aggregate = TRUE, randomisation = "normal"
  suppressWarnings({
    expect_contains(potential_gridcells_sf1,
                    grid_designation(observations_sf1, grid = grid_df2,
                                     id_col = "id",
                                     randomisation = "normal") %>%
                      dplyr::filter(n > 0) %>%
                      dplyr::pull(id))
  })
  expect_contains(potential_gridcells_sf2,
                  grid_designation(observations_sf2, grid = grid_df2,
                                   id_col = "id",
                                   randomisation = "normal") %>%
                    dplyr::filter(n > 0) %>%
                    dplyr::pull(id))

  # aggregate = FALSE, randomisation = "uniform"
  suppressWarnings({
    expect_contains(potential_gridcells_sf1,
                    grid_designation(observations_sf1, grid = grid_df2,
                                     id_col = "id",
                                     aggregate = FALSE) %>%
                     dplyr::pull(id))
  })
  expect_contains(potential_gridcells_sf2,
                  grid_designation(observations_sf2, grid = grid_df2,
                                   id_col = "id",
                                   aggregate = FALSE) %>%
                    dplyr::pull(id))
  # aggregate = FALSE, randomisation = "normal"
  suppressWarnings({
    expect_contains(potential_gridcells_sf1,
                    grid_designation(observations_sf1, grid = grid_df2,
                                     id_col = "id",
                                     randomisation = "normal",
                                     aggregate = FALSE) %>%
                      dplyr::pull(id))
  })
  expect_contains(potential_gridcells_sf2,
                  grid_designation(observations_sf2, grid = grid_df2,
                                   id_col = "id",
                                   randomisation = "normal",
                                   aggregate = FALSE) %>%
                    dplyr::pull(id))
})

test_that("number of observations should equal numbers in grid", {
  # randomisation = "uniform"
  suppressWarnings({
    expect_equal(grid_designation(observations_sf1, grid = grid_df1) %>%
                   dplyr::pull(n) %>%
                   sum(),
                 nrow(observations_sf1))
  })
  expect_equal(grid_designation(observations_sf2, grid = grid_df1) %>%
                 dplyr::pull(n) %>%
                 sum(),
               nrow(observations_sf2))
  # randomisation = "normal"
  suppressWarnings({
    expect_equal(grid_designation(observations_sf1, grid = grid_df1,
                                  randomisation = "normal") %>%
                   dplyr::pull(n) %>%
                   sum(),
                 nrow(observations_sf1))
  })
  expect_equal(grid_designation(observations_sf2, grid = grid_df1,
                                randomisation = "normal") %>%
                 dplyr::pull(n) %>%
                 sum(),
               nrow(observations_sf2))
})

test_that("number of observations be the same as output if aggregate = FALSE", {
  # randomisation = "uniform"
  suppressWarnings({
    expect_equal(grid_designation(observations_sf1, grid = grid_df1,
                                  aggregate = FALSE) %>%
                   nrow(),
                 nrow(observations_sf1))
  })
  expect_equal(grid_designation(observations_sf2, grid = grid_df1,
                                aggregate = FALSE) %>%
                 nrow(),
               nrow(observations_sf2))
  # randomisation = "normal"
  suppressWarnings({
    expect_equal(grid_designation(observations_sf1, grid = grid_df1,
                                  randomisation = "normal",
                                  aggregate = FALSE) %>%
                   nrow(),
                 nrow(observations_sf1))
  })
  expect_equal(grid_designation(observations_sf2, grid = grid_df1,
                                randomisation = "normal",
                                aggregate = FALSE) %>%
                 nrow(),
               nrow(observations_sf2))
})
