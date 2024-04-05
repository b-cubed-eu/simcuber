test_that("sample_occurrences() returns valid sf POINT geometry", {
  # build timeseries and raster to test on
  test_raster <-
    terra::rast(
      ncol = 50,
      nrow = 50,
      xmin = 0,
      xmax = 50,
      ymin = 0,
      ymax = 50
    )
  terra::values(test_raster) <- 1:terra::ncell(test_raster)
  timeseries <- c(20, 40, 60)

  expect_s3_class(
    sample_occurrences(rs = test_raster, ts = timeseries),
    "data.frame"
  )

  expect_s3_class(
    sample_occurrences(rs = test_raster, ts = timeseries),
    "sf"
  )
})

test_that("sample_occurrences() fails on invalid input argument", {
  expect_error(
    sample_occurrences(data.frame(5:9))
  )
})
