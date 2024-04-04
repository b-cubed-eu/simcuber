test_that("add_coordinate_uncertainty() returns error on non sf occurrence input", {
  not_an_sf_object <- data.frame(1:5,5,6)
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
