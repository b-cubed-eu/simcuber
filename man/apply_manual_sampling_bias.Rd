% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/apply_manual_sampling_bias.R
\name{apply_manual_sampling_bias}
\alias{apply_manual_sampling_bias}
\title{Generate a sampling bias via a grid}
\usage{
apply_manual_sampling_bias(occurrences_sf, bias_weights)
}
\arguments{
\item{occurrences_sf}{An sf object with POINT geometry.}

\item{bias_weights}{A raster layer (sf object with POLYGON geometry). The
raster of bias weights to be applied to the sampling of occurrences. This sf
object should contain a \code{bias_weight} and \code{geometry} column. Higher weights
indicate a higher probability of sampling. Weights must be numeric values
between 0 and 1 OR positive integers that will be rescaled to values between
0 and 1.}
}
\value{
An sf object with POINT geometry with a bias_weight column
containing the sampling probability based on sampling bias.
}
\description{
The function adds a sampling bias weight column containing the sample
probability based on bias weights within each cell of a given grid layer.
}
\examples{
# Load packages
library(sf)
library(dplyr)
library(ggplot2)

# Set seed for reproducibility
set.seed(123)

# Simulate some occurrence data with coordinates and time points
num_points <- 10
occurrences <- data.frame(
  lon = runif(num_points, min = -180, max = 180),
  lat = runif(num_points, min = -90, max = 90),
  time_point = 0
)

# Convert the occurrence data to an sf object
occurrences_sf <- st_as_sf(occurrences, coords = c("lon", "lat"))

# Create raster grid
grid <- st_make_grid(occurrences_sf) \%>\%
  st_sf()

# Bias weights between 0 and 1
grid1 <- grid \%>\%
  mutate(bias_weight = runif(nrow(grid), min = 0, max = 1))

apply_manual_sampling_bias(occurrences_sf, grid1)

# Bias weights larger than 1
grid2 <- grid \%>\%
  mutate(bias_weight = rpois(nrow(grid), 5))

occurrence_bias_sf <- apply_manual_sampling_bias(occurrences_sf, grid2)
occurrence_bias_sf

# Visualise where the bias is
ggplot() +
 geom_sf(data = grid2) +
 geom_sf_text(data = grid2, aes(label = bias_weight)) +
 geom_sf(data = occurrence_bias_sf, aes(colour = bias_weight)) +
 scale_color_gradient(trans = "reverse")
}
\seealso{
Other detection: 
\code{\link{apply_polygon_sampling_bias}()}
}
\concept{detection}
