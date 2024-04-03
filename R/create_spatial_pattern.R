

### idea: create a raster (terra) with a spatial pattern from which a sampling can be made.
## user need to provide a polygon (sf)

## the spatial pattern could be defined by string (3 options) or a value from 1 to 100.
## the value is a multiplier of the resolution size.by defalt 1 is random, 10 is clustered and 100 is regular.


create_spatial_pattern <- function(
  polygon,
  resolution,
  spatial_pattern = c("random", "clustered")
  seed = NA
  )
