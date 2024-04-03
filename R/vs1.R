# Loading required packages
library(sf)
library(raster)
library(viridis)
library(virtualspecies)
library(tidyverse)


library(geodata)

worldclim_global(var, res, path, version="2.1", ...)
worldclim_country(country, var, path, version="2.1", ...)
worldclim_tile(var, lon, lat, path, version="2.1", ...)
d <- worldclim_country(country = "Jamaica", var = "tmin", res=0.5, path=tempdir())
?worldclim_country
### Generation of Virtual Species ###
# The input data for virtual species is gridded spatial data
worldclim <- getData("worldclim", var = "bio", res=0.5, lon=20, lat=0)
?getData
plot(d[[5]])


d
