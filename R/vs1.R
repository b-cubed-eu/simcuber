# vs
# Loading required packages
library(sf)
library(ClimDatDownloadR)
library(raster)
library(virtualspecies)
library(tidyverse)


#Dowload bioclimatic variables from Worldclim

worldclim <- getData("worldclim", var = "bio", res = 0.5, lon=5, lat=45)
worldclim

names(worldclim)
plot(worldclim)


PA.points <- sampleOccurrences(my.first.species, n =
                                 30
                               , type =
                                 "presence-absence"
                               , sampling.area = c(
                                 "South America"
                                 ,
                                 "Mexico"
                               ))
