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

Number_species <- 10
for (i in 1:Number_species){
  seed(i)
  first_Virtual_species <-  generateRandomSp (worldclim,
                                               convert.to.PA = FALSE,
                                               # How to combine response functions
                                               species.type = "multiplicative",
                                                # Random approach between PCA and response function
                                                 approach = "random",
                                               # Response function
                                               relations = "gaussian",
                                               # Realistic species
                                               realistic.sp = TRUE,
                                               plot = FALSE)

new.pres <-convertToPA(first_Virtual_species,
                       beta = "random",
                       alpha = -0.05, plot = FALSE,
                       species.prevalence = 0.1)

presence.points <- sampleOccurrences(new.pres,
                                     n = 100,
                                     type = "presence-absence",
                                     sample.prevalence = 0.9,
                                     error.probability = 0,
                                     detection.probability = 1,
                                     correct.by.suitability = TRUE,
                                     plot = FALSE,
                                     sampling.area="Italy")
}

