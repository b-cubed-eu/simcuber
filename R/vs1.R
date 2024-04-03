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


