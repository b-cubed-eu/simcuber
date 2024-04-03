# Loading required packages
library(sf)
library(raster)
library(viridis)
library(virtualspecies)
library(tidyverse)


library(geodata)

# worldclim_global(var, res, path, version="2.1", ...)
worldclim_country(country, var, path, version="2.1", ...)
# worldclim_tile(var, lon, lat, path, version="2.1", ...)
d <- worldclim_country(country = "Jamaica", var = "tmin", res=0.5, path=tempdir())
?worldclim_country
### Generation of Virtual Species ###
# The input data for virtual species is gridded spatial data
worldclim <- getData("worldclim", var = "bio", res=0.5, lon=20, lat=0)
?getData
plot(d[[5]])

Number_species <- 3
for (i in 1:Number_species){
  set.seed(i)
  first_Virtual_species <-  generateRandomSp(d,
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
                                     plot = FALSE)
                                    # sampling.area="Italy")
}





# Lista vuota di output
Outputs <- list()

## Ciclo for: la lunghezza della lista (10) equivale a ciascuna specie
for (i in 1:Number_species) {
  set.seed(i)
  # climatic_variables <- myRandNum[[i]]

  # Selezioniamo tre variabili climatiche da mydata utilizzando gli indici generati casualmente
  # selected_rasters <- mydata[[climatic_variables]]


  # Creiamo uno stack di raster dalle variabili climatiche selezionate
  # raster_stack <- stack(selected_rasters)
  # print(names(selected_rasters))
  random.sp <- generateRandomSp(raster.stack = d,
                                convert.to.PA = FALSE,

                                # Come si combinano le funzioni di risposta per ottenere la suitability
                                species.type = "multiplicative",

                                # Funzione di risposta agli input ambientali
                                relations = "gaussian",
                                realistic.sp = TRUE,
                                plot = FALSE)

  # Presenza/Assenza: richiede di definire i parametri alfa, beta e la prevalenza di specie
  new.pres <-convertToPA(random.sp,
                         beta = "random",
                         alpha = -0.05, plot = FALSE,
                         species.prevalence = 0.02)
  beta_value <- new.pres$PA.conversion[4]



  presence.points <- sampleOccurrences(new.pres,
                                       n = 40,
                                       type = "presence-absence",
                                       sample.prevalence = 0.5,

                                       # Si potrebbe mettere uguale a beta
                                       detection.probability = 0.8,
                                       correct.by.suitability = TRUE,
                                       plot = TRUE)
  print(i)
  Outputs_i = data.frame(i, presence.points$sample.points)
  names(Outputs_i) <- c("species", "x", "y")
  Outputs[[i]] <- Outputs_i
  #  Outputs[[i]] <- presence.points

}
print(Outputs)
Outputs[[1]]
