# Loading required packages
library(sf)
library(raster)
library(viridis)
library(virtualspecies)
library(tidyverse)
library(geodata)

# worldclim_global(var, res, path, version="2.1", ...)
# worldclim_country(country, var, path, version="2.1", ...)
# worldclim_tile(var, lon, lat, path, version="2.1", ...)

# Download all bioclimatic variables
d <- worldclim_country(country = "Italy", var = "bio", res=0.5, path=tempdir())
plot(d)
plot(d[[5]])

# Number of random vs
Number_species <- 3

# Empty list
Outputs <- list()


for (i in 1:Number_species) {
  set.seed(i)
  random.sp <- generateRandomSp(raster.stack = d,
                                convert.to.PA = FALSE,
                                species.type = "multiplicative",
                                relations = "gaussian",
                                realistic.sp = TRUE,
                                plot = FALSE)


  new.pres <-convertToPA(random.sp,
                         beta = "random",
                         alpha = -0.05, plot = FALSE,
                         species.prevalence = 0.02)

  presence.points <- sampleOccurrences(new.pres,
                                       n = 40,
                                       type = "presence-absence",
                                       sample.prevalence = 0.5,
                                       detection.probability = 0.8,
                                       correct.by.suitability = TRUE,
                                       plot = TRUE)
  print(i)
  Outputs_i = data.frame(i, presence.points$sample.points)
  names(Outputs_i) <- c("species", "x", "y")
  Outputs[[i]] <- Outputs_i

}
print(Outputs)
Outputs[[1]]
