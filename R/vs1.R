# Loading required packages
library(sf)
library(raster)
library(viridis)
library(virtualspecies)
library(tidyverse)
library(geodata)
library(corrplot)
library(car)
install.packages("usethis")
# worldclim_global(var, res, path, version="2.1", ...)
# worldclim_country(country, var, path, version="2.1", ...)
# worldclim_tile(var, lon, lat, path, version="2.1", ...)

# Download all bioclimatic variables
d <- worldclim_country(country = "Italy", var = "bio", res=0.5, path=tempdir()) %>% stack()

#BIO1 = Annual Mean Temperature

#BIO2 = Mean Diurnal Range (Mean of monthly (max temp - min temp))

#BIO3 = Isothermality (BIO2/BIO7) (×100)

#BIO4 = Temperature Seasonality (standard deviation ×100)

#BIO5 = Max Temperature of Warmest Month

#BIO6 = Min Temperature of Coldest Month

#BIO7 = Temperature Annual Range (BIO5-BIO6)

#BIO8 = Mean Temperature of Wettest Quarter

#BIO9 = Mean Temperature of Driest Quarter

#BIO10 = Mean Temperature of Warmest Quarter

#BIO11 = Mean Temperature of Coldest Quarter

#BIO12 = Annual Precipitation

#BIO13 = Precipitation of Wettest Month

#BIO14 = Precipitation of Driest Month

#BIO15 = Precipitation Seasonality (Coefficient of Variation)

#BIO16 = Precipitation of Wettest Quarter

#BIO17 = Precipitation of Driest Quarter

#BIO18 = Precipitation of Warmest Quarter

#BIO19 = Precipitation of Coldest Quarter

# Subsample 10% of pixels and calculate pairwise correlations

cor <- cor(sampleRandom(mydata, size= ncell(r1) * 0.10 ), method = "pearson")
d
pp <- stack(d)


data_df <- as.data.frame(d, xy = TRUE)
# Supponendo che 'data' sia il tuo dataframe contenente le variabili bioclimatiche
vif_results <- vif(lm.fit = lm())


# Plot correlation matrix
df <- corrplot(cor, method = "number")

# Creazione di un vettore di nomi delle variabili
names_var <- c()

# Numero di variabili da rinominare
num_var <- 19

# Generazione dei nomi delle variabili
for (i in 1:num_var) {
  new_name <- paste("bio", i, sep = "")
  names_var <- c(names_var, new_name)
}

# Visualizzazione dei nuovi nomi delle variabili
print(names_var)
names(d) <- names_var


#Numero variabili bioclimatiche

num_bioclim <- 5

indici <- sample(1:nlayers(d), 3)

selezionati <- d[[indici]]

print(names(selezionati))

# Plot correlation matrix
df <- corrplot(cor, method = "number")

# Number of random vs
Number_species <- 3

# Empty list
Outputs <- list()


for (i in 1:Number_species) {
  set.seed(i)
  random.sp <- generateRandomSp(raster.stack = biovar,
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


usethis::use_vignette("my-vignette")

