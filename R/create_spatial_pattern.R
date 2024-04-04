

### idea: create a raster (terra) with a spatial pattern from which a sampling can be made.
## user need to provide a polygon (sf)

## the spatial pattern could be defined by string (3 options) or a value from 1 to 100.
## the value is a multiplier of the resolution size.by default 1 is random, 5 is clustered


create_spatial_pattern <- function(
  polygon,
  resolution,
  spatial_pattern = c("random", "clustered"),
  seed = NA,
  n_sim = 1
){

  if (length(seed) != 1) {
    cli::cli_abort(c(
      "{.var seed} must be a numeric vector of length 1.",
      "x" = paste(
        "You've supplied a {.cls {class(seed)}} vector",
        "of length {length(seed)}."
      )
    ))
  }

  # create a reference raster with same extent as the polygon and user defined resolution
  templ <- terra::rast(vect(polygon), res=resolution)
  poly_raster <- terra::rasterize(vect(plgn), templ)

  dfxy <- as.data.frame(poly_raster, xy = TRUE)

  # define the spatial pattern ----
  if(is.character(spatial_pattern)){

    if(spatial_pattern[1] == "random"){
      multiplier <- 1
    }
    if(spatial_pattern[1] == "clustered"){
      multiplier <- 10
    }

    if(!any(spatial_pattern[1] %in% c("random", "clustered"))){
      cli::cli_abort(
        c(paste(
          "When class of {.var spatial_pattern} is",
          "{.cls {class(spatial_pattern)}} you should provide 'random' or 'clustered' as string"),
          "x" = "You've provided the string '{spatial_pattern}' in {.var spatial_pattern}"
        )
      )
    }

    ## should have a stopper when is not one of the options

  }

  #
  if(is.numeric(spatial_pattern)){
    # value should be between 1 and 100
    if(between(spatial_pattern, 1, 100)){
      multiplier <- spatial_pattern
    }else{
      cli::cli_abort("")
    }
  }

  range_size <- resolution * multiplier

  ## generate the pattern

  # Set seed if provided
  if (!is.na(seed)) {
    if (is.numeric(seed)) {
      withr::local_seed(seed)
    } else {
      cli::cli_abort(c(
        "{.var seed} must be a numeric vector of length 1.",
        "x" = paste(
          "You've supplied a {.cls {class(seed)}} vector",
          "of length {length(seed)}."
        )
      ))
    }
  }

  # !idea: we can allow the user to provide a gstat object with other vgm model...
  gstat_model <- gstat::gstat(formula=z~1, locations=~x+y, dummy=T, beta=1,
                       model=vgm(psill=0.5, model="Sph", range=range_size,
                                 nugget = 0), nmax=2)

  dfxy_pred <- predict(gstat_model, newdata=dfxy, nsim=n_sim)


  # standardize values between 0 and 1
  dfxy_std <- dfxy_pred %>%
    mutate(
      across(starts_with("sim"), ~vegan::decostand(.x, "range"))
    )

  r <- rast(dfxy_std)

  r
}
