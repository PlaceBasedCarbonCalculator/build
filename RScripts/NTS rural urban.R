library(targets)
library(sf)
library(tmap)

ru = read.csv("F:/GitHub/PlaceBasedCarbonCalculator/inputdata/rural_urban/Rural_Urban_Classification_(2011)_of_Lower_Layer_Super_Output_Areas_in_England_and_Wales.csv")
tar_load(lookup_OA_LSOA_MSOA_classifications)


oac = lookup_OA_LSOA_MSOA_classifications[,c("LSOA11CD", "SOAC11NM")]
oac = oac[!duplicated(oac$LSOA11CD),]
ru = ru[,c("LSOA11CD","RUC11CD","RUC11")]

bounds <- read_sf("../../mem48/UNCCUT/data/LSOA_bounds_simplified.gpkg")

oac = dplyr::left_join(oac,ru, by = "LSOA11CD" )
oac = oac[!is.na(oac$RUC11),]

oac$combined = paste0(oac$SOAC11NM," ",oac$RUC11)

bounds <- bounds[bounds$geo_code %in% oac$LSOA11CD,]
bounds <- dplyr::left_join(bounds, oac, by = c("geo_code" = "LSOA11CD"))

tmap_options(max.categories = 136)

library(randomcoloR)
cols = as.data.frame(table(oac$combined))



bounds$colour

tmap_mode("plot")
tm_shape(bounds) +
  tm_fill("combined")
