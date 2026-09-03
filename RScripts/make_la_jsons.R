# Make Search JSONs
#
# The website's report pages search these lists, so they must offer exactly the
# areas that have a report behind them, under the same name the report itself
# shows. Wards and parishes therefore come from `area_names`, which is built
# from the areas the population weighting reaches (see R/area_weights.R).
# Local authorities and constituencies come from their boundary layers, minus
# Northern Ireland: the boundary layers are UK-wide but every dataset behind
# the reports is GB-only, so an NI council or constituency in the search box
# only ever leads to an empty report.

library(targets)
library(sf)
library(jsonlite)

site = "../PlaceBasedCarbonCalculator.github.io/reports/"

tar_load(area_names)

write_json(area_names$ward, paste0(site, "wards.json"))
write_json(area_names$parish, paste0(site, "parish.json"))

as_search_list = function(x){
  x = st_drop_geometry(x)
  names(x) = c("id","name")
  # N-prefixed ONS codes are Northern Ireland
  x = x[!grepl("^N", x$id),]
  x[order(x$name),]
}

tar_load(bounds_la)
write_json(as_search_list(bounds_la), paste0(site, "la.json"))

tar_load(bounds_westminster)
write_json(as_search_list(bounds_westminster), paste0(site, "westminster.json"))
