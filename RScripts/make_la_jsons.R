# Make Search JSONs

library(targets)
library(sf)
library(jsonlite)

tar_load(bounds_la)

bounds_la  = st_drop_geometry(bounds_la)
names(bounds_la) = c("id","name")
bounds_la = bounds_la[order(bounds_la$name),]
write_json(bounds_la, "../PlaceBasedCarbonCalculator.github.io/reports/la.json")

tar_load(bounds_wards)
bounds_wards  = st_drop_geometry(bounds_wards)
names(bounds_wards) = c("id","name")
bounds_wards = bounds_wards[order(bounds_wards$name),]
write_json(bounds_wards, "../PlaceBasedCarbonCalculator.github.io/reports/wards.json")

tar_load(bounds_parish)
bounds_parish  = st_drop_geometry(bounds_parish)
names(bounds_parish) = c("id","name")
bounds_parish = bounds_parish[order(bounds_parish$name),]
write_json(bounds_parish, "../PlaceBasedCarbonCalculator.github.io/reports/parish.json")

tar_load(bounds_westminster)
bounds_westminster  = st_drop_geometry(bounds_westminster)
names(bounds_westminster) = c("id","name")
bounds_westminster = bounds_westminster[order(bounds_westminster$name),]
write_json(bounds_westminster, "../PlaceBasedCarbonCalculator.github.io/reports/westminster.json")


