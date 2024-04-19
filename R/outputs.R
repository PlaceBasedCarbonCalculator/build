select_map_outputs = function(lsoa_emissions_all, year = 2020) {
  lsoa = lsoa_emissions_all[,c("LSOA21CD",names(lsoa_emissions_all)[grepl(paste0("_",year),names(lsoa_emissions_all))])]
  lsoa = lsoa[,c("LSOA21CD",names(lsoa)[grepl("grade",names(lsoa))])]
  lsoa
}


join_for_geojson = function(lsoa_map_data, bounds) {
  bounds = dplyr::left_join(bounds, lsoa_map_data, by = "LSOA21CD")
  bounds
}
