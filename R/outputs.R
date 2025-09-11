select_map_outputs = function(lsoa_emissions_all, year = 2020) {
  lsoa = lsoa_emissions_all[lsoa_emissions_all$year == year,]
  lsoa = lsoa[,c("LSOA21CD","dom_gas_grade","dom_elec_grade","car_grade",
                 "van_grade","flights_grade","total_grade","goods_services_combined_grade")]
  lsoa
}


join_for_geojson = function(lsoa_map_data, bounds) {
  bounds = dplyr::left_join(bounds, lsoa_map_data, by = "LSOA21CD")
  bounds
}
