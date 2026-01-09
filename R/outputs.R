select_map_outputs = function(lsoa_emissions_all, area_classifications_11_21, year = 2020) {
  lsoa_savings = lsoa_emissions_all[lsoa_emissions_all$year %in% c(2010,year),]
  lsoa_savings = lsoa_savings[,c("LSOA21CD","total_kgco2e_percap","year")]
  lsoa_savings = lsoa_savings |>
    tidyr::pivot_wider(values_from = total_kgco2e_percap, names_from = year)
  lsoa_savings$preduction = (lsoa_savings[["2010"]] - lsoa_savings[[as.character(year)]]) / lsoa_savings[["2010"]] * 100
  lsoa_savings$preduction[is.infinite(lsoa_savings$preduction)] = NA
  lsoa_savings$reduction_grade = value2grade(lsoa_savings$preduction, high_good = TRUE, zeroNA = FALSE)

  lsoa = lsoa_emissions_all[lsoa_emissions_all$year == year,]
  lsoa = lsoa[,c("LSOA21CD","dom_gas_grade","dom_elec_grade","car_grade",
                 "van_grade","flights_grade","total_grade","goods_services_combined_grade")]

  lsoa = dplyr::left_join(lsoa, lsoa_savings[,c("LSOA21CD","reduction_grade")], by = "LSOA21CD")

  lsoa = dplyr::left_join(lsoa, area_classifications_11_21[,c("LSOA21CD","lsoa_class_code")], by = "LSOA21CD")

  lsoa
}


join_for_geojson = function(lsoa_map_data, bounds) {
  bounds = dplyr::left_join(bounds, lsoa_map_data, by = "LSOA21CD")
  bounds
}
