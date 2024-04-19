combine_lsoa_emissions = function(flights_lsoa_emissions,
                                  consumption_emissions,
                                  car_emissions,
                                  domestic_electricity_emissions,
                                  domestic_gas_emissions,
                                  other_heating_emissions) {

  lsoa = dplyr::left_join(domestic_gas_emissions, domestic_electricity_emissions, by = "LSOA21CD")
  lsoa = dplyr::left_join(lsoa, car_emissions, by = "LSOA21CD")

  other_heating_emissions = tidyr::pivot_wider(other_heating_emissions,
                                               id_cols = "LSOA21CD",
                                               values_from = "heating_other_kgco2e_percap",
                                               names_from = "year")
  names(other_heating_emissions)[2:ncol(other_heating_emissions)] = paste0("heating_other_kgco2e_percap_",names(other_heating_emissions)[2:ncol(other_heating_emissions)])

  consumption_emissions = tidyr::pivot_wider(consumption_emissions, id_cols = "LSOA21CD",
                                             names_from = "year",
                                             values_from = c("consumables_emissions_percap",
                                                             "gas_electric_emissions_percap",
                                                             "nutrition_emissions_percap",
                                                             "other_shelter_emissions_percap",
                                                             "recreation_emissions_percap",
                                                             "services_emissions_percap"))

  lsoa = dplyr::left_join(lsoa, other_heating_emissions, by = "LSOA21CD")
  lsoa = dplyr::left_join(lsoa, consumption_emissions, by = "LSOA21CD")
  flights_lsoa_emissions = flights_lsoa_emissions[,c("LSOA21CD", paste0("emissions_flights_percap_",2010:2021))]
  lsoa = dplyr::left_join(lsoa, flights_lsoa_emissions, by = "LSOA21CD")

  for(i in 2010:2021){
    lsoa[,paste0("goods_services_combined_kgco2e_percap_",i)] = lsoa[,paste0("consumables_emissions_percap_",i)] +
      lsoa[,paste0("nutrition_emissions_percap_",i)] +
      lsoa[,paste0("other_shelter_emissions_percap_",i)] +
      lsoa[,paste0("recreation_emissions_percap_",i)] +
      lsoa[,paste0("services_emissions_percap_",i)]


    lsoa[,paste0("total_kgco2e_percap_",i)] = lsoa[,paste0("dom_gas_kgco2e_percap_",i)] +
                                              lsoa[,paste0("dom_elec_kgco2e_percap_",i)] +
                                              lsoa[,paste0("car_emissions_percap_",i)] +
                                              lsoa[,paste0("van_emissions_percap_",i)] +
                                              lsoa[,paste0("goods_services_combined_kgco2e_percap_",i)] +
                                              lsoa[,paste0("emissions_flights_percap_",i)]
  }

  # Add Grades
  for(i in 2010:2020){
    lsoa[,paste0("total_grade_",i)] = value2grade(lsoa[,paste0("total_kgco2e_percap_",i)])
    lsoa[,paste0("dom_gas_grade_",i)] = value2grade(lsoa[,paste0("dom_gas_kgco2e_percap_",i)])
    lsoa[,paste0("dom_elec_grade_",i)] = value2grade(lsoa[,paste0("dom_elec_kgco2e_percap_",i)])
    lsoa[,paste0("car_grade_",i)] = value2grade(lsoa[,paste0("car_emissions_percap_",i)])
    lsoa[,paste0("van_grade_",i)] = value2grade(lsoa[,paste0("van_emissions_percap_",i)])
    lsoa[,paste0("goods_services_combined_grade_",i)] = value2grade(lsoa[,paste0("goods_services_combined_kgco2e_percap_",i)])
    lsoa[,paste0("flights_grade_",i)] = value2grade(lsoa[,paste0("emissions_flights_percap_",i)])



  }



  lsoa
}
