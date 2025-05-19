combine_lsoa_emissions = function(flights_lsoa_emissions,
                                  consumption_emissions,
                                  car_emissions,
                                  domestic_electricity_emissions,
                                  domestic_gas_emissions,
                                  other_heating_emissions,
                                  max_year = 2020
                                  ) {

  lsoa = dplyr::left_join(domestic_gas_emissions, domestic_electricity_emissions, by = "LSOA21CD")

  car_emissions = tidyr::pivot_wider(car_emissions,
                                     id_cols = "LSOA21CD",
                                     values_from = c("car_emissions_percap","van_emissions_percap","company_bike_emissions_percap"),
                                     names_from = "year"
                                    )


  other_heating_emissions = other_heating_emissions[other_heating_emissions$year <= max_year,]

  other_heating_emissions = tidyr::pivot_wider(other_heating_emissions,
                                               id_cols = "LSOA21CD",
                                               values_from = "heating_other_kgco2e_percap",
                                               names_from = "year")
  names(other_heating_emissions)[2:ncol(other_heating_emissions)] = paste0("heating_other_kgco2e_percap_",names(other_heating_emissions)[2:ncol(other_heating_emissions)])


  consumption_emissions = consumption_emissions[,c("LSOA21CD","year",
                                                   "emissions_percap_food","emissions_percap_alcohol","emissions_percap_clothing",
                                                   "emissions_percap_communication","emissions_percap_housing_other",
                                                   "emissions_percap_furnish","emissions_percap_recreation",
                                                   "emissions_percap_transport_vehiclepurchase","emissions_percap_health",
                                                   "emissions_percap_education","emissions_percap_restaurant","emissions_percap_misc"
                                                   )]

  names(consumption_emissions) = gsub("emissions_percap_","",names(consumption_emissions))
  names(consumption_emissions)[3:ncol(consumption_emissions)] = paste0(names(consumption_emissions)[3:ncol(consumption_emissions)],"_kgco2e_percap")

  consumption_emissions = tidyr::pivot_wider(consumption_emissions, id_cols = "LSOA21CD",
                                             names_from = "year",
                                             values_from = c("food_kgco2e_percap",
                                                             "alcohol_kgco2e_percap",
                                                             "clothing_kgco2e_percap",
                                                             "communication_kgco2e_percap",
                                                             "housing_other_kgco2e_percap",
                                                             "furnish_kgco2e_percap",
                                                             "recreation_kgco2e_percap",
                                                             "transport_vehiclepurchase_kgco2e_percap",
                                                             "health_kgco2e_percap",
                                                             "education_kgco2e_percap",
                                                             "restaurant_kgco2e_percap",
                                                             "misc_kgco2e_percap"))


  flights_lsoa_emissions = flights_lsoa_emissions[,c("LSOA21CD","year","emissions_percap")]
  names(flights_lsoa_emissions)[3] = "flights_kgco2e_percap"

  #TODO
  flights_lsoa_emissions = tidyr::pivot_wider(flights_lsoa_emissions, id_cols = "LSOA21CD",
                                             names_from = "year",
                                             values_from = "flights_kgco2e_percap",
                                             names_prefix = "flights_kgco2e_percap_")


  lsoa = dplyr::left_join(lsoa, car_emissions, by = "LSOA21CD")
  lsoa = dplyr::left_join(lsoa, other_heating_emissions, by = "LSOA21CD")
  lsoa = dplyr::left_join(lsoa, consumption_emissions, by = "LSOA21CD")
  #flights_lsoa_emissions = flights_lsoa_emissions[,c("LSOA21CD", paste0("emissions_flights_percap_",2010:2021))]
  lsoa = dplyr::left_join(lsoa, flights_lsoa_emissions, by = "LSOA21CD")

  for(i in 2010:2020){
    lsoa[,paste0("goods_services_combined_kgco2e_percap_",i)] = lsoa[,paste0("food_kgco2e_percap_",i)] +
      lsoa[,paste0("alcohol_kgco2e_percap_",i)] +
      lsoa[,paste0("clothing_kgco2e_percap_",i)] +
      lsoa[,paste0("communication_kgco2e_percap_",i)] +
      lsoa[,paste0("housing_other_kgco2e_percap_",i)] +
      lsoa[,paste0("furnish_kgco2e_percap_",i)] +
      lsoa[,paste0("recreation_kgco2e_percap_",i)] +
      lsoa[,paste0("health_kgco2e_percap_",i)] +
      lsoa[,paste0("education_kgco2e_percap_",i)] +
      lsoa[,paste0("restaurant_kgco2e_percap_",i)] +
      lsoa[,paste0("misc_kgco2e_percap_",i)]



    lsoa[,paste0("total_kgco2e_percap_",i)] = lsoa[,paste0("dom_gas_kgco2e_percap_",i)] +
                                              lsoa[,paste0("dom_elec_kgco2e_percap_",i)] +
                                              lsoa[,paste0("car_emissions_percap_",i)] +
                                              lsoa[,paste0("van_emissions_percap_",i)] +
                                              lsoa[,paste0("flights_kgco2e_percap_",i)] +
                                              lsoa[,paste0("heating_other_kgco2e_percap_",i)] +
                                              lsoa[,paste0("transport_vehiclepurchase_kgco2e_percap_",i)] +
                                              lsoa[,paste0("goods_services_combined_kgco2e_percap_",i)]

  }

  # Add Grades
  for(i in 2010:2020){
    lsoa[,paste0("total_grade_",i)] = value2grade(lsoa[,paste0("total_kgco2e_percap_",i)])
    lsoa[,paste0("dom_gas_grade_",i)] = value2grade(lsoa[,paste0("dom_gas_kgco2e_percap_",i)])
    lsoa[,paste0("dom_elec_grade_",i)] = value2grade(lsoa[,paste0("dom_elec_kgco2e_percap_",i)])
    lsoa[,paste0("car_grade_",i)] = value2grade(lsoa[,paste0("car_emissions_percap_",i)])
    lsoa[,paste0("van_grade_",i)] = value2grade(lsoa[,paste0("van_emissions_percap_",i)])
    lsoa[,paste0("goods_services_combined_grade_",i)] = value2grade(lsoa[,paste0("goods_services_combined_kgco2e_percap_",i)])
    lsoa[,paste0("flights_grade_",i)] = value2grade(lsoa[,paste0("flights_kgco2e_percap_",i)])



  }



  lsoa
}
