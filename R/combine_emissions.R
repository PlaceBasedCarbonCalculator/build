combine_lsoa_emissions = function(flights_lsoa_emissions,
                                  consumption_emissions,
                                  car_emissions,
                                  domestic_electricity_emissions,
                                  domestic_gas_emissions,
                                  other_heating_emissions,
                                  max_year = 2020
                                  ) {

  #TODO: some NA grades, check for consistency of 0 population zones.
  lsoa = dplyr::left_join(domestic_gas_emissions, domestic_electricity_emissions, by = c("LSOA21CD","year"))

  lsoa = lsoa[lsoa$year <= max_year,]

  # car_emissions = tidyr::pivot_wider(car_emissions,
  #                                    id_cols = "LSOA21CD",
  #                                    values_from = c("car_emissions_percap","van_emissions_percap","company_bike_emissions_percap"),
  #                                    names_from = "year"
  #                                   )

  names(car_emissions) = gsub("_emissions_percap","_kgco2e_percap",names(car_emissions))


  other_heating_emissions = other_heating_emissions[other_heating_emissions$year <= max_year,]

  # other_heating_emissions = tidyr::pivot_wider(other_heating_emissions,
  #                                              id_cols = "LSOA21CD",
  #                                              values_from = "heating_other_kgco2e_percap",
  #                                              names_from = "year")
  # names(other_heating_emissions)[2:ncol(other_heating_emissions)] = paste0("heating_other_kgco2e_percap_",names(other_heating_emissions)[2:ncol(other_heating_emissions)])


  consumption_emissions = consumption_emissions[,c("LSOA21CD","year",
                                                   "emissions_percap_food","emissions_percap_alcohol","emissions_percap_clothing",
                                                   "emissions_percap_communication","emissions_percap_housing_other",
                                                   "emissions_percap_furnish","emissions_percap_recreation",
                                                   "emissions_percap_transport_optranequip_other",
                                                   "emissions_percap_transport_vehiclepurchase","emissions_percap_transport_pt",
                                                   "emissions_percap_health",
                                                   "emissions_percap_education","emissions_percap_restaurant","emissions_percap_misc"
                                                   )]

  names(consumption_emissions) = gsub("emissions_percap_","",names(consumption_emissions))
  names(consumption_emissions)[3:ncol(consumption_emissions)] = paste0(names(consumption_emissions)[3:ncol(consumption_emissions)],"_kgco2e_percap")

  # consumption_emissions = tidyr::pivot_wider(consumption_emissions, id_cols = "LSOA21CD",
  #                                            names_from = "year",
  #                                            values_from = c("food_kgco2e_percap",
  #                                                            "alcohol_kgco2e_percap",
  #                                                            "clothing_kgco2e_percap",
  #                                                            "communication_kgco2e_percap",
  #                                                            "housing_other_kgco2e_percap",
  #                                                            "furnish_kgco2e_percap",
  #                                                            "recreation_kgco2e_percap",
  #                                                            "transport_vehiclepurchase_kgco2e_percap",
  #                                                            "health_kgco2e_percap",
  #                                                            "education_kgco2e_percap",
  #                                                            "restaurant_kgco2e_percap",
  #                                                            "misc_kgco2e_percap"))


  flights_lsoa_emissions = flights_lsoa_emissions[,c("LSOA21CD","year","emissions_percap")]
  names(flights_lsoa_emissions)[3] = "flights_kgco2e_percap"


  # flights_lsoa_emissions = tidyr::pivot_wider(flights_lsoa_emissions, id_cols = "LSOA21CD",
  #                                            names_from = "year",
  #                                            values_from = "flights_kgco2e_percap",
  #                                            names_prefix = "flights_kgco2e_percap_")


  lsoa = dplyr::left_join(lsoa, car_emissions, by = c("LSOA21CD","year"))
  lsoa = dplyr::left_join(lsoa, other_heating_emissions, by = c("LSOA21CD","year"))
  lsoa = dplyr::left_join(lsoa, consumption_emissions, by = c("LSOA21CD","year"))
  lsoa = dplyr::left_join(lsoa, flights_lsoa_emissions, by = c("LSOA21CD","year"))

  # Total Goods and Services
  lsoa$goods_services_combined_kgco2e_percap = rowSums(lsoa[,c("food_kgco2e_percap",
                                                               "alcohol_kgco2e_percap",
                                                               "clothing_kgco2e_percap",
                                                               "communication_kgco2e_percap",
                                                               "housing_other_kgco2e_percap",
                                                               "furnish_kgco2e_percap",
                                                               "recreation_kgco2e_percap",
                                                               "health_kgco2e_percap",
                                                               "education_kgco2e_percap",
                                                               "restaurant_kgco2e_percap",
                                                               "misc_kgco2e_percap")], na.rm = TRUE)


  lsoa$total_kgco2e_percap = rowSums(lsoa[,c("dom_gas_kgco2e_percap",
                                                               "dom_elec_kgco2e_percap",
                                                               "car_kgco2e_percap",
                                                               "van_kgco2e_percap",
                                                               "flights_kgco2e_percap",
                                                               "heating_other_kgco2e_percap",
                                                               "transport_vehiclepurchase_kgco2e_percap",
                                                               "transport_pt_kgco2e_percap",
                                                               "transport_optranequip_other_kgco2e_percap",
                                                               "goods_services_combined_kgco2e_percap")], na.rm = TRUE)


  lsoa = lsoa |>
    dplyr::group_by(year) |>
    dplyr::mutate(

      dom_gas_grade = value2grade(dom_gas_kgco2e_percap),
      dom_elec_grade = value2grade(dom_elec_kgco2e_percap),
      heating_other_grade = value2grade(heating_other_kgco2e_percap),
      car_grade = value2grade(car_kgco2e_percap),
      van_grade = value2grade(van_kgco2e_percap),
      company_bike_grade = value2grade(company_bike_kgco2e_percap),
      transport_optranequip_other_grade = value2grade(transport_optranequip_other_kgco2e_percap),
      transport_vehiclepurchase_grade = value2grade(transport_vehiclepurchase_kgco2e_percap),
      transport_pt_grade = value2grade(transport_pt_kgco2e_percap),
      flights_grade = value2grade(flights_kgco2e_percap),
      food_grade = value2grade(food_kgco2e_percap),
      alcohol_grade = value2grade(alcohol_kgco2e_percap),
      clothing_grade = value2grade(clothing_kgco2e_percap),
      communication_grade = value2grade(communication_kgco2e_percap),
      housing_other_grade = value2grade(housing_other_kgco2e_percap),
      furnish_grade = value2grade(furnish_kgco2e_percap),
      recreation_grade = value2grade(recreation_kgco2e_percap),
      health_grade = value2grade(health_kgco2e_percap),
      education_grade = value2grade(education_kgco2e_percap),
      restaurant_grade = value2grade(restaurant_kgco2e_percap),
      misc_grade = value2grade(misc_kgco2e_percap),
      total_grade = value2grade(total_kgco2e_percap),
      goods_services_combined_grade = value2grade(goods_services_combined_kgco2e_percap),

    ) |>
    dplyr::ungroup()


  lsoa
}
