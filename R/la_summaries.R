make_la_summary = function(lsoa_emissions_all, lsoa_admin, population){

  lsoa_admin = lsoa_admin[,c("LSOA21CD","LAD25CD")]

  population = population[,c("LSOA21CD","year","all_ages")]

  lsoa_emissions_all = lsoa_emissions_all[,!grepl("grade",names(lsoa_emissions_all))]
  lsoa_emissions_all = lsoa_emissions_all[,!grepl("kgco2e_percap",names(lsoa_emissions_all))]

  lsoa_emissions_all = dplyr::left_join(lsoa_emissions_all, lsoa_admin, by = "LSOA21CD")
  lsoa_emissions_all = dplyr::left_join(lsoa_emissions_all, population, by = c("LSOA21CD","year"))

  la_emissions = lsoa_emissions_all |>
    dplyr::group_by(LAD25CD, year) |>
    dplyr::summarise(
      dom_gas_kgco2e_percap = sum(dom_gas_total_emissions) / sum(all_ages),
      dom_elec_kgco2e_percap = sum(dom_elec_total_emissions) / sum(all_ages),
      car_kgco2e_percap = sum(car_emissions) / sum(all_ages),
      van_kgco2e_percap = sum(van_emissions) / sum(all_ages),
      company_bike_kgco2e_percap = sum(company_bike_emissions) / sum(all_ages),
      heating_other_kgco2e_percap = sum(heating_other_emissions_total) / sum(all_ages),
      food_kgco2e_percap = sum(emissions_food) / sum(all_ages),
      alcohol_kgco2e_percap = sum(emissions_alcohol) / sum(all_ages),
      clothing_kgco2e_percap = sum(emissions_clothing) / sum(all_ages),
      communication_kgco2e_percap = sum(emissions_communication) / sum(all_ages),
      housing_other_kgco2e_percap = sum(emissions_housing_other) / sum(all_ages),
      furnish_kgco2e_percap = sum(emissions_furnish) / sum(all_ages),
      recreation_kgco2e_percap = sum(emissions_recreation) / sum(all_ages),
      transport_optranequip_other_kgco2e_percap = sum(emissions_transport_optranequip_other) / sum(all_ages),
      transport_vehiclepurchase_kgco2e_percap = sum(emissions_transport_vehiclepurchase) / sum(all_ages),
      transport_pt_kgco2e_percap = sum(emissions_transport_pt) / sum(all_ages),
      health_kgco2e_percap = sum(emissions_health) / sum(all_ages),
      education_kgco2e_percap = sum(emissions_education) / sum(all_ages),
      restaurant_kgco2e_percap = sum(emissions_restaurant) / sum(all_ages),
      misc_kgco2e_percap = sum(emissions_misc) / sum(all_ages),
      flights_kgco2e_percap = sum(flights_emissions_total) / sum(all_ages),
      goods_services_combined_kgco2e_percap = sum(goods_services_combined_total) / sum(all_ages),
      total_kgco2e_percap = sum(emissions_total) / sum(all_ages)

    )

  national_emissions = lsoa_emissions_all |>
    dplyr::group_by(year) |>
    dplyr::summarise(
      dom_gas_kgco2e_percap = sum(dom_gas_total_emissions) / sum(all_ages),
      dom_elec_kgco2e_percap = sum(dom_elec_total_emissions) / sum(all_ages),
      car_kgco2e_percap = sum(car_emissions) / sum(all_ages),
      van_kgco2e_percap = sum(van_emissions) / sum(all_ages),
      company_bike_kgco2e_percap = sum(company_bike_emissions) / sum(all_ages),
      heating_other_kgco2e_percap = sum(heating_other_emissions_total) / sum(all_ages),
      food_kgco2e_percap = sum(emissions_food) / sum(all_ages),
      alcohol_kgco2e_percap = sum(emissions_alcohol) / sum(all_ages),
      clothing_kgco2e_percap = sum(emissions_clothing) / sum(all_ages),
      communication_kgco2e_percap = sum(emissions_communication) / sum(all_ages),
      housing_other_kgco2e_percap = sum(emissions_housing_other) / sum(all_ages),
      furnish_kgco2e_percap = sum(emissions_furnish) / sum(all_ages),
      recreation_kgco2e_percap = sum(emissions_recreation) / sum(all_ages),
      transport_optranequip_other_kgco2e_percap = sum(emissions_transport_optranequip_other) / sum(all_ages),
      transport_vehiclepurchase_kgco2e_percap = sum(emissions_transport_vehiclepurchase) / sum(all_ages),
      transport_pt_kgco2e_percap = sum(emissions_transport_pt) / sum(all_ages),
      health_kgco2e_percap = sum(emissions_health) / sum(all_ages),
      education_kgco2e_percap = sum(emissions_education) / sum(all_ages),
      restaurant_kgco2e_percap = sum(emissions_restaurant) / sum(all_ages),
      misc_kgco2e_percap = sum(emissions_misc) / sum(all_ages),
      flights_kgco2e_percap = sum(flights_emissions_total) / sum(all_ages),
      goods_services_combined_kgco2e_percap = sum(goods_services_combined_total) / sum(all_ages),
      total_kgco2e_percap = sum(emissions_total) / sum(all_ages)

    )
  national_emissions$LAD25CD = "GB"

  la_emissions = rbind(national_emissions, la_emissions)

  la_emissions

}


make_oac_summary = function(lsoa_emissions_all, area_classifications_11_21, population){

  area_classifications_11_21 = area_classifications_11_21[,c("LSOA21CD","lsoa_class_code")]

  population = population[,c("LSOA21CD","year","all_ages")]

  lsoa_emissions_all = lsoa_emissions_all[,!grepl("grade",names(lsoa_emissions_all))]
  lsoa_emissions_all = lsoa_emissions_all[,!grepl("kgco2e_percap",names(lsoa_emissions_all))]

  lsoa_emissions_all = dplyr::left_join(lsoa_emissions_all, area_classifications_11_21, by = "LSOA21CD")
  lsoa_emissions_all = dplyr::left_join(lsoa_emissions_all, population, by = c("LSOA21CD","year"))

  la_emissions = lsoa_emissions_all |>
    dplyr::group_by(lsoa_class_code, year) |>
    dplyr::summarise(
      dom_gas_kgco2e_percap = sum(dom_gas_total_emissions) / sum(all_ages),
      dom_elec_kgco2e_percap = sum(dom_elec_total_emissions) / sum(all_ages),
      car_kgco2e_percap = sum(car_emissions) / sum(all_ages),
      van_kgco2e_percap = sum(van_emissions) / sum(all_ages),
      company_bike_kgco2e_percap = sum(company_bike_emissions) / sum(all_ages),
      heating_other_kgco2e_percap = sum(heating_other_emissions_total) / sum(all_ages),
      food_kgco2e_percap = sum(emissions_food) / sum(all_ages),
      alcohol_kgco2e_percap = sum(emissions_alcohol) / sum(all_ages),
      clothing_kgco2e_percap = sum(emissions_clothing) / sum(all_ages),
      communication_kgco2e_percap = sum(emissions_communication) / sum(all_ages),
      housing_other_kgco2e_percap = sum(emissions_housing_other) / sum(all_ages),
      furnish_kgco2e_percap = sum(emissions_furnish) / sum(all_ages),
      recreation_kgco2e_percap = sum(emissions_recreation) / sum(all_ages),
      transport_optranequip_other_kgco2e_percap = sum(emissions_transport_optranequip_other) / sum(all_ages),
      transport_vehiclepurchase_kgco2e_percap = sum(emissions_transport_vehiclepurchase) / sum(all_ages),
      transport_pt_kgco2e_percap = sum(emissions_transport_pt) / sum(all_ages),
      health_kgco2e_percap = sum(emissions_health) / sum(all_ages),
      education_kgco2e_percap = sum(emissions_education) / sum(all_ages),
      restaurant_kgco2e_percap = sum(emissions_restaurant) / sum(all_ages),
      misc_kgco2e_percap = sum(emissions_misc) / sum(all_ages),
      flights_kgco2e_percap = sum(flights_emissions_total) / sum(all_ages),
      goods_services_combined_kgco2e_percap = sum(goods_services_combined_total) / sum(all_ages),
      total_kgco2e_percap = sum(emissions_total) / sum(all_ages)

    )

  la_emissions

}
