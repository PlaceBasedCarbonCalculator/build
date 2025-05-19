load_consumption_lookup = function(path = "../inputdata/consumption/PBCC_lookup.xlsx"){
  lookup = readxl::read_xlsx(path)
  lookup
}


calculate_consumption_lsoa = function(consumption_syth_pop, population, consumption_uk, consumption_lookup) {

  population = population[,c("LSOA21CD","year","all_ages")]

  names(consumption_syth_pop)[names(consumption_syth_pop) == "by"] = "LSOA21CD"


  # Build Emissions for each year

  consumption_uk = dplyr::left_join(consumption_uk, consumption_lookup, by = c("name" = "Name"))
  consumption_uk = consumption_uk[,c("year","name","value","LCFS_extended","Detail")]

  consumption_uk_grp = dplyr::group_by(consumption_uk, year, LCFS_extended) |>
    dplyr::summarise(value = sum(value))

  consumption_uk_wide = tidyr::pivot_wider(consumption_uk_grp, names_from = "LCFS_extended", values_from = "value")
  names(consumption_uk_wide) = paste0("total_emisions_",names(consumption_uk_wide))


  consumption_syth_pop_odd = consumption_syth_pop
  consumption_syth_pop_odd$year = consumption_syth_pop_odd$year + 1
  consumption_syth_pop_odd = consumption_syth_pop_odd[consumption_syth_pop_odd$year <= max(consumption_uk_wide$total_emisions_year),]

  consumption_syth_pop = rbind(consumption_syth_pop, consumption_syth_pop_odd)
  rm(consumption_syth_pop_odd)

  consumption_syth_pop = dplyr::left_join(consumption_syth_pop, consumption_uk_wide, by = c("year" = "total_emisions_year"))

  consumption_syth_pop = consumption_syth_pop |>
    dplyr::group_by(year) |>
    dplyr::mutate(emissions_food = spend_food * total_emisions_food * 1e6 / sum(spend_food),
                  emissions_alcohol = spend_alcohol * total_emisions_alcohol * 1e6 / sum(spend_alcohol),
                  emissions_clothing = spend_clothing * total_emisions_clothing * 1e6 / sum(spend_clothing),
                  emissions_communication = spend_communication * total_emisions_communication * 1e6 / sum(spend_communication),
                  emissions_housing_gaselecfuel = spend_housing_gaselecfuel * total_emisions_housing_gaselecfuel * 1e6 / sum(spend_housing_gaselecfuel),
                  emissions_housing_other = (spend_housing - spend_housing_gaselecfuel)  * total_emisions_housing * 1e6 / sum((spend_housing - spend_housing_gaselecfuel)),
                  emissions_furnish = spend_furnish * total_emisions_furnish * 1e6 / sum(spend_furnish),
                  emissions_recreation = spend_recreation * total_emisions_recreation * 1e6 / sum(spend_recreation),
                  emissions_transport_optranequip = (spend_transport_optranequip_fuel + spend_transport_optranequip_other) * total_emisions_transport_optranequip * 1e6 / sum((spend_transport_optranequip_fuel + spend_transport_optranequip_other)),
                  emissions_transport_services = (spend_transport_services_pt + spend_transport_services_air) * total_emisions_transport_services * 1e6 / sum((spend_transport_services_pt + spend_transport_services_air)),
                  emissions_transport_vehiclepurchase = spend_transport_vehiclepurchase * total_emisions_transport_vehiclepurchase * 1e6 / sum(spend_transport_vehiclepurchase),
                  emissions_health = spend_health * total_emisions_health * 1e6 / sum(spend_health),
                  emissions_education = spend_education * total_emisions_education * 1e6 / sum(spend_education),
                  emissions_restaurant = spend_restaurant * total_emisions_restaurant * 1e6 / sum(spend_restaurant),
                  emissions_misc = spend_misc * total_emisions_misc * 1e6 / sum(spend_misc)
    ) |>
    dplyr::ungroup()


  consumption_syth_pop = dplyr::left_join(consumption_syth_pop, population, by = c("LSOA21CD" = "LSOA21CD", "year" = "year"))

  consumption_syth_pop = consumption_syth_pop |>
    dplyr::mutate(emissions_percap_food = remove_inf(emissions_food / all_ages),
                  emissions_percap_alcohol = remove_inf(emissions_alcohol / all_ages),
                  emissions_percap_clothing = remove_inf(emissions_clothing / all_ages),
                  emissions_percap_communication = remove_inf(emissions_communication / all_ages),
                  emissions_percap_housing_other = remove_inf(emissions_housing_other / all_ages),
                  emissions_percap_housing_gaselecfuel = remove_inf(emissions_housing_gaselecfuel / all_ages),
                  emissions_percap_furnish = remove_inf(emissions_furnish / all_ages),
                  emissions_percap_recreation = remove_inf(emissions_recreation / all_ages),
                  emissions_percap_transport_services = remove_inf(emissions_transport_services / all_ages),
                  emissions_percap_transport_optranequip = remove_inf(emissions_transport_optranequip / all_ages),
                  emissions_percap_transport_vehiclepurchase = remove_inf(emissions_transport_vehiclepurchase / all_ages),
                  emissions_percap_health = remove_inf(emissions_health / all_ages),
                  emissions_percap_education  = remove_inf(emissions_education  / all_ages),
                  emissions_percap_restaurant = remove_inf(emissions_restaurant / all_ages),
                  emissions_percap_misc = remove_inf(emissions_misc / all_ages)
    )


  #foo = consumption_syth_pop[consumption_syth_pop$by == "E01000006",]



  consumption_syth_pop


}


remove_inf = function(x){
  x[is.infinite(x)] = 0
  x
}
