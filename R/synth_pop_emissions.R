load_consumption_lookup = function(path = "../inputdata/consumption/PBCC_lookup.xlsx"){
  lookup = readxl::read_xlsx(path)
  lookup
}


calculate_consumption_lsoa = function(consumption_syth_pop, population, consumption_nations, consumption_lookup, consumption_multipliers_uk) {

  population = population[,c("LSOA21CD","year","all_ages")]
  population = population[population$year >= 2010,]

  names(consumption_syth_pop)[names(consumption_syth_pop) == "by"] = "LSOA21CD"


  # Build Emissions for each year

  consumption_nations = dplyr::left_join(consumption_nations, consumption_lookup, by = c("name" = "Name"))
  consumption_nations = consumption_nations[,c("year","name","value_uk","value_england","value_scotland",
                                               "value_wales","value_ni","LCFS_extended","Detail")]

  consumption_uk_grp = dplyr::group_by(consumption_nations, year, LCFS_extended) |>
    dplyr::summarise(value_uk = sum(value_uk, na.rm = TRUE),
                     value_england = sum(value_england, na.rm = TRUE),
                     value_scotland = sum(value_scotland, na.rm = TRUE),
                     value_wales = sum(value_wales, na.rm = TRUE),
                     value_ni = sum(value_ni, na.rm = TRUE)
                     )

  # One dud value for health in 2007
  # consumption_uk_grp$check = consumption_uk_grp$value_uk - consumption_uk_grp$value_england - consumption_uk_grp$value_scotland - consumption_uk_grp$value_wales - consumption_uk_grp$value_ni


  names(consumption_uk_grp) = gsub("value_","",names(consumption_uk_grp))

  consumption_uk_grp = consumption_uk_grp[,c("year","LCFS_extended","england","scotland","wales")]

  consumption_england_wide = tidyr::pivot_wider(consumption_uk_grp[,c("year","LCFS_extended","england")],
                                           names_from = "LCFS_extended",
                                           values_from = "england")
  consumption_scotland_wide = tidyr::pivot_wider(consumption_uk_grp[,c("year","LCFS_extended","scotland")],
                                                names_from = "LCFS_extended",
                                                values_from ="scotland")
  consumption_wales_wide = tidyr::pivot_wider(consumption_uk_grp[,c("year","LCFS_extended","wales")],
                                                names_from = "LCFS_extended",
                                                values_from = "wales")


  names(consumption_england_wide) = paste0("total_emisions_",names(consumption_england_wide))
  names(consumption_scotland_wide) = paste0("total_emisions_",names(consumption_scotland_wide))
  names(consumption_wales_wide) = paste0("total_emisions_",names(consumption_wales_wide))

  # Make synth pop for alternate years
  #TODO: Need fix for missing pops in even year, e.g. firs year of population is 2019
  consumption_syth_pop_odd = consumption_syth_pop
  consumption_syth_pop_odd$year = consumption_syth_pop_odd$year + 1
  consumption_syth_pop_odd = consumption_syth_pop_odd[consumption_syth_pop_odd$year <= max(consumption_england_wide$total_emisions_year),]

  consumption_syth_pop = rbind(consumption_syth_pop, consumption_syth_pop_odd)
  rm(consumption_syth_pop_odd)

  # Split by country
  consumption_syth_pop$country = substr(consumption_syth_pop$LSOA21CD,1,1)

  consumption_syth_pop_E = consumption_syth_pop[consumption_syth_pop$country == "E",]
  consumption_syth_pop_S = consumption_syth_pop[consumption_syth_pop$country == "S",]
  consumption_syth_pop_W = consumption_syth_pop[consumption_syth_pop$country == "W",]

  consumption_syth_pop_E = dplyr::left_join(consumption_syth_pop_E, consumption_england_wide, by = c("year" = "total_emisions_year"))
  consumption_syth_pop_S = dplyr::left_join(consumption_syth_pop_S, consumption_scotland_wide, by = c("year" = "total_emisions_year"))
  consumption_syth_pop_W = dplyr::left_join(consumption_syth_pop_W, consumption_wales_wide, by = c("year" = "total_emisions_year"))

  consumption_syth_pop = rbind(consumption_syth_pop_E,
                               consumption_syth_pop_S,
                               consumption_syth_pop_W)

  # Alt method for parts where we don't have national emissions
  # Public Transport and non-fuel vehicle spending
  consumption_multipliers_uk = consumption_multipliers_uk[consumption_multipliers_uk$product
                                                          %in% c("7.2.1 Spare parts and accessories for personal transport equipment",
                                                                 "7.2.2 Fuels and lubricants for personal transport equipment",
                                                                 "7.2.3 Maintenance and repair of personal transport equipment",
                                                                 "7.2.4 Other services in respect of personal transport equipment",
                                                                 "7.3.1_2 Passenger transport by railway and road"
  ),]
  consumption_multipliers_uk$type = ifelse(consumption_multipliers_uk$product == "7.3.1_2 Passenger transport by railway and road","pt","optranequip_other")

  consumption_multipliers_uk = consumption_multipliers_uk |>
    dplyr::group_by(year, type) |>
    dplyr::summarise(ghg_pound = mean(ghg_pound))

  consumption_multipliers_uk = consumption_multipliers_uk |>
    tidyr::pivot_wider(names_from = "type", values_from = "ghg_pound")

  names(consumption_multipliers_uk)[2:3] = paste0("multip_",names(consumption_multipliers_uk)[2:3])

  consumption_multipliers_uk$year = as.numeric(consumption_multipliers_uk$year)

  consumption_syth_pop = dplyr::left_join(consumption_syth_pop, consumption_multipliers_uk, by = "year")

  # Summarise Emissions
  consumption_syth_emiss = consumption_syth_pop |>
    dplyr::group_by(year, country) |>
    dplyr::mutate(emissions_food = spend_food * total_emisions_food * 1e6 / sum(spend_food),
                  emissions_alcohol = spend_alcohol * total_emisions_alcohol * 1e6 / sum(spend_alcohol),
                  emissions_clothing = spend_clothing * total_emisions_clothing * 1e6 / sum(spend_clothing),
                  emissions_communication = spend_communication * total_emisions_communication * 1e6 / sum(spend_communication),
                  emissions_housing_gaselecfuel = spend_housing_gaselecfuel * total_emisions_housing_gaselecfuel * 1e6 / sum(spend_housing_gaselecfuel),
                  emissions_housing_other = (spend_housing - spend_housing_gaselecfuel)  * total_emisions_housing * 1e6 / sum((spend_housing - spend_housing_gaselecfuel)),
                  emissions_furnish = spend_furnish * total_emisions_furnish * 1e6 / sum(spend_furnish),
                  emissions_recreation = spend_recreation * total_emisions_recreation * 1e6 / sum(spend_recreation),
                  emissions_transport_optranequip = (spend_transport_optranequip_fuel + spend_transport_optranequip_other) * total_emisions_transport_optranequip * 1e6 / sum((spend_transport_optranequip_fuel + spend_transport_optranequip_other)),
                  emissions_transport_optranequip_other = spend_transport_optranequip_other * multip_optranequip_other * (365/7), #Alt approach
                  emissions_transport_services = (spend_transport_services_pt + spend_transport_services_air) * total_emisions_transport_services * 1e6 / sum((spend_transport_services_pt + spend_transport_services_air)),
                  emissions_transport_pt = spend_transport_services_pt * multip_pt * (365/7), #Alt approach
                  emissions_transport_vehiclepurchase = spend_transport_vehiclepurchase * total_emisions_transport_vehiclepurchase * 1e6 / sum(spend_transport_vehiclepurchase),
                  emissions_health = spend_health * total_emisions_health * 1e6 / sum(spend_health),
                  emissions_education = spend_education * total_emisions_education * 1e6 / sum(spend_education),
                  emissions_restaurant = spend_restaurant * total_emisions_restaurant * 1e6 / sum(spend_restaurant),
                  emissions_misc = spend_misc * total_emisions_misc * 1e6 / sum(spend_misc)
    ) |>
    dplyr::ungroup()


  population = population[population$year %in% unique(consumption_syth_emiss$year),]

  consumption_syth_emiss = dplyr::left_join(population, consumption_syth_emiss, by = c("LSOA21CD" = "LSOA21CD", "year" = "year"))

  #NAs form LSOAs with 0 population

  for(i in 1:ncol(consumption_syth_emiss)){
    if(inherits(consumption_syth_emiss[[i]],"numeric")){
      consumption_syth_emiss[[i]][is.na(consumption_syth_emiss[[i]])] = 0
    }
    if(inherits(consumption_syth_emiss[[i]],"integer")){
      consumption_syth_emiss[[i]][is.na(consumption_syth_emiss[[i]])] = 0L
    }
  }

  consumption_syth_emiss = consumption_syth_emiss |>
    dplyr::mutate(emissions_percap_food = remove_inf(emissions_food / all_ages),
                  emissions_percap_alcohol = remove_inf(emissions_alcohol / all_ages),
                  emissions_percap_clothing = remove_inf(emissions_clothing / all_ages),
                  emissions_percap_communication = remove_inf(emissions_communication / all_ages),
                  emissions_percap_housing_other = remove_inf(emissions_housing_other / all_ages),
                  emissions_percap_housing_gaselecfuel = remove_inf(emissions_housing_gaselecfuel / all_ages),
                  emissions_percap_furnish = remove_inf(emissions_furnish / all_ages),
                  emissions_percap_recreation = remove_inf(emissions_recreation / all_ages),
                  emissions_percap_transport_services = remove_inf(emissions_transport_services / all_ages),
                  emissions_percap_transport_pt = remove_inf(emissions_transport_pt / all_ages),
                  emissions_percap_transport_optranequip = remove_inf(emissions_transport_optranequip / all_ages),
                  emissions_percap_transport_optranequip_other = remove_inf(emissions_transport_optranequip_other / all_ages),
                  emissions_percap_transport_vehiclepurchase = remove_inf(emissions_transport_vehiclepurchase / all_ages),
                  emissions_percap_health = remove_inf(emissions_health / all_ages),
                  emissions_percap_education  = remove_inf(emissions_education  / all_ages),
                  emissions_percap_restaurant = remove_inf(emissions_restaurant / all_ages),
                  emissions_percap_misc = remove_inf(emissions_misc / all_ages)
    )




  consumption_syth_emiss = consumption_syth_emiss[,!grepl("^total_",names(consumption_syth_emiss))] # Drop the total_ columns as name is confusing

  consumption_syth_emiss

  # foo = consumption_syth_emiss[consumption_syth_emiss$LSOA21CD == "E01000006",]
  # bar = consumption_emissions[consumption_emissions$LSOA21CD == "E01000006",]
  # foo = foo[foo$year != 2021,]
  # foo = foo[order(foo$year, foo$LSOA21CD),]
  # bar = bar[order(bar$year, foo$LSOA21CD),]
  # summary(foo$year == bar$year)
  #
  # summary((foo$emissions_percap_food - bar$emissions_percap_food) / bar$emissions_percap_food * 100)
  # summary((foo$emissions_percap_recreation - bar$emissions_percap_recreation) / bar$emissions_percap_recreation * 100)
  # summary((foo$emissions_percap_education - bar$emissions_percap_education) / bar$emissions_percap_education * 100)
  #
  # education
  #
  # summary((foo$income_total - bar$income_total) / bar$income_total * 100)
  #
  # summary((foo$emissions_food - bar$emissions_food) / bar$emissions_food * 100)

}


remove_inf = function(x){
  x[is.infinite(x)] = 0
  x[is.nan(x)] = 0
  x
}
