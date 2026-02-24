consumption_footprint_syth_pop = function(synth_households_lcfs_2022,
                                          synth_households_lcfs_2020,
                                          synth_households_lcfs_2018,
                                          synth_households_lcfs_2016,
                                          synth_households_lcfs_2014,
                                          synth_households_lcfs_2012,
                                          synth_households_lcfs_2010,
                                          synth_households_lcfs_2022_scotland,
                                          synth_households_lcfs_2020_scotland,
                                          synth_households_lcfs_2018_scotland,
                                          synth_households_lcfs_2016_scotland,
                                          synth_households_lcfs_2014_scotland,
                                          synth_households_lcfs_2012_scotland,
                                          synth_households_lcfs_2010_scotland
){

  # Standardise inputs
  synth_households_lcfs_2010$flight_other = 0
  synth_households_lcfs_2010_scotland$flight_other = 0


  synth_households_lcfs_2022 = consumption_lsoa_summary(synth_households_lcfs_2022)
  synth_households_lcfs_2020 = consumption_lsoa_summary(synth_households_lcfs_2020)
  synth_households_lcfs_2018 = consumption_lsoa_summary(synth_households_lcfs_2018)
  synth_households_lcfs_2016 = consumption_lsoa_summary(synth_households_lcfs_2016)
  synth_households_lcfs_2014 = consumption_lsoa_summary(synth_households_lcfs_2014)
  synth_households_lcfs_2012 = consumption_lsoa_summary(synth_households_lcfs_2012)
  synth_households_lcfs_2010 = consumption_lsoa_summary(synth_households_lcfs_2010)

  synth_households_lcfs_2022_scotland = consumption_lsoa_summary(synth_households_lcfs_2022_scotland)
  synth_households_lcfs_2020_scotland = consumption_lsoa_summary(synth_households_lcfs_2020_scotland)
  synth_households_lcfs_2018_scotland = consumption_lsoa_summary(synth_households_lcfs_2018_scotland)
  synth_households_lcfs_2016_scotland = consumption_lsoa_summary(synth_households_lcfs_2016_scotland)
  synth_households_lcfs_2014_scotland = consumption_lsoa_summary(synth_households_lcfs_2014_scotland)
  synth_households_lcfs_2012_scotland = consumption_lsoa_summary(synth_households_lcfs_2012_scotland)
  synth_households_lcfs_2010_scotland = consumption_lsoa_summary(synth_households_lcfs_2010_scotland)

  synth_households_lcfs_2022$year = 2022
  synth_households_lcfs_2020$year = 2020
  synth_households_lcfs_2018$year = 2018
  synth_households_lcfs_2016$year = 2016
  synth_households_lcfs_2014$year = 2014
  synth_households_lcfs_2012$year = 2012
  synth_households_lcfs_2010$year = 2010

  synth_households_lcfs_2022_scotland$year = 2022
  synth_households_lcfs_2020_scotland$year = 2020
  synth_households_lcfs_2018_scotland$year = 2018
  synth_households_lcfs_2016_scotland$year = 2016
  synth_households_lcfs_2014_scotland$year = 2014
  synth_households_lcfs_2012_scotland$year = 2012
  synth_households_lcfs_2010_scotland$year = 2010

  res = dplyr::bind_rows(list(synth_households_lcfs_2022, synth_households_lcfs_2022_scotland,
                              synth_households_lcfs_2020, synth_households_lcfs_2020_scotland,
              synth_households_lcfs_2018, synth_households_lcfs_2018_scotland,
              synth_households_lcfs_2016, synth_households_lcfs_2016_scotland,
              synth_households_lcfs_2014, synth_households_lcfs_2014_scotland,
              synth_households_lcfs_2012, synth_households_lcfs_2012_scotland,
              synth_households_lcfs_2010, synth_households_lcfs_2010_scotland))

  res


}

consumption_lsoa_summary = function(synth_households_lcfs_2020){

  sub = dplyr::group_by(synth_households_lcfs_2020, by = LSOA21CD) |>
    dplyr::summarise(households = dplyr::n(),

                     income_total = sum(annual_income),
                     income_mean = mean(annual_income),
                     income_min = min(annual_income),
                     income_max = max(annual_income),

                     spend_food = sum(spend_food[spend_food > 0]),
                     spend_alcohol = sum(spend_alcohol[spend_alcohol > 0]),
                     spend_clothing = sum(spend_clothing[spend_clothing > 0]),
                     spend_housing = sum(spend_housing[spend_housing > 0]),
                     spend_furnish = sum(spend_furnish[spend_furnish > 0]),
                     spend_health = sum(spend_health[spend_health > 0]),
                     spend_transport = sum(spend_transport[spend_transport > 0]),
                     spend_communication = sum(spend_communication[spend_communication > 0]),
                     spend_recreation = sum(spend_recreation[spend_recreation > 0]),
                     spend_education = sum(spend_education[spend_education > 0]),
                     spend_restaurant = sum(spend_restaurant[spend_restaurant > 0]),
                     spend_misc = sum(spend_misc[spend_misc > 0]),
                     spend_nonconsump = sum(spend_nonconsump[spend_nonconsump > 0]),
                     spend_totalconsump = sum(spend_totalconsump[spend_totalconsump > 0]),
                     spend_total = sum(spend_total[spend_total > 0]),

                     spend_housing_gaselecfuel = sum(spend_housing_gaselecfuel[spend_housing_gaselecfuel > 0]),
                     spend_housing_gaselec = sum(spend_housing_gaselec[spend_housing_gaselec > 0]),
                     spend_housing_otherfuels = sum(spend_housing_otherfuels[spend_housing_otherfuels > 0]),
                     spend_housing_gaselec_seconddwelling = sum(spend_housing_gaselec_seconddwelling[spend_housing_gaselec_seconddwelling > 0]),
                     spend_housing_gaselec_rebates = sum(spend_housing_gaselec_rebates),


                     spend_transport_vehiclepurchase = sum(spend_transport_vehiclepurchase[spend_transport_vehiclepurchase > 0]),
                     spend_transport_optranequip_fuel = sum(spend_transport_optranequip_fuel[spend_transport_optranequip_fuel > 0]),
                     spend_transport_optranequip_other = sum(spend_transport_optranequip_other[spend_transport_optranequip_other > 0]),
                     spend_transport_services_pt = sum(spend_transport_services_pt[spend_transport_services_pt > 0]),
                     spend_transport_services_air = sum(spend_transport_services_air[spend_transport_services_air > 0]),

                     cars_new_purchased = sum(cars_new),
                     cars_secondhand_purchased = sum(cars_secondhand),
                     motorcycles_owned  = sum(motorcycles),


                     flight_international_return = sum(flight_international_return, na.rm = TRUE),
                     flight_other = sum(flight_other, na.rm = TRUE),
                     flight_domestic_return = sum(flight_domestic_return, na.rm = TRUE),
                     flight_domestic_single = sum(flight_domestic_single, na.rm = TRUE)


                     )


}
