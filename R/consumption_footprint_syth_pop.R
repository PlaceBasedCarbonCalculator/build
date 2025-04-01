consumption_footprint_syth_pop = function(synth_households_lcfs_2020,
                                          synth_households_lcfs_2018,
                                          synth_households_lcfs_2016,
                                          synth_households_lcfs_2014


){


}

consumption_lsoa_summary = function(synth_households_lcfs_2020){

  sub = dplyr::group_by(synth_households_lcfs_2020, by = LSOA) |>
    dplyr::summarise(households = dplyr::n(),

                     income_total = sum(annual_income),
                     income_mean = mean(annual_income),
                     income_min = min(annual_income),
                     income_max = max(annual_income),

                     spend_food = sum(spend_food),
                     spend_alcohol = sum(spend_alcohol),
                     spend_clothing = sum(spend_clothing),
                     spend_housing = sum(spend_housing),
                     spend_furnish = sum(spend_furnish),
                     spend_health = sum(spend_health),
                     spend_transport = sum(spend_transport),
                     spend_communication = sum(spend_communication),
                     spend_recreation = sum(spend_recreation),
                     spend_education = sum(spend_education),
                     spend_restaurant = sum(spend_restaurant),
                     spend_misc = sum(spend_misc),
                     spend_nonconsump = sum(spend_nonconsump),
                     spend_totalconsump = sum(spend_totalconsump),
                     spend_total = sum(spend_total),

                     cars_new_purchased = sum(cars_new),
                     cars_secondhand_purchased = sum(cars_secondhand),
                     motorcycles_owned  = sum(motorcycles),


                     flight_international_return = sum(flight_international_return, na.rm = TRUE),
                     flight_other = sum(flight_other, na.rm = TRUE),
                     flight_domestic_return = sum(flight_domestic_return, na.rm = TRUE),
                     flight_domestic_single = sum(flight_domestic_single, na.rm = TRUE)


                     )


}
