#' Estimate spending on heating fuels other than gas/electricity
#'
#' @description Rough estimate of household spend on oil, solid fuel and
#'   other heating fuels per LSOA: households using each fuel (from the
#'   census central-heating data) x the LSOA's median gas demand (as a proxy
#'   for heat demand) x the fuel price (oil price used where no better price
#'   exists). Averaged over all households in the zone. Used by the
#'   `bills_other_heating` target.
#' @param ch_all Other-heating table with fuel counts and `median_gas_kwh`
#'   (the `other_heating_emissions` target).
#' @param prices_other_heating Fuel prices per kWh
#'   (`prices_other_heating` target).
#' @param population GB population/households (`population` target).
#' @return A data frame with `LSOA21CD`, `year`, `otherheating_spend_total`
#'   and `otherheating_average_bill`.
#' @keywords internal
estimate_other_heating_bills = function(ch_all, prices_other_heating, population){

  population = population[,c("LSOA21CD","year","households_est")]

  ch_all = ch_all[,c("LSOA21CD","year","oil","solid_fuel","other","two_or_more","median_gas_kwh")]

  ch_all = dplyr::left_join(ch_all, prices_other_heating[,c("year","smokeless_pound_kwh","oil_pound_kwh")], by = "year")
  ch_all = dplyr::left_join(ch_all, population, by = c("LSOA21CD","year"))

  ch_all$oil_spend_total = round(ch_all$oil * ch_all$median_gas_kwh * ch_all$oil_pound_kwh)
  ch_all$solidfuel_spend_total = round(ch_all$solid_fuel * ch_all$median_gas_kwh * ch_all$smokeless_pound_kwh)
  ch_all$otherfuel_spend_total = round(ch_all$other * ch_all$median_gas_kwh * ch_all$oil_pound_kwh) #No other data on fuel price
  ch_all$twofuel_spend_total = round(ch_all$two_or_more * ch_all$median_gas_kwh * ch_all$oil_pound_kwh) #No other data on fuel price

  ch_all$otherheating_spend_total = ch_all$oil_spend_total + ch_all$solidfuel_spend_total + ch_all$otherfuel_spend_total + ch_all$twofuel_spend_total


  ch_all$otherheating_average_bill = ch_all$otherheating_spend_total / ch_all$households_est

  ch_all = ch_all[,c("LSOA21CD","year","otherheating_spend_total","otherheating_average_bill")]

  ch_all

}
