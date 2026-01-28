#TODO; This is a very rough estimate of energy spending on fuels other than
#gas/electric it assumes that heat demand is the UK average and that people by
#oil or smokeless solid fuel.
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
