select_retofit_vars = function(epc_dom_summary, population, house_prices_nowcast,
                               income_lsoa_msoa, income_scot_dz22,
                               domestic_electricity, domestic_gas,
                               bills_gas_electric, bills_other_heating) {

  bills_gas_electric = bills_gas_electric[,c("LSOA21CD","year","energy_average_bill")]
  bills_gas_electric = bills_gas_electric[bills_gas_electric$year == max(bills_gas_electric$year),]

  #bills_other_heating = bills_other_heating[bills_other_heating$year == max(bills_other_heating$year),]
  # Scotland late on other heating data
  bills_other_heating = bills_other_heating[bills_other_heating$year >= 2022,]
  bills_other_heating_EW = bills_other_heating[substr(bills_other_heating$LSOA21CD,1,1) %in% c("E","W"),]
  bills_other_heating_EW = bills_other_heating_EW[bills_other_heating_EW$year == max(bills_other_heating_EW$year),]

  bills_other_heating_S = bills_other_heating[substr(bills_other_heating$LSOA21CD,1,1) == "S",]
  bills_other_heating_S = bills_other_heating_S[!is.na(bills_other_heating_S$otherheating_average_bill),]
  bills_other_heating_S = bills_other_heating_S[bills_other_heating_S$year == max(bills_other_heating_S$year),]

  bills_other_heating = rbind(bills_other_heating_EW, bills_other_heating_S)

  if(length(unique(c(bills_gas_electric$year[1]),c(bills_other_heating$year[1]))) != 1){
    stop("Other heating and gas/electric bills are from different years")
  }
  bills_gas_electric$year = NULL
  bills_other_heating$year = NULL

  sub = epc_dom_summary[,c("LSOA21CD","epc_total","epc_score_avg","floor_area_avg")]
  sub$modal_age = modal(epc_dom_summary[,c("age_pre1900",
                                           "age_19001929","age_19301949","age_19501966",
                                           "age_19671975","age_19761982","age_19831990",
                                           "age_19911995","age_19962002","age_20032006",
                                           "age_20072011","age_20122021","age_post2022")])
  sub$modal_wall = modal(epc_dom_summary[,c("wall_verygood","wall_good",
                                            "wall_average","wall_poor","wall_verypoor","wall_other")])
  sub$modal_roof = modal(epc_dom_summary[,c("roof_verygood","roof_good",
                                            "roof_average","roof_poor","roof_verypoor","roof_above",
                                            "roof_other")])
  sub$modal_heat = modal(epc_dom_summary[,c("mainheat_verygood","mainheat_good",
                                            "mainheat_average","mainheat_poor","mainheat_verypoor","mainheat_other")])
  sub$modal_window = modal(epc_dom_summary[,c("window_verygood","window_good","window_average","window_poor",
                                              "window_verypoor","window_other")])


  sub$modal_mainheat = modal(epc_dom_summary[,c("mainheatdesc_gasboiler","mainheatdesc_oilboiler","mainheatdesc_storageheater","mainheatdesc_portableheater",
                                            "mainheatdesc_roomheater","mainheatdesc_heatpump","mainheatdesc_community")])
  sub$modal_mainfuel = modal(epc_dom_summary[,c("mainfuel_mainsgas",
                                                "mainfuel_electric","mainfuel_oil","mainfuel_coal","mainfuel_lpg",
                                                "mainfuel_biomass","mainfuel_dualfuel")])
  sub$modal_floord = modal(epc_dom_summary[,c("floord_soliduninsulated","floord_solidinsulated","floord_solidlimitedinsulated",
                                              "floord_suspendeduninsulated","floord_suspendedinsualted","floord_suspendedlimitedinsulated","floord_below")])

  type = epc_dom_summary[,c("type_house_semi","type_house_midterrace","type_house_endterrace","type_house_detached","type_flat",
                            "type_bungalow_semi","type_bungalow_midterrace","type_bungalow_endterrace","type_bungalow_detached",
                            "type_maisonette","type_parkhome")]
  names(type) = gsub("type_","",names(type))

  sub$modal_type = modal(type, drop = FALSE)

  sub$modal_tenure = modal(epc_dom_summary[,c("tenure_owner","tenure_privaterent",
                                              "tenure_socialrent","tenure_unknown")])

  population = population[,c("LSOA21CD","year","households_est" , "all_properties")]
  population = population[population$year == 2022,]
  population$year = NULL

  #TODO: Scotland
  sub = dplyr::left_join(sub, population, by = "LSOA21CD")

  sub$percent_EPC = round(sub$epc_total / sub$all_properties * 100)
  # sub$households_est = NULL
  # sub$all_properties = NULL
  # sub$epc_total = NULL

  sub$epc_score_avg = as.character(cut(sub$epc_score_avg, breaks = c(0,50,55,60,65,70,80,100)))
  sub$epc_score_avg = gsub("\\(|\\[|\\)|\\]","",sub$epc_score_avg)
  sub$epc_score_avg = gsub(",","-",sub$epc_score_avg)

  sub$floor_area_avg = as.character(cut(sub$floor_area_avg, breaks = c(0,40,60,80,100,120,140,160,500)))
  sub$floor_area_avg = gsub("\\(|\\[|\\)|\\]","",sub$floor_area_avg)
  sub$floor_area_avg = gsub(",","-",sub$floor_area_avg)

  sub$percent_EPC = as.character(cut(sub$percent_EPC, breaks = c(0,30,50,60,70,80,90,200)))
  sub$percent_EPC = gsub("\\(|\\[|\\)|\\]","",sub$percent_EPC)
  sub$percent_EPC = gsub(",","-",sub$percent_EPC)

  # House Prices
  # Not all areas have a transaction in 2024, so take most recent year

  # house_prices_lsoa = house_prices_lsoa[,c("LSOA21CD","year","transactions","price_median")]
  # house_prices_lsoa = house_prices_lsoa |>
  #   dplyr::group_by(LSOA21CD) |>
  #   dplyr::summarise(max_year = max(year),
  #     price_median = price_median[year == max_year],
  #     transactions = transactions[year == max_year]
  #     )
  # house_prices_lsoa = house_prices_lsoa[substr(house_prices_lsoa$LSOA21CD,1,1) != "S",] #Attribution error
  # house_prices_lsoa$price_median[house_prices_lsoa$transactions < 3] = NA
  # TODO: 373 LSOAs have less than 5 transactions per year
  # sub = dplyr::left_join(sub, house_prices_lsoa[,c("LSOA21CD","price_median")], by = "LSOA21CD")

  house_prices_nowcast = house_prices_nowcast[,c("LSOA21CD","price_2024")]
  house_prices_nowcast = house_prices_nowcast |>
    dplyr::group_by(LSOA21CD) |>
    dplyr::summarise(price_2024 = median(price_2024))
  sub = dplyr::left_join(sub, house_prices_nowcast, by = "LSOA21CD")


  # Income
  income_lsoa_msoa = income_lsoa_msoa[income_lsoa_msoa$year == max(income_lsoa_msoa$year),]
  income_lsoa_msoa = income_lsoa_msoa[,c("LSOA21CD","total_annual_income")]

  income_scot_dz22 = income_scot_dz22[income_scot_dz22$year == max(income_scot_dz22$year),]
  income_scot_dz22 = income_scot_dz22[,c("DataZone22","total_annual_income")]
  names(income_scot_dz22) = c("LSOA21CD","total_annual_income")

  income = rbind(income_lsoa_msoa, income_scot_dz22)

  sub = dplyr::left_join(sub, income, by = "LSOA21CD")
  #sub$house_income_ratio = round(sub$price_median / sub$total_annual_income,1)

  sub$house_income_ratio = round(sub$price_2024 / sub$total_annual_income,1)

  #Energy
  domestic_gas = domestic_gas[domestic_gas$year == max(domestic_gas$year),]
  domestic_electricity = domestic_electricity[domestic_electricity$year == max(domestic_electricity$year),]

  domestic_gas = domestic_gas[,c("LSOA21CD","median_gas_kwh")]
  domestic_electricity = domestic_electricity[,c("LSOA21CD","median_elec_kwh")]

  sub = dplyr::left_join(sub, domestic_gas, by = "LSOA21CD")
  sub = dplyr::left_join(sub, domestic_electricity, by = "LSOA21CD")

  sub = dplyr::left_join(sub, bills_gas_electric, by = "LSOA21CD")
  sub = dplyr::left_join(sub, bills_other_heating, by = "LSOA21CD")

  #sub$energy_average_bill_other = sub$otherheating_spend_total / sub$households_est
  sub$energy_average_bill_both =  sub$energy_average_bill + sub$otherheating_average_bill

  # Bivariate
  sub$fuelcost_bivaraite = bivariate_categories(sub$energy_average_bill_both, sub$total_annual_income)

  sub = sub[,c("LSOA21CD","epc_score_avg","floor_area_avg","modal_age","modal_wall",
               "modal_roof","modal_heat","modal_window","modal_mainheat","modal_mainfuel",
               "modal_floord","modal_type","modal_tenure","percent_EPC","price_2024",
               "house_income_ratio","median_gas_kwh","median_elec_kwh","fuelcost_bivaraite" )]

  sub


}

modal = function(df, drop = TRUE){
  x = apply(df, 1, function(x) names(df)[x == max(x)][1])
  if(drop){
    x = strsplit(x,"_")
    x = sapply(x,`[[`,2)
  }
  x
}
