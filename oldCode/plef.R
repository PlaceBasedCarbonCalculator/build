load_plef = function(path = file.path(parameters$path_data,"PLEF")){
  plef = readxl::read_excel(file.path(path,"PLEF Tables.xlsx"), sheet = "Fig 9")
  plef

}


forcast_emissions_plef = function(lsoa_emissions_all, plef){
  lsoa_emissions_all = lsoa_emissions_all[,c("LSOA21CD",names(lsoa_emissions_all)[grepl("percap_2020",names(lsoa_emissions_all))])]

  # Match to PLEF Sectors
  lsoa_emissions_all$PLEF_residential_2020 =
    lsoa_emissions_all$dom_gas_kgco2e_percap_2020 +
    lsoa_emissions_all$dom_elec_kgco2e_percap_2020 +
    lsoa_emissions_all$heating_other_kgco2e_percap_2020

  lsoa_emissions_all$PLEF_transport_2020 =
    lsoa_emissions_all$car_emissions_percap_2020 +
    lsoa_emissions_all$van_emissions_percap_2020 +
    lsoa_emissions_all$emissions_flights_percap_2020

  lsoa_emissions_all$PLEF_agriculture_2020 =
    lsoa_emissions_all$nutrition_emissions_percap_2020

  lsoa_emissions_all$PLEF_industry_2020 =
    lsoa_emissions_all$consumables_emissions_percap_2020

  lsoa_emissions_all$PLEF_nondomestic_2020 =
    lsoa_emissions_all$services_emissions_percap_2020 +
    lsoa_emissions_all$recreation_emissions_percap_2020

  lsoa_emissions_all = lsoa_emissions_all[,c("LSOA21CD",names(lsoa_emissions_all)[grepl("PLEF",names(lsoa_emissions_all))])]

  plef$Scenario = tolower(plef$Scenario)
  names(plef) = tolower(gsub("-","",names(plef)))
  lsoa_emissions_all = as.data.frame(lsoa_emissions_all)
  plef = as.data.frame(plef)

  # Loop over Scenarios and years
  # Grandfather
  for(scenario in c("ignore","steer","shift","transform")){
    for(year in c(2030,2040,2050)){
      for(sector in c("residential","transport","agriculture","industry","nondomestic")){
        lsoa_emissions_all[,paste0("PLEF_",sector,"_grandfathered_",scenario,"_",year)] =
          lsoa_emissions_all[,paste0("PLEF_",sector,"_2020")] * (1 + plef[plef$year == year & plef$scenario == scenario, sector])
      }
    }
  }

  # Equity: Equal share per cap by 2050
  for(scenario in c("ignore","steer","shift","transform")){
    for(sector in c("residential","transport","agriculture","industry","nondomestic")){
      lsoa_emissions_all = cbind(lsoa_emissions_all,
                                  plef_equity(
                                    x = lsoa_emissions_all[,paste0("PLEF_",sector,"_2020")],
                                    plef_sub = plef[plef$scenario == scenario, sector],
                                    nm = paste0("PLEF_",sector,"_equity_",scenario,"_")
                                  ))
    }
  }

  lsoa_emissions_all
}


# Takes in 2020 emissions and sets targets that archive overall reductions and
# reach eaqual emissions per person in 2050
#' @param x basline 2020 emissions for a sector
#' @param plef_sub plef values for sector and scenario
# x = lsoa_emissions_all$PLEF_residential_2020
#plef_sub = plef$residential[plef$scenario == "transform"]

plef_equity = function(x, plef_sub, nm){
  xtot = sum(x, na.rm = TRUE)
  xtot_years = (1 + plef_sub) * xtot
  names(xtot_years) = c(2020,2030,2040,2050)

  length_x = length(x[!is.na(x)])

  emission_2050 = rep(xtot_years[4]/length_x,length(x))
  emission_2050 = unname(emission_2050)
  emission_2050 = ifelse(is.na(x),NA,emission_2050)

  #Interpolate reduction in variation between 2020 and 2050
  #y = mx+c, x =1
  mean_2020 = mean(x, na.rm = TRUE)
  var_2020 = (x-mean_2020)/mean_2020

  grad = -var_2020/3
  int = var_2020

  var_2030 = 1 * grad + int
  var_2040 = 2 * grad + int

  mean_2030 = xtot_years[2] / length_x
  mean_2040 = xtot_years[3] / length_x

  emission_2030 = var_2030 * mean_2030 + mean_2030
  emission_2040 = var_2040 * mean_2040 + mean_2040

  res = data.frame(X2030 = emission_2030,
                   X2040 = emission_2040,
                   X2050 = emission_2050
                   )
  names(res) = paste0(nm, gsub("X","",names(res)))

  # Check
  diff = round(xtot_years - c(sum(x, na.rm = T),colSums(res, na.rm = T)))
  if(!all(diff == 0)){
    stop("Resutls don't match inputs")
  }

  res
}
