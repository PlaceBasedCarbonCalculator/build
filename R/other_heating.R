calculate_other_heating = function(central_heating_2021, central_heating_2011_21, domestic_gas, population){

  #TODO: Add Scotland

  population = population[,c("year","LSOA21CD","all_ages")]

  # Emission Factors
  # Defra emission factors 2020
  # https://www.gov.uk/government/publications/greenhouse-gas-reporting-conversion-factors-2020

  emissions_oil <-   0.25964 # burning oil
  emissions_solid <- 0.36276 # coal
  emissions_other <- 0.25964 # Also oil, not sure what else to use
  emissions_wood <-  0.026 # SAP 2019 Wood chips
  emissions_gas <- 0.20374
  emissions_heat_network <- 0.3 # Gas CHP

  gas_average <- dplyr::group_by(domestic_gas, year)
  gas_average <- dplyr::summarise(gas_average,
                                  median_gas_kwh = median(median_gas_kwh, na.rm = TRUE))

  central_heating_2021 = central_heating_2021[,c("LSOA21CD","bottled_gas",
                                                  "oil","wood","solid_fuel",
                                                  "heat_network",
                                                  "other_central_heating",
                                                 "two_types_no_renewable_energy",
                                                 "two_types_inc_renewable_energy")]

  central_heating_2011_21 = central_heating_2011_21[,c("LSOA21CD","oil","solid_fuel","other","two_or_more")]

  # Use 2011 for 2010 - 2015, then 2021

  ch_2011 = list()
  for(i in 2010:2015){
    sub = central_heating_2011_21
    sub$year = i
    ch_2011[[i - 2009]] = sub
  }
  ch_2011 = dplyr::bind_rows(ch_2011)

  ch_2021 = list()
  for(i in 2016:2023){
    sub = central_heating_2021
    sub$year = i
    ch_2021[[i - 2015]] = sub
  }
  ch_2021 = dplyr::bind_rows(ch_2021)

  ch_2011$bottled_gas = 0
  ch_2011$wood = 0
  ch_2011$heat_network = 0
  ch_2011$two_types_no_renewable_energy = 0
  ch_2011$two_types_inc_renewable_energy = 0

  ch_2021$other = ch_2021$other_central_heating
  ch_2021$other_central_heating = NULL
  ch_2021$two_or_more = 0

  ch_all = rbind(ch_2011, ch_2021)

  ch_all = dplyr::left_join(ch_all, gas_average, by = "year")

  ch_all$heat_oil_emis_total        = ch_all$median_gas_kwh * emissions_oil *   ch_all$oil
  ch_all$heat_solid_fuel_emis_total = ch_all$median_gas_kwh * emissions_solid * ch_all$solid_fuel
  ch_all$heat_wood_emis_total       = ch_all$median_gas_kwh * emissions_wood  * ch_all$wood
  ch_all$heat_other_emis_total      = ch_all$median_gas_kwh * emissions_other * (ch_all$other)
  ch_all$heat_bottle_gas_emis_total = ch_all$median_gas_kwh * emissions_gas *   ch_all$bottled_gas
  ch_all$heat_network_emis_total    = ch_all$median_gas_kwh * emissions_heat_network *   ch_all$heat_network
  ch_all$heat_two_emis_total        = ch_all$median_gas_kwh * emissions_gas *   (ch_all$two_types_no_renewable_energy + ch_all$two_types_inc_renewable_energy)

  ch_all$heating_other_emissions_total = ch_all$heat_oil_emis_total  +
    ch_all$heat_solid_fuel_emis_total +
    ch_all$heat_wood_emis_total +
    ch_all$heat_other_emis_total +
    ch_all$heat_bottle_gas_emis_total +
    ch_all$heat_network_emis_total +
    ch_all$heat_two_emis_total

  ch_all = ch_all[,c("LSOA21CD","year","heating_other_emissions_total")]
  ch_all = dplyr::left_join(ch_all, population, by = c("LSOA21CD","year"))
  ch_all$heating_other_kgco2e_percap = ch_all$heating_other_emissions_total / ch_all$all_ages

  ch_all = ch_all[,c("LSOA21CD","year","heating_other_kgco2e_percap")]
  ch_all
}

load_central_heating_2011 = function(path = file.path(parameters$path_data,"nomis","2011")){
  dat = read.csv(file.path(path,"QS415UK - central heating.csv"), skip = 7)
  names(dat) = c("Area","LSOA11CD",
                 "all_households","no_central_heating",
                 "gas","electric",
                 "oil","solid_fuel",
                 "other","two_or_more")
  dat$Area = NULL
  dat
}

central_heating_2011_to_2021 = function(central_heating_2011, lsoa_11_21_tools){

  dat_S = central_heating_2011[central_heating_2011$LSOA11CD %in% lsoa_11_21_tools$lookup_split$LSOA11CD,]
  dat_M = central_heating_2011[central_heating_2011$LSOA11CD %in% lsoa_11_21_tools$lookup_merge$LSOA11CD,]
  dat_U = central_heating_2011[central_heating_2011$LSOA11CD %in% lsoa_11_21_tools$lookup_unchanged$LSOA11CD,]

  #Unchanged
  dat_U = dplyr::left_join(dat_U, lsoa_11_21_tools$lookup_unchanged, by = "LSOA11CD")

  # Merge
  dat_M = dplyr::left_join(dat_M, lsoa_11_21_tools$lookup_merge, by = "LSOA11CD")
  dat_M = dplyr::select(dat_M, -LSOA11CD)
  dat_M = dplyr::group_by(dat_M, LSOA21CD)
  dat_M = dplyr::summarise_all(dat_M, sum, na.rm = TRUE)
  dat_M = dplyr::ungroup(dat_M)

  #Split
  lookup_split = lsoa_11_21_tools$lookup_split
  lookup_split = lookup_split[,c("LSOA11CD","LSOA21CD","year","household_ratio")]
  lookup_split = lookup_split[lookup_split$year == 2011,]
  dat_S = dplyr::left_join(lookup_split, dat_S,
                            by = c("LSOA11CD"),
                            relationship = "many-to-many")
  dat_S = as.data.frame(dat_S)
  for(i in 5:12){
    dat_S[i] = dat_S[,i ,drop = TRUE] * dat_S$household_ratio
  }

  nms = c("LSOA21CD","all_households","no_central_heating",
          "gas","electric","oil","solid_fuel","other","two_or_more")

  dat_S = dat_S[,nms]
  dat_M = dat_M[,nms]
  dat_U = dat_U[,nms]

  final = rbind(dat_S, dat_M, dat_U)
  final

}
