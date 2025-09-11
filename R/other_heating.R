calculate_other_heating = function(central_heating_2021,
                                   central_heating_2011,
                                   central_heating_2011_scotland,
                                   central_heating_2022_scotland,
                                   domestic_gas,
                                   population){


  #TODO: Add Scotland

  population = population[,c("year","LSOA21CD","all_ages","all_properties")]

  # Emission Factors
  # Defra emission factors 2020
  # https://www.gov.uk/government/publications/greenhouse-gas-reporting-conversion-factors-2020

  emissions_oil <-   0.25964 # burning oil
  emissions_solid <- 0.36276 # coal
  emissions_other <- 0.25964 # Also oil, not sure what else to use
  emissions_wood <-  0.026 # SAP 2019 Wood chips
  emissions_gas <- 0.20374
  emissions_heat_network <- 0.3 # Gas CHP


  # For houses not heated by gas/electricty we assume they use the avaege amount of heat a gas home uses
  gas_average <- dplyr::group_by(domestic_gas, year)
  gas_average <- dplyr::summarise(gas_average,
                                  median_gas_kwh = median(median_gas_kwh, na.rm = TRUE))

  central_heating_2021 = central_heating_2021[,c("LSOA21CD","all_households","bottled_gas",
                                                  "oil","wood","solid_fuel",
                                                  "heat_network",
                                                  "other_central_heating",
                                                 "two_types_no_renewable_energy",
                                                 "two_types_inc_renewable_energy")]

  central_heating_2011 = central_heating_2011[,c("LSOA21CD","all_households","oil","solid_fuel","other","two_or_more")]

  central_heating_2022_scotland = central_heating_2022_scotland[,c("LSOA21CD","total","oil","solid_fuel_exwood","other","two_or_more")]
  names(central_heating_2022_scotland) = c("LSOA21CD","all_households","oil","solid_fuel","other_central_heating","two_types_no_renewable_energy")

  central_heating_2011_scotland = central_heating_2011_scotland[,c("LSOA21CD","occupied_households","oil","solid_fuel","other","two_or_more")]
  names(central_heating_2011_scotland) = c("LSOA21CD","all_households","oil","solid_fuel","other","two_or_more")

  central_heating_2021 = dplyr::bind_rows(list(central_heating_2021, central_heating_2022_scotland))
  central_heating_2011 = dplyr::bind_rows(list(central_heating_2011, central_heating_2011_scotland))

  central_heating_2021$bottled_gas[is.na(central_heating_2021$bottled_gas)] = 0
  central_heating_2021$wood[is.na(central_heating_2021$wood)] = 0
  central_heating_2021$heat_network[is.na(central_heating_2021$heat_network)] = 0
  central_heating_2021$two_types_inc_renewable_energy[is.na(central_heating_2021$two_types_inc_renewable_energy)] = 0

  # Use 2011 for 2010 - 2015, then 2021

  ch_2011 = list()
  for(i in 2010:2015){
    sub = central_heating_2011
    sub$year = i
    pop_sub = population[population$year == i,]
    sub = dplyr::left_join(sub, pop_sub, by = c("LSOA21CD","year"))
    sub$splitratio = ifelse(sub$all_households != 0,
                            sub$all_properties / sub$all_households,
                            0)

    sub$oil = sub$oil * sub$splitratio
    sub$solid_fuel = sub$solid_fuel * sub$splitratio
    sub$other = sub$other * sub$splitratio
    sub$two_or_more = sub$two_or_more * sub$splitratio

    sub = sub[,c("LSOA21CD","year","oil","solid_fuel","other","two_or_more")]

    ch_2011[[i - 2009]] = sub
  }
  ch_2011 = dplyr::bind_rows(ch_2011)

  ch_2021 = list()
  for(i in 2016:2021){
    sub = central_heating_2021
    sub$year = i

    pop_sub = population[population$year == i,]
    sub = dplyr::left_join(sub, pop_sub, by = c("LSOA21CD","year"))
    sub$splitratio = ifelse(sub$all_households != 0,
                            sub$all_properties / sub$all_households,
                            0)

    sub$bottled_gas = sub$bottled_gas * sub$splitratio
    sub$oil = sub$oil * sub$splitratio
    sub$wood = sub$wood * sub$splitratio
    sub$solid_fuel = sub$solid_fuel * sub$splitratio
    sub$heat_network = sub$heat_network * sub$splitratio
    sub$other_central_heating = sub$other_central_heating * sub$splitratio
    sub$two_types_no_renewable_energy = sub$two_types_no_renewable_energy * sub$splitratio
    sub$two_types_inc_renewable_energy = sub$two_types_inc_renewable_energy * sub$splitratio

    sub = sub[,c("LSOA21CD","year","bottled_gas",
                "oil","wood","solid_fuel",
                "heat_network","other_central_heating",
                "two_types_no_renewable_energy",
                 "two_types_inc_renewable_energy")]

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

  #ch_all = ch_all[,c("LSOA21CD","year","heating_other_emissions_total")]
  ch_all = dplyr::left_join(ch_all, population, by = c("LSOA21CD","year"))
  ch_all$heating_other_kgco2e_percap = ifelse(ch_all$all_ages != 0,
                                              ch_all$heating_other_emissions_total / ch_all$all_ages,
                                              0)

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

load_cental_heating_scotland_2011 = function(path = "../inputdata/gas_electric/scotland_2011_centralheating.csv"){

  ch = readr::read_csv(path)
  names(ch) = c("DataZone11","occupied_households",
                "no_central_heating","gas",
                "electric","oil",
                "solid_fuel","other",
                "two_or_more"  )

  ch = ch[ch$DataZone11 != "S92000003",]
  ch
}

load_cental_heating_scotland_2022 = function(path = "../inputdata/gas_electric/scotland_2022_centralheating.csv"){

  ch = readr::read_csv(path, skip = 9)
  names(ch) = c("Counting","LSOA21CD","heating" ,"Count","dud")
  ch = ch[,c("LSOA21CD","heating" ,"Count")]
  ch = ch[!is.na(ch$Count),]
  ch = ch[substr(ch$LSOA21CD,1,1) == "S",]
  ch = tidyr::pivot_wider(ch, names_from = "heating", values_from = "Count")
  names(ch) = c("LSOA21CD","no_central_heating","gas",
                "electric","oil",
                "solid_fuel_exwood","other",
                "two_or_more","total")
  ch
}

central_heating_2011_to_2022_scotland = function(sub, lookup_dz_2011_22_pre){

  # Scotland
  lookup_dz_2011_22_pre = sf::st_drop_geometry(lookup_dz_2011_22_pre)
  # Share of the 2011 households
  lookup_dz_2011_22_pre = lookup_dz_2011_22_pre |>
    dplyr::group_by(DataZone) |>
    dplyr::mutate(splitshare = count / sum(count)) |>
    dplyr::ungroup()

  lookup_dz_2011_22_pre = lookup_dz_2011_22_pre[,c("DataZone","DataZone22","splitshare")]
  names(lookup_dz_2011_22_pre) = c("LSOA11CD","LSOA21CD","splitshare")

  sub2 = dplyr::left_join(lookup_dz_2011_22_pre, sub, by = c("LSOA11CD" = "DataZone11"))

  sub2 = sub2 |>
    dplyr::group_by(LSOA21CD) |>
    dplyr::summarise(occupied_households = round(sum(occupied_households * splitshare)),
                     no_central_heating  = round(sum(no_central_heating * splitshare)),
                     gas = round(sum(gas * splitshare)),
                     electric = round(sum(electric * splitshare)),
                     oil = round(sum(oil * splitshare)),
                     solid_fuel  = round(sum(solid_fuel  * splitshare)),
                     other = round(sum(other * splitshare)),
                     two_or_more = round(sum(two_or_more * splitshare))
                     )
  sub2


}
