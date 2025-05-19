load_car_emissions <- function(path){
  dir.create(file.path(tempdir(),"car_emissions"))
  unzip(path, exdir = file.path(tempdir(),"car_emissions"))
  emissions <- readr::read_csv(file.path(tempdir(),"car_emissions","Historical_Car_Emissions_LSOA.csv"))
  unlink(file.path(tempdir(),"car_emissions"), recursive = TRUE)

  emissions <- emissions[,c("year","LSOA","fuel","AllCars","AvgCO2","AvgAge")]
  emissions_gp <- dplyr::group_by(emissions, year, LSOA)
  emissions_gp <- dplyr::summarise(emissions_gp,
               total_cars = sum(AllCars, na.rm = TRUE),
              AvgCO2_cars = weighted.mean(AvgCO2, AllCars, na.rm = TRUE),
              AvgAge_cars = weighted.mean(AvgAge, AllCars, na.rm = TRUE))

  emissions_gp$AvgCO2_cars <- round(emissions_gp$AvgCO2_cars, 1)
  emissions_gp$AvgAge_cars <- round(emissions_gp$AvgAge_cars, 1)

  # emissons_wide <- tidyr::pivot_wider(emissions_gp, id_cols = "LSOA",
  #                                     names_from = c("year"),
  #                                     values_from = c("total_cars","AvgCO2_cars","AvgAge_cars"))
  # emissons_wide

  emissions_gp
}

car_emissions_to_21 = function(car_emissions_11, lsoa_11_21_tools) {


  names(car_emissions_11)[names(car_emissions_11) == "LSOA"] = "LSOA11CD"
  car_emissions_11 = car_emissions_11[car_emissions_11$year > 2001,] # No 2001 pop data

  car_emissions_11_S = car_emissions_11[car_emissions_11$LSOA11CD %in% lsoa_11_21_tools$lookup_split$LSOA11CD,]
  car_emissions_11_M = car_emissions_11[car_emissions_11$LSOA11CD %in% lsoa_11_21_tools$lookup_merge$LSOA11CD,]
  car_emissions_11_U = car_emissions_11[car_emissions_11$LSOA11CD %in% lsoa_11_21_tools$lookup_unchanged$LSOA11CD,]

  #Unchanged
  car_emissions_11_U = dplyr::left_join(car_emissions_11_U, lsoa_11_21_tools$lookup_unchanged, by = "LSOA11CD")

  # Merge
  car_emissions_11_M = dplyr::left_join(car_emissions_11_M, lsoa_11_21_tools$lookup_merge, by = "LSOA11CD")
  car_emissions_11_M = dplyr::group_by(car_emissions_11_M, LSOA21CD, year)
  car_emissions_11_M = dplyr::summarise(car_emissions_11_M,
                                        AvgCO2_cars = weighted.mean(AvgCO2_cars, total_cars, na.rm = TRUE),
                                        AvgAge_cars = weighted.mean(AvgAge_cars, total_cars, na.rm = TRUE),
                                        total_cars = sum(total_cars,na.rm = TRUE)
                                        )
  car_emissions_11_M = dplyr::ungroup(car_emissions_11_M)


  # Split
  lookup_split = lsoa_11_21_tools$lookup_split
  lookup_split = lookup_split[,c("LSOA11CD","LSOA21CD","year","household_ratio")]
  lookup_split = lookup_split[lookup_split$year %in% 2002:2018,]

  car_emissions_11_S = dplyr::left_join(lookup_split, car_emissions_11_S,
                                    by = c("LSOA11CD", "year"),
                                    relationship = "many-to-many")


  car_emissions_11_S$total_cars = car_emissions_11_S$total_cars * car_emissions_11_S$household_ratio

  #TODO: no obvious way to split up average age and average CO2

  nms = c("LSOA21CD","year","total_cars","AvgCO2_cars","AvgAge_cars")

  car_emissions_11_S = car_emissions_11_S[,nms]
  car_emissions_11_M = car_emissions_11_M[,nms]
  car_emissions_11_U = car_emissions_11_U[,nms]

  final = rbind(car_emissions_11_S, car_emissions_11_M, car_emissions_11_U)
  final



}

car_emissions_post2018 = function(car_emissions_11,
                                  vehicle_registrations_21,
                                  ulev_registrations_21) {

  ulev_registrations_21 = ulev_registrations_21[,!grepl("Disposal",names(ulev_registrations_21))]
  #ev_registrations_21 = ev_registrations_21[,!grepl("Disposal",names(ev_registrations_21))]
  # EV and ULEV basically the same, difference dute to estiamteing small values
  vehicle_registrations_21 = vehicle_registrations_21[,!grepl("Disposal",names(vehicle_registrations_21))]
  vehicle_registrations_21 = vehicle_registrations_21[,!grepl("SORN",names(vehicle_registrations_21))]

  vehicle_registrations_21$all_vehicles = rowSums(vehicle_registrations_21[,
    c("Cars_Company_Licensed","Cars_Private_Licensed",
      "Other body types_Company_Licensed","Other body types_Private_Licensed",
      "Motorcycles_Company_Licensed","Motorcycles_Private_Licensed")])

  # ev_registrations_21$BEV = rowSums(ev_registrations_21[,c("Battery electric_Company","Battery electric_Private")],
  #                                   na.rm = TRUE)
  # ev_registrations_21$PHEV = rowSums(ev_registrations_21[,c("Plug-in hybrid electric (petrol)_Company","Plug-in hybrid electric (petrol)_Private",
  #                                                               "Plug-in hybrid electric (diesel)_Company","Plug-in hybrid electric (diesel)_Private")],
  #                                    na.rm = TRUE)
  # ev_registrations_21$REEV = rowSums(ev_registrations_21[,c("Range extended electric_Private","Range extended electric_Company")],
  #                                    na.rm = TRUE)
  #
  ulev_registrations_21$PHEV = rowSums(ulev_registrations_21[,c("Plug-in hybrid electric (petrol)_Company","Plug-in hybrid electric (petrol)_Private",
                                                            "Plug-in hybrid electric (diesel)_Company","Plug-in hybrid electric (diesel)_Private")],
                                     na.rm = TRUE)
  ulev_registrations_21$BEV = rowSums(ulev_registrations_21[,c("Battery electric_Company","Battery electric_Private")],
                                    na.rm = TRUE)
  ulev_registrations_21$REEV = rowSums(ulev_registrations_21[,c("Range extended electric_Private","Range extended electric_Company")],
                                     na.rm = TRUE)
  ulev_registrations_21$HEV = rowSums(ulev_registrations_21[,c("Hybrid electric (diesel)_Company","Hybrid electric (diesel)_Private",
                                                               "Hybrid electric (petrol)_Private","Hybrid electric (petrol)_Company")],
                                       na.rm = TRUE)
  ulev_registrations_21$ULEV_ICE = rowSums(ulev_registrations_21[,c("Diesel_Private","Diesel_Company","Petrol_Company","Petrol_Private")],
                                      na.rm = TRUE)
  ulev_registrations_21$fuelcell = rowSums(ulev_registrations_21[,c("Fuel cell electric_Company","Fuel cell electric_Private")],
                                           na.rm = TRUE)


  vehicle_registrations_21 = vehicle_registrations_21[,c("LSOA21CD","year","all_vehicles")]
  ulev_registrations_21 = ulev_registrations_21[,c("LSOA21CD","year","PHEV","BEV","REEV","HEV","ULEV_ICE","fuelcell" )]
  #ev_registrations_21 = ev_registrations_21[,c("LSOA21CD","year","BEV","PHEV","REEV" )]
  #names(ev_registrations_21) = c("LSOA21CD","year","ev_BEV","ev_PHEV","ev_REEV")

  vehicle_registrations_21 = dplyr::left_join(vehicle_registrations_21,ulev_registrations_21, by = c("LSOA21CD","year"))
  #foo = dplyr::left_join(foo,ev_registrations_21, by = c("LSOA21CD","year"))

  for(i in 1:ncol(vehicle_registrations_21)){
    x = vehicle_registrations_21[,i]
    if(is.numeric(x)){
      x[is.na(x)] = 0
      vehicle_registrations_21[,i] = x
    }
  }

  # BEVs Fuel Cell are 0 tailpipe, other ULEV < 75g CO2 per km
  emiss_2018 = car_emissions_11[car_emissions_11$year == 2018,]
  vehicle_registrations_21 = vehicle_registrations_21[vehicle_registrations_21$year > 2017,]
  vehicle_registrations_21$share_0g = (vehicle_registrations_21$BEV + vehicle_registrations_21$fuelcell) / vehicle_registrations_21$all_vehicles
  vehicle_registrations_21$share_75g = (vehicle_registrations_21$PHEV +
                                          vehicle_registrations_21$REEV +
                                          vehicle_registrations_21$HEV +
                                          vehicle_registrations_21$ULEV_ICE) / vehicle_registrations_21$all_vehicles

  vehicle_registrations_21$share_0g[is.infinite(vehicle_registrations_21$share_0g)] = 0
  vehicle_registrations_21$share_75g[is.infinite(vehicle_registrations_21$share_75g)] = 0

  vehicle_registrations_21$share_0g[is.nan(vehicle_registrations_21$share_0g)] = 0
  vehicle_registrations_21$share_75g[is.nan(vehicle_registrations_21$share_75g)] = 0

  vehicle_registrations_21$share_full = 1 - vehicle_registrations_21$share_0g - vehicle_registrations_21$share_75g
  # Work out 2018 share excluding ULEV
  emiss_2018 = dplyr::left_join(emiss_2018,
                         vehicle_registrations_21[vehicle_registrations_21$year == 2018,],
                         by = c("LSOA21CD","year"))
  emiss_2018$AvgCO2_cars_nonULEV = (emiss_2018$AvgCO2_cars - (75 * emiss_2018$share_75g)) / emiss_2018$share_full
  emiss_2018 = emiss_2018[,c("LSOA21CD","AvgCO2_cars_nonULEV")]

  foo = dplyr::left_join(vehicle_registrations_21, emiss_2018, by = "LSOA21CD")
  foo$AvgCO2 = (foo$AvgCO2_cars_nonULEV * foo$share_full) +
    (75 * foo$share_75g)

  # No (old) data for Scotland
  foo$AvgCO2 = ifelse(is.na(foo$AvgCO2), (median(foo$AvgCO2_cars_nonULEV, na.rm = TRUE) * foo$share_full) +
    (75 * foo$share_75g), foo$AvgCO2)

  foo = foo[foo$year > 2018,]
  foo = foo[,c("LSOA21CD","year","all_vehicles","AvgCO2")]
  foo$AvgAge = NA

  names(car_emissions_11) = c("LSOA21CD","year","all_vehicles","AvgCO2","AvgAge")

  final = rbind(car_emissions_11, foo)
  final = final[order(final$LSOA21CD, final$year),]
  final

  # Checks
  # library(ggplot2)
  # chk = final[final$LSOA21CD == "E01032669",]
  # ggplot(chk, aes(x = year, y = AvgCO2)) +
  #   geom_line() +
  #   ylim(0,NA)


}



calculate_car_emissions = function(car_km_lsoa_21, car_emissions_perkm, population){


  lsoa = dplyr::left_join(car_km_lsoa_21, car_emissions_perkm, by = c("LSOA21CD","year"))

  lsoa$car_emissions = lsoa$Cars_Private * lsoa$AvgCO2
  lsoa$van_emissions = lsoa$vans_Private * lsoa$AvgCO2
  lsoa$company_bike_emissions = lsoa$company_bike * lsoa$AvgCO2

  population = population[,c("year","LSOA21CD","all_ages")]

  lsoa = lsoa[lsoa$year <= max(population$year),]

  lsoa = dplyr::left_join(lsoa, population, by = c("LSOA21CD","year"))

  lsoa$car_emissions_percap = ifelse(lsoa$all_ages == 0, 0, lsoa$car_emissions / lsoa$all_ages)
  lsoa$van_emissions_percap = ifelse(lsoa$all_ages == 0, 0, lsoa$van_emissions / lsoa$all_ages)
  lsoa$company_bike_emissions_percap = ifelse(lsoa$all_ages == 0, 0, lsoa$company_bike_emissions / lsoa$all_ages) # This is company all types and private bikes

  #TODO: Missing emissions rate for Scotland pre 2019 and Scotland population in 2022
  lsoa = lsoa[,c("LSOA21CD","year","car_emissions_percap","van_emissions_percap","company_bike_emissions_percap")]
  lsoa

}

# DfT don't give average emissions anymore
# Use EV / UVEL to make assumptions
estimate_car_meissions_post2018 = function(){

}
