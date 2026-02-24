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
                                  vehicle_registrations,
                                  ulev_registrations) {

  ulev_registrations = ulev_registrations[,!grepl("DISPOSAL",names(ulev_registrations))]

  vehicle_registrations = vehicle_registrations[,!grepl("DISPOSAL",names(vehicle_registrations))]
  vehicle_registrations = vehicle_registrations[,!grepl("SORN",names(vehicle_registrations))]

  vehicle_registrations$year = as.integer(gsub(" Q1","",vehicle_registrations$quarter))
  ulev_registrations$year = as.integer(gsub(" Q1","",ulev_registrations$quarter))


  vehicle_registrations$all_vehicles = rowSums(vehicle_registrations[,
    c("Cars_COMPANY_Licensed","Cars_PRIVATE_Licensed",
      "Other vehicles_COMPANY_Licensed","Other vehicles_PRIVATE_Licensed",
      "Motorcycles_COMPANY_Licensed","Motorcycles_PRIVATE_Licensed")])


  ulev_registrations$PHEV = rowSums(ulev_registrations[,
              c("PLUG-IN HYBRID ELECTRIC (PETROL)_COMPANY",
                "PLUG-IN HYBRID ELECTRIC (PETROL)_PRIVATE",
                "PLUG-IN HYBRID ELECTRIC (DIESEL)_COMPANY",
                "PLUG-IN HYBRID ELECTRIC (DIESEL)_PRIVATE")],
                                     na.rm = TRUE)
  ulev_registrations$BEV = rowSums(ulev_registrations[,
                c("BATTERY ELECTRIC_COMPANY","BATTERY ELECTRIC_PRIVATE")],
                                    na.rm = TRUE)
  ulev_registrations$REEV = rowSums(ulev_registrations[,
                c("RANGE EXTENDED ELECTRIC_PRIVATE","RANGE EXTENDED ELECTRIC_COMPANY")],
                                     na.rm = TRUE)
  ulev_registrations$HEV = rowSums(ulev_registrations[,
                c("HYBRID ELECTRIC (DIESEL)_COMPANY",
                  "HYBRID ELECTRIC (DIESEL)_PRIVATE",
                  "HYBRID ELECTRIC (PETROL)_PRIVATE",
                  "HYBRID ELECTRIC (PETROL)_COMPANY")],
                                       na.rm = TRUE)
  ulev_registrations$ULEV_ICE = rowSums(ulev_registrations[,
                c("DIESEL_PRIVATE","DIESEL_COMPANY","PETROL_COMPANY",
                  "PETROL_PRIVATE")],na.rm = TRUE)
  ulev_registrations$fuelcell = rowSums(ulev_registrations[,
                c("FUEL CELLS_COMPANY","FUEL CELLS_PRIVATE")],
                                           na.rm = TRUE)


  vehicle_registrations = vehicle_registrations[,c("LSOA21CD","year","all_vehicles")]
  ulev_registrations = ulev_registrations[,c("LSOA21CD","year","PHEV","BEV","REEV","HEV","ULEV_ICE","fuelcell" )]


  vehicle_registrations = dplyr::left_join(vehicle_registrations,ulev_registrations, by = c("LSOA21CD","year"))


  for(i in 1:ncol(vehicle_registrations)){
    x = vehicle_registrations[,i]
    if(is.numeric(x)){
      x[is.na(x)] = 0
      vehicle_registrations[,i] = x
    }
  }

  # BEVs Fuel Cell are 0 tailpipe, other ULEV < 75g CO2 per km
  emiss_2018 = car_emissions_11[car_emissions_11$year == 2018,]

  #vehicle_registrations = vehicle_registrations[vehicle_registrations$year > 2017,]
  # Do for all years as missing Scotland data
  vehicle_registrations$share_0g = (vehicle_registrations$BEV + vehicle_registrations$fuelcell) / vehicle_registrations$all_vehicles
  vehicle_registrations$share_75g = (vehicle_registrations$PHEV +
                                          vehicle_registrations$REEV +
                                          vehicle_registrations$HEV +
                                          vehicle_registrations$ULEV_ICE) / vehicle_registrations$all_vehicles

  vehicle_registrations$share_0g[is.infinite(vehicle_registrations$share_0g)] = 0
  vehicle_registrations$share_75g[is.infinite(vehicle_registrations$share_75g)] = 0

  vehicle_registrations$share_0g[is.nan(vehicle_registrations$share_0g)] = 0
  vehicle_registrations$share_75g[is.nan(vehicle_registrations$share_75g)] = 0

  vehicle_registrations$share_full = 1 - vehicle_registrations$share_0g - vehicle_registrations$share_75g

  # Work out 2018 share excluding ULEV
  emiss_2018 = dplyr::left_join(emiss_2018,
                         vehicle_registrations[vehicle_registrations$year == 2018,],
                         by = c("LSOA21CD","year"))
  emiss_2018$AvgCO2_cars_nonULEV = (emiss_2018$AvgCO2_cars - (75 * emiss_2018$share_75g)) / emiss_2018$share_full
  emiss_2018 = emiss_2018[,c("LSOA21CD","AvgCO2_cars_nonULEV")]

  foo = dplyr::left_join(vehicle_registrations, emiss_2018, by = "LSOA21CD")

  #Fill in missing data for scotland with averages
  annualemissis = car_emissions_11 |>
    dplyr::group_by(year) |>
    dplyr::summarise(natAvgCO2_cars_nonULEV = median(AvgCO2_cars, na.rm = TRUE))

  foo = dplyr::left_join(foo, annualemissis, by = "year")
  foo$AvgCO2_cars_nonULEV = ifelse(is.na(foo$AvgCO2_cars_nonULEV),foo$natAvgCO2_cars_nonULEV,foo$AvgCO2_cars_nonULEV)

  # Calculate Average COs
  foo$AvgCO2 = (foo$AvgCO2_cars_nonULEV * foo$share_full) +
    (75 * foo$share_75g)

  # No (old) data for Scotland, so extrapolate forward
  foo$AvgCO2 = ifelse(is.na(foo$AvgCO2), (median(foo$AvgCO2_cars_nonULEV[foo$year == 2018], na.rm = TRUE) * foo$share_full) +
    (75 * foo$share_75g), foo$AvgCO2)

  foo = foo[,c("LSOA21CD","year","all_vehicles","AvgCO2")]
  foo$AvgAge = NA

  names(car_emissions_11) = c("LSOA21CD","year","all_vehicles","AvgCO2","AvgAge")

  # Bind Real Data EW 2002-2018, estimated ESW 2019-2024, estimated Scotland 2010-2018


  final = rbind(car_emissions_11, foo[foo$year >= 2019,], foo[foo$year <= 2018 & substr(foo$LSOA21CD,1,1) == "S",])
  final = final[order(final$LSOA21CD, final$year),]
  final

  # Checks
  # library(ggplot2)
  # chk = final[final$LSOA21CD == "E01032669",]
  # ggplot(chk, aes(x = year, y = AvgCO2)) +
  #   geom_line() +
  #   ylim(0,NA)


}



calculate_car_emissions = function(car_km_lsoa_21, car_emissions_perkm, population, years = 2010:2021){

  population = population[population$year %in% years,]
  car_km_lsoa_21 = car_km_lsoa_21[car_km_lsoa_21$year %in% years,]
  car_emissions_perkm = car_emissions_perkm[car_emissions_perkm$year %in% years,]

  lsoa = dplyr::left_join(car_km_lsoa_21, car_emissions_perkm, by = c("LSOA21CD","year"))

  lsoa$car_emissions = lsoa$car_km  * lsoa$AvgCO2 / 1000
  lsoa$van_emissions = lsoa$van_km * lsoa$AvgCO2 / 1000
  lsoa$company_bike_emissions = lsoa$company_km * lsoa$AvgCO2 / 1000

  population = population[,c("year","LSOA21CD","all_ages")]

  lsoa = dplyr::left_join(lsoa, population, by = c("LSOA21CD","year"))

  lsoa$car_emissions_percap = ifelse(lsoa$all_ages == 0, 0, lsoa$car_emissions / lsoa$all_ages)
  lsoa$van_emissions_percap = ifelse(lsoa$all_ages == 0, 0, lsoa$van_emissions / lsoa$all_ages)
  lsoa$company_bike_emissions_percap = ifelse(lsoa$all_ages == 0, 0, lsoa$company_bike_emissions / lsoa$all_ages) # This is company all types and private bikes


  lsoa = lsoa[,c("LSOA21CD","year",
                 "car_emissions","van_emissions","company_bike_emissions",
                 "car_km","van_km","company_km",
                 "car_emissions_percap","van_emissions_percap","company_bike_emissions_percap")]
  lsoa

}


