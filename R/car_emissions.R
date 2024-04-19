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

  emissons_wide <- tidyr::pivot_wider(emissions_gp, id_cols = "LSOA",
                                      names_from = c("year"),
                                      values_from = c("total_cars","AvgCO2_cars","AvgAge_cars"))
  emissons_wide
}

car_emissions_to_21 = function(car_emissions_11, lsoa_11_21_tools) {


  names(car_emissions_11)[1] = "LSOA11CD"

  car_emissions_11_S = car_emissions_11[car_emissions_11$LSOA11CD %in% lsoa_11_21_tools$lookup_split$LSOA11CD,]
  car_emissions_11_M = car_emissions_11[car_emissions_11$LSOA11CD %in% lsoa_11_21_tools$lookup_merge$LSOA11CD,]
  car_emissions_11_U = car_emissions_11[car_emissions_11$LSOA11CD %in% lsoa_11_21_tools$lookup_unchanged$LSOA11CD,]

  #Unchanged
  car_emissions_11_U = dplyr::left_join(car_emissions_11_U, lsoa_11_21_tools$lookup_unchanged, by = "LSOA11CD")

  # Merge
  car_emissions_11_M = dplyr::left_join(car_emissions_11_M, lsoa_11_21_tools$lookup_merge, by = "LSOA11CD")
  car_emissions_11_M = dplyr::group_by(car_emissions_11_M, LSOA21CD)
  car_emissions_11_M = dplyr::summarise(car_emissions_11_M,
                                        AvgCO2_cars_2001 = weighted.mean(AvgCO2_cars_2001, total_cars_2001, na.rm = TRUE),
                                        AvgCO2_cars_2002 = weighted.mean(AvgCO2_cars_2002, total_cars_2002, na.rm = TRUE),
                                        AvgCO2_cars_2003 = weighted.mean(AvgCO2_cars_2003, total_cars_2003, na.rm = TRUE),
                                        AvgCO2_cars_2004 = weighted.mean(AvgCO2_cars_2004, total_cars_2004, na.rm = TRUE),
                                        AvgCO2_cars_2005 = weighted.mean(AvgCO2_cars_2005, total_cars_2005, na.rm = TRUE),
                                        AvgCO2_cars_2006 = weighted.mean(AvgCO2_cars_2006, total_cars_2006, na.rm = TRUE),
                                        AvgCO2_cars_2007 = weighted.mean(AvgCO2_cars_2007, total_cars_2007, na.rm = TRUE),
                                        AvgCO2_cars_2008 = weighted.mean(AvgCO2_cars_2008, total_cars_2008, na.rm = TRUE),
                                        AvgCO2_cars_2009 = weighted.mean(AvgCO2_cars_2009, total_cars_2009, na.rm = TRUE),
                                        AvgCO2_cars_2010 = weighted.mean(AvgCO2_cars_2010, total_cars_2010, na.rm = TRUE),
                                        AvgCO2_cars_2011 = weighted.mean(AvgCO2_cars_2011, total_cars_2011, na.rm = TRUE),
                                        AvgCO2_cars_2012 = weighted.mean(AvgCO2_cars_2012, total_cars_2012, na.rm = TRUE),
                                        AvgCO2_cars_2013 = weighted.mean(AvgCO2_cars_2013, total_cars_2013, na.rm = TRUE),
                                        AvgCO2_cars_2014 = weighted.mean(AvgCO2_cars_2014, total_cars_2014, na.rm = TRUE),
                                        AvgCO2_cars_2015 = weighted.mean(AvgCO2_cars_2015, total_cars_2015, na.rm = TRUE),
                                        AvgCO2_cars_2016 = weighted.mean(AvgCO2_cars_2016, total_cars_2016, na.rm = TRUE),
                                        AvgCO2_cars_2017 = weighted.mean(AvgCO2_cars_2017, total_cars_2017, na.rm = TRUE),
                                        AvgCO2_cars_2018 = weighted.mean(AvgCO2_cars_2018, total_cars_2018, na.rm = TRUE),
                                        AvgAge_cars_2001 = weighted.mean(AvgAge_cars_2001, total_cars_2001, na.rm = TRUE),
                                        AvgAge_cars_2002 = weighted.mean(AvgAge_cars_2002, total_cars_2002, na.rm = TRUE),
                                        AvgAge_cars_2003 = weighted.mean(AvgAge_cars_2003, total_cars_2003, na.rm = TRUE),
                                        AvgAge_cars_2004 = weighted.mean(AvgAge_cars_2004, total_cars_2004, na.rm = TRUE),
                                        AvgAge_cars_2005 = weighted.mean(AvgAge_cars_2005, total_cars_2005, na.rm = TRUE),
                                        AvgAge_cars_2006 = weighted.mean(AvgAge_cars_2006, total_cars_2006, na.rm = TRUE),
                                        AvgAge_cars_2007 = weighted.mean(AvgAge_cars_2007, total_cars_2007, na.rm = TRUE),
                                        AvgAge_cars_2008 = weighted.mean(AvgAge_cars_2008, total_cars_2008, na.rm = TRUE),
                                        AvgAge_cars_2009 = weighted.mean(AvgAge_cars_2009, total_cars_2009, na.rm = TRUE),
                                        AvgAge_cars_2010 = weighted.mean(AvgAge_cars_2010, total_cars_2010, na.rm = TRUE),
                                        AvgAge_cars_2011 = weighted.mean(AvgAge_cars_2011, total_cars_2011, na.rm = TRUE),
                                        AvgAge_cars_2012 = weighted.mean(AvgAge_cars_2012, total_cars_2012, na.rm = TRUE),
                                        AvgAge_cars_2013 = weighted.mean(AvgAge_cars_2013, total_cars_2013, na.rm = TRUE),
                                        AvgAge_cars_2014 = weighted.mean(AvgAge_cars_2014, total_cars_2014, na.rm = TRUE),
                                        AvgAge_cars_2015 = weighted.mean(AvgAge_cars_2015, total_cars_2015, na.rm = TRUE),
                                        AvgAge_cars_2016 = weighted.mean(AvgAge_cars_2016, total_cars_2016, na.rm = TRUE),
                                        AvgAge_cars_2017 = weighted.mean(AvgAge_cars_2017, total_cars_2017, na.rm = TRUE),
                                        AvgAge_cars_2018 = weighted.mean(AvgAge_cars_2018, total_cars_2018, na.rm = TRUE),
                                        total_cars_2001 = sum(total_cars_2001,na.rm = TRUE),
                                        total_cars_2002 = sum(total_cars_2002,na.rm = TRUE),
                                        total_cars_2003 = sum(total_cars_2003,na.rm = TRUE),
                                        total_cars_2004 = sum(total_cars_2004,na.rm = TRUE),
                                        total_cars_2005 = sum(total_cars_2005,na.rm = TRUE),
                                        total_cars_2006 = sum(total_cars_2006,na.rm = TRUE),
                                        total_cars_2007 = sum(total_cars_2007,na.rm = TRUE),
                                        total_cars_2008 = sum(total_cars_2008,na.rm = TRUE),
                                        total_cars_2009 = sum(total_cars_2009,na.rm = TRUE),
                                        total_cars_2010 = sum(total_cars_2010,na.rm = TRUE),
                                        total_cars_2011 = sum(total_cars_2011,na.rm = TRUE),
                                        total_cars_2012 = sum(total_cars_2012,na.rm = TRUE),
                                        total_cars_2013 = sum(total_cars_2013,na.rm = TRUE),
                                        total_cars_2014 = sum(total_cars_2014,na.rm = TRUE),
                                        total_cars_2015 = sum(total_cars_2015,na.rm = TRUE),
                                        total_cars_2016 = sum(total_cars_2016,na.rm = TRUE),
                                        total_cars_2017 = sum(total_cars_2017,na.rm = TRUE),
                                        total_cars_2018 = sum(total_cars_2018,na.rm = TRUE)
                                        )
  car_emissions_11_M = dplyr::ungroup(car_emissions_11_M)

  #Split
  car_emissions_11_S = dplyr::left_join(lsoa_11_21_tools$lookup_split, car_emissions_11_S,
                                            by = "LSOA11CD", relationship = "many-to-many")
  car_emissions_11_S = as.data.frame(car_emissions_11_S)
  for(i in 4:57){
    car_emissions_11_S[i] = car_emissions_11_S[,i ,drop = TRUE] * car_emissions_11_S$pop_ratio
  }

  nms = c("LSOA21CD",paste0("total_cars_",2001:2018),paste0("AvgCO2_cars_",2001:2018),paste0("AvgAge_cars_",2001:2018))

  car_emissions_11_S = car_emissions_11_S[,nms]
  car_emissions_11_M = car_emissions_11_M[,nms]
  car_emissions_11_U = car_emissions_11_U[,nms]

  final = rbind(car_emissions_11_S, car_emissions_11_M, car_emissions_11_U)
  final



}



calculate_car_emissions = function(car_km_lsoa, car_emissions, population){

  #TODO: Get better CO2 per km estimate for 2019 - 2023
  # Email sent to DfT
  # Alt option look at MOT data
  lsoa = dplyr::left_join(car_km_lsoa, car_emissions, by = "LSOA21CD")
  for(i in 2010:2018){
    lsoa[paste0("car_emissions_",i)] = lsoa[paste0("car_km_",i-2000)] * lsoa[paste0("AvgCO2_cars_",i)] / 1000
  }
  for(i in 2010:2018){
    lsoa[paste0("van_emissions_",i)] = lsoa[paste0("van_km_",i-2000)] * lsoa[paste0("AvgCO2_cars_",i)] / 1000
  }
  for(i in 2019:2023){
    lsoa[paste0("car_emissions_",i)] = lsoa[paste0("car_km_",i-2000)] * lsoa[paste0("AvgCO2_cars_",2018)] / 1000
  }
  for(i in 2019:2023){
    lsoa[paste0("van_emissions_",i)] = lsoa[paste0("van_km_",i-2000)] * lsoa[paste0("AvgCO2_cars_",2018)] / 1000
  }
  lsoa = lsoa[,c("LSOA21CD",paste0("car_emissions_",2010:2023),paste0("van_emissions_",2010:2023))]

  population = population[,c("year","LSOA21CD","all_ages")]
  population = tidyr::pivot_wider(population, names_from = "year", values_from = "all_ages")

  lsoa = dplyr::left_join(lsoa, population, by = "LSOA21CD")

  for(i in 2010:2021){
    lsoa[paste0("car_emissions_percap_",i)] = lsoa[paste0("car_emissions_",i)] / lsoa[as.character(i)]
  }
  for(i in 2010:2021){
    lsoa[paste0("van_emissions_percap_",i)] = lsoa[paste0("van_emissions_",i)] / lsoa[as.character(i)]
  }
  lsoa = lsoa[,c("LSOA21CD",paste0("car_emissions_percap_",2010:2021),paste0("van_emissions_percap_",2010:2021))]
  lsoa

}

