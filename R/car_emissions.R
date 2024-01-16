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
