read_motoring_along = function(path = file.path(parameters$path_secure_data,"CREDS Data/Tim Share/From Tim/MOT Data RACv9.3")){

  mot11 <- read.csv(file.path(path, "MOT Data RACv9.3 LSOAoutputs_2011.csv"))
  mot10 <- read.csv(file.path(path, "MOT Data RACv9.3 LSOAoutputs_2010.csv"))
  mot09 <- read.csv(file.path(path, "MOT Data RACv9.3 LSOAoutputs_2009.csv"))

  names(mot11) <- c("LSOA11", "cars_total","cars_miles","pu5k","p5_12k","po12k","age_av","miles_av_u3",
                    "miles_av_o13","pcars_diesel","pmiles_diesel","vans_total","vans_miles",
                    "pmiles_car","pmiles_vans","cars_percap","miles_percap")
  names(mot10) <- c("LSOA11", "cars_total","cars_miles","pu5k","p5_12k","po12k","age_av","miles_av_u3",
                    "miles_av_o13","pcars_diesel","pmiles_diesel","vans_total","vans_miles",
                    "pmiles_car","pmiles_vans","cars_percap","miles_percap")
  names(mot09) <- c("LSOA11", "cars_total","cars_miles","pu5k","p5_12k","po12k","age_av","miles_av_u3",
                    "miles_av_o13","pcars_diesel","pmiles_diesel","vans_total","vans_miles",
                    "pmiles_car","pmiles_vans","cars_percap","miles_percap")
  mot11 <- mot11[,c("LSOA11","cars_total","cars_miles","vans_total","vans_miles")]
  mot10 <- mot10[,c("LSOA11","cars_total","cars_miles","vans_total","vans_miles")]
  mot09 <- mot09[,c("LSOA11","cars_total","cars_miles","vans_total","vans_miles")]

  names(mot11) <- c("LSOA11","cars_total_11","car_miles_11","vans_total_11","vans_miles_11")
  names(mot10) <- c("LSOA11","cars_total_10","car_miles_10","vans_total_10","vans_miles_10")
  names(mot09) <- c("LSOA11","cars_total_09","car_miles_09","vans_total_09","vans_miles_09")

  mot_all <- dplyr::left_join(mot09, mot10, by = "LSOA11")
  mot_all <- dplyr::left_join(mot_all, mot11, by = "LSOA11")

  mot_all$car_km_09 <- round(mot_all$car_miles_09  * 1.60934,1)
  mot_all$car_km_10 <- round(mot_all$car_miles_10  * 1.60934,1)
  mot_all$car_km_11 <- round(mot_all$car_miles_11  * 1.60934,1)

  mot_all$van_km_09 <- round(mot_all$vans_miles_09  * 1.60934,1)
  mot_all$van_km_10 <- round(mot_all$vans_miles_10  * 1.60934,1)
  mot_all$van_km_11 <- round(mot_all$vans_miles_11  * 1.60934,1)

  mot_all <- mot_all[,c("LSOA11",
                        "cars_total_09","cars_total_10","cars_total_11",
                        "vans_total_09","vans_total_10","vans_total_11",
                        "car_km_09","car_km_10","car_km_11",
                        "van_km_09","van_km_10","van_km_11")]

  mot_all

}
