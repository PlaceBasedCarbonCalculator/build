read_mot_km_pc = function(path = file.path(parameters$path_secure_data,"CARS/Anoymised MOT/clean/postcode_total_vkm_2005_2023.Rds")){
  vkm = readRDS(path)
  vkm
}

# extraplote_car_km_trends = function(car_km_pc, car_km_2009_2011, centroids_lsoa11,
#                                     centroids_dz11, population, car_emissions_perkm,
#                                     vehicle_registrations){
#   # Combine Scot and EW
#   centroids_lsoa11 = rbind(centroids_lsoa11, centroids_dz11)
#
#   # Total scaling fo account for difference between datasets
#   #sum(car_km_pc$`2011`, na.rm = T) / sum(car_km_2009_2011$car_km_11) # 1.342153
#
#   pc_geom = car_km_pc[,"PC_AREA"]
#   pc_geom = sf::st_transform(pc_geom, 27700)
#   car_km_pc = sf::st_drop_geometry(car_km_pc)
#
#   #TODO: Update to 2023
#   car_km_pc$baseline <- car_km_pc$`2011`
#   car_km_pc[as.character(2005:2023)] <- lapply(car_km_pc[as.character(2005:2023)], function(x){
#     x <- x/car_km_pc$baseline * 1.342153
#     x
#   })
#   #mot_postcode_baseline <- as.data.frame(mot_postcode_baseline)
#   #names(mot_postcode_baseline) <- paste0("base_",names(mot_postcode_baseline))
#   #mot_postcode <- cbind(mot_postcode, mot_postcode_baseline)
#
#   # LSOA data is fluctuating from 2009 to 2011
#   # 1.0448, 1.02517, 1 (mean)
#   # Postcode data is increasing
#   # 1.0757, 1.120, 1.155 (mean)
#   summary(car_km_2009_2011$vans_total_09 / car_km_2009_2011$vans_total_11)
#   summary(car_km_pc$`2009`)
#   summary(car_km_2009_2011$vans_total_10 / car_km_2009_2011$vans_total_11)
#   summary(car_km_pc$`2010`)
#
#   centroids_lsoa11 <- sf::st_join(centroids_lsoa11, pc_geom)
#   centroids_lsoa11 <- sf::st_drop_geometry(centroids_lsoa11)
#   centroids_lsoa11 <- centroids_lsoa11[,c("LSOA11CD","PC_AREA")]
#
#   car_km_2009_2011 <- dplyr::left_join(car_km_2009_2011, centroids_lsoa11, by = c("LSOA11" = "LSOA11CD"))
#   #No MOT tests done in City of London (EC & WC)
#   #So use us neighbouring W as example
#   car_km_2009_2011$PC_AREA[car_km_2009_2011$PC_AREA %in% c("EC","WC")] = "W"
#   car_km_pc <- car_km_pc[,c("PC_AREA",as.character(2006:2023))]
#
#   car_km_2009_2011 <- dplyr::left_join(car_km_2009_2011, car_km_pc, by = c("PC_AREA" = "PC_AREA"))
#
#   car_km_2009_2011$car_km_11_orig <- car_km_2009_2011$car_km_11
#   car_km_2009_2011$van_km_11_orig <- car_km_2009_2011$van_km_11
#
#   car_km_2009_2011$car_km_09 <- car_km_2009_2011$car_km_11_orig * car_km_2009_2011$`2009`
#   car_km_2009_2011$car_km_10 <- car_km_2009_2011$car_km_11_orig * car_km_2009_2011$`2010`
#   car_km_2009_2011$car_km_11 <- car_km_2009_2011$car_km_11_orig * car_km_2009_2011$`2011`
#   car_km_2009_2011$car_km_12 <- car_km_2009_2011$car_km_11_orig * car_km_2009_2011$`2012`
#   car_km_2009_2011$car_km_13 <- car_km_2009_2011$car_km_11_orig * car_km_2009_2011$`2013`
#   car_km_2009_2011$car_km_14 <- car_km_2009_2011$car_km_11_orig * car_km_2009_2011$`2014`
#   car_km_2009_2011$car_km_15 <- car_km_2009_2011$car_km_11_orig * car_km_2009_2011$`2015`
#   car_km_2009_2011$car_km_16 <- car_km_2009_2011$car_km_11_orig * car_km_2009_2011$`2016`
#   car_km_2009_2011$car_km_17 <- car_km_2009_2011$car_km_11_orig * car_km_2009_2011$`2017`
#   car_km_2009_2011$car_km_18 <- car_km_2009_2011$car_km_11_orig * car_km_2009_2011$`2018`
#   car_km_2009_2011$car_km_19 <- car_km_2009_2011$car_km_11_orig * car_km_2009_2011$`2019`
#   car_km_2009_2011$car_km_20 <- car_km_2009_2011$car_km_11_orig * car_km_2009_2011$`2020`
#   car_km_2009_2011$car_km_21 <- car_km_2009_2011$car_km_11_orig * car_km_2009_2011$`2021`
#   car_km_2009_2011$car_km_22 <- car_km_2009_2011$car_km_11_orig * car_km_2009_2011$`2022`
#   car_km_2009_2011$car_km_23 <- car_km_2009_2011$car_km_11_orig * car_km_2009_2011$`2023`
#
#   car_km_2009_2011$van_km_09 <- car_km_2009_2011$van_km_11_orig * car_km_2009_2011$`2009`
#   car_km_2009_2011$van_km_10 <- car_km_2009_2011$van_km_11_orig * car_km_2009_2011$`2010`
#   car_km_2009_2011$van_km_11 <- car_km_2009_2011$van_km_11_orig * car_km_2009_2011$`2011`
#   car_km_2009_2011$van_km_12 <- car_km_2009_2011$van_km_11_orig * car_km_2009_2011$`2012`
#   car_km_2009_2011$van_km_13 <- car_km_2009_2011$van_km_11_orig * car_km_2009_2011$`2013`
#   car_km_2009_2011$van_km_14 <- car_km_2009_2011$van_km_11_orig * car_km_2009_2011$`2014`
#   car_km_2009_2011$van_km_15 <- car_km_2009_2011$van_km_11_orig * car_km_2009_2011$`2015`
#   car_km_2009_2011$van_km_16 <- car_km_2009_2011$van_km_11_orig * car_km_2009_2011$`2016`
#   car_km_2009_2011$van_km_17 <- car_km_2009_2011$van_km_11_orig * car_km_2009_2011$`2017`
#   car_km_2009_2011$van_km_18 <- car_km_2009_2011$van_km_11_orig * car_km_2009_2011$`2018`
#   car_km_2009_2011$van_km_19 <- car_km_2009_2011$van_km_11_orig * car_km_2009_2011$`2019`
#   car_km_2009_2011$van_km_20 <- car_km_2009_2011$van_km_11_orig * car_km_2009_2011$`2020`
#   car_km_2009_2011$van_km_21 <- car_km_2009_2011$van_km_11_orig * car_km_2009_2011$`2021`
#   car_km_2009_2011$van_km_22 <- car_km_2009_2011$van_km_11_orig * car_km_2009_2011$`2022`
#   car_km_2009_2011$van_km_23 <- car_km_2009_2011$van_km_11_orig * car_km_2009_2011$`2023`
#
#   car_km_2009_2011 <- car_km_2009_2011[,c("LSOA11","vans_total_09","vans_total_10","vans_total_11",
#                                   paste0("van_km_",10:23),
#                                   paste0("car_km_",10:23))]
#
#   car_km_2009_2011
#
#
#
# }



extraplote_car_km_trends2 = function(car_km_pc, car_km_2009_2011, centroids_lsoa21,
                                    centroids_dz11,
                                    vehicle_registrations_21, lookup_lsoa_2011_21){


  pc_geom = car_km_pc[,"PC_AREA"]
  pc_geom = sf::st_transform(pc_geom, 27700)
  car_km_pc = sf::st_drop_geometry(car_km_pc)
  car_km_pc = car_km_pc[,c("PC_AREA",2010:2023)]

  car_km_2009_2011$vans_total_11[is.na(car_km_2009_2011$vans_total_11)] = 0
  car_km_2009_2011$van_km_11[is.na(car_km_2009_2011$van_km_11)] = 0


  # Combine Scot and EW
  centroids_dz11 = centroids_dz11[,c("LSOA11CD","geometry")]
  names(centroids_dz11) =  c("LSOA21CD","geometry")
  centroids_lsoa21 = rbind(centroids_lsoa21, centroids_dz11)

  centroids_lsoa21 <- sf::st_join(centroids_lsoa21, pc_geom)
  centroids_lsoa21 <- sf::st_drop_geometry(centroids_lsoa21)
  centroids_lsoa21 <- centroids_lsoa21[,c("LSOA21CD","PC_AREA")]



  # MErged LSOA get data from one 2011 LSOA
  lookup_lsoa_2011_21 = lookup_lsoa_2011_21[,c("LSOA21CD","LSOA11CD")]
  lookup_lsoa_2011_21 = lookup_lsoa_2011_21[!duplicated(lookup_lsoa_2011_21$LSOA21CD),]

  centroids_dz11 = sf::st_drop_geometry(centroids_dz11)
  centroids_dz11$LSOA11CD = centroids_dz11$LSOA21CD

  lookup_lsoa_2011_21 = rbind(lookup_lsoa_2011_21, centroids_dz11)

  car_km_2009_2011 <- dplyr::left_join(lookup_lsoa_2011_21, car_km_2009_2011,
                                       by = c("LSOA11CD" = "LSOA11"))

  car_km_2009_2011 <- dplyr::left_join(car_km_2009_2011, centroids_lsoa21,
                                       by = c("LSOA21CD"))
  #No MOT tests done in City of London (EC & WC)
  #So use us neighbouring W as example
  car_km_2009_2011$PC_AREA[car_km_2009_2011$PC_AREA %in% c("EC","WC")] = "W"

  # Get vkm in 2011
  car_km_2009_2011 = car_km_2009_2011[,c("LSOA21CD","PC_AREA","cars_total_11","vans_total_11","car_km_11","van_km_11")]
  car_km_2009_2011$private_vehicles_total_11 = car_km_2009_2011$cars_total_11 +  car_km_2009_2011$vans_total_11
  car_km_2009_2011$car_vkmyear_11 = car_km_2009_2011$car_km_11 / car_km_2009_2011$cars_total_11
  car_km_2009_2011$van_vkmyear_11 = car_km_2009_2011$van_km_11 / car_km_2009_2011$vans_total_11
  car_km_2009_2011$van_vkmyear_11[is.nan(car_km_2009_2011$van_vkmyear_11)] = mean(car_km_2009_2011$van_vkmyear_11, na.rm = TRUE) # Some places with 0 vans

  # Drop SORN an Disposal as won't do many (if any) driving
  vehicle_registrations_21 = vehicle_registrations_21[,!grepl("SORN",names(vehicle_registrations_21))]
  vehicle_registrations_21 = vehicle_registrations_21[,!grepl("Disposal",names(vehicle_registrations_21))]

  # Number of each vehicle type, renamed
  names(vehicle_registrations_21)  = gsub("_Licensed","",names(vehicle_registrations_21))
  vehicle_registrations_21$company_bike = rowSums(vehicle_registrations_21[,c("Cars_Company","Other body types_Company",
                                                                              "Motorcycles_Company","Motorcycles_Private")],
                                                  na.rm = TRUE)
  vehicle_registrations_21$all_vehicles = rowSums(vehicle_registrations_21[,c("Cars_Private","Other body types_Private","company_bike")],
                                                  na.rm = TRUE)
  vehicle_registrations_21 = vehicle_registrations_21[,c("LSOA21CD","year","Cars_Private",
                                                         "Other body types_Private","company_bike",
                                                         "all_vehicles")]
  names(vehicle_registrations_21) = gsub("Other body types","vans",names(vehicle_registrations_21))

  # Add on PC

  vehicle_registrations_21 = dplyr::left_join(vehicle_registrations_21, centroids_lsoa21, by = "LSOA21CD")
  vehicle_registrations_21$PC_AREA[vehicle_registrations_21$PC_AREA %in% c("EC","WC")] = "W"

  vr_pc_summary = dplyr::group_by(vehicle_registrations_21, PC_AREA, year)
  vr_pc_summary = dplyr::summarise(vr_pc_summary,
                                   Cars_Private = sum(Cars_Private),
                                   vans_Private = sum(vans_Private),
                                   company_bike = sum(company_bike),
                                   all_vehicles = sum(all_vehicles))

  vr_pc_summary$company_ratio = vr_pc_summary$company_bike /  vr_pc_summary$all_vehicles
  vr_pc_summary$car_ratio = vr_pc_summary$Cars_Private /  (vr_pc_summary$Cars_Private + vr_pc_summary$vans_Private)

  # Pivot Longer Postcode data

  names(car_km_pc) = gsub("^20","total_20",names(car_km_pc))
  car_km_pc_long = tidyr::pivot_longer(car_km_pc,
                                       cols = paste0("total_",2010:2023),
                                       names_sep = "_",
                                       names_to = c(".value","year"))
  car_km_pc_long$year = as.numeric(car_km_pc_long$year)
  names(car_km_pc_long) = c("PC_AREA","year","pc_total_km")

  # Weight By total number of vehicles
  vr_pc_summary = left_join(vr_pc_summary, car_km_pc_long, by = c("PC_AREA","year"))
  vr_pc_summary$pc_company_km = vr_pc_summary$pc_total_km * vr_pc_summary$company_ratio
  vr_pc_summary$pc_private_km = vr_pc_summary$pc_total_km - vr_pc_summary$pc_company_km
  vr_pc_summary$pc_car_km = vr_pc_summary$pc_private_km * vr_pc_summary$car_ratio
  vr_pc_summary$pc_van_km = vr_pc_summary$pc_private_km - vr_pc_summary$pc_car_km

  # Join on 2011 Data

  vehicle_registrations_21 = dplyr::left_join(vehicle_registrations_21,
                          car_km_2009_2011[,c("LSOA21CD","car_vkmyear_11","van_vkmyear_11")],
                         by = "LSOA21CD")
  vehicle_registrations_21 = dplyr::group_by(vehicle_registrations_21, PC_AREA, year)
  vehicle_registrations_21 = dplyr::mutate(vehicle_registrations_21,
                      car_share = Cars_Private * car_vkmyear_11 / sum(Cars_Private * car_vkmyear_11, na.rm = TRUE),
                      van_share = vans_Private  * van_vkmyear_11 / sum(vans_Private * van_vkmyear_11, na.rm = TRUE),
                      company_share = company_bike / sum(company_bike, na.rm = TRUE)
                      )
  vehicle_registrations_21 = dplyr::ungroup(vehicle_registrations_21)

  # Join on Postcode Totals

  vehicle_registrations_21 = dplyr::left_join(vehicle_registrations_21,
                          vr_pc_summary[,c("PC_AREA","year","pc_company_km",
                                           "pc_private_km","pc_car_km","pc_van_km")],
                          by = c("PC_AREA","year"))
  vehicle_registrations_21$car_km = vehicle_registrations_21$car_share * vehicle_registrations_21$pc_car_km
  vehicle_registrations_21$van_km = vehicle_registrations_21$van_share * vehicle_registrations_21$pc_van_km
  vehicle_registrations_21$company_km = vehicle_registrations_21$company_share * vehicle_registrations_21$pc_company_km

  vehicle_registrations_21 = vehicle_registrations_21[,c("LSOA21CD","year","Cars_Private","vans_Private","company_bike",
                                                         "all_vehicles","PC_AREA","car_km","van_km","company_km")]

  return(vehicle_registrations_21)

  # Old Code below

  #car_km_pc$baseline <- car_km_pc$`2011`
  # car_km_pc[paste0("trend_",as.character(2005:2023))] <- lapply(car_km_pc[as.character(2005:2023)], function(x){
  #   x <- x/car_km_pc$baseline
  #   x
  # })
  # car_km_pc <- car_km_pc[,c("PC_AREA",as.character(2006:2023),paste0("trend_",as.character(2006:2023)))]

  # LSOA data is fluctuating from 2009 to 2011
  # 1.0448, 1.02517, 1 (mean)
  # Postcode data is increasing
  # 1.0757, 1.120, 1.155 (mean)
  # summary(car_km_2009_2011$vans_total_09 / car_km_2009_2011$vans_total_11)
  # summary(car_km_pc$`2009`)
  # summary(car_km_2009_2011$vans_total_10 / car_km_2009_2011$vans_total_11)
  # summary(car_km_pc$`2010`)

  #car_km_2009_2011$car_vkmyear_11_norm = car_km_2009_2011$car_vkmyear_11 / sum(car_km_2009_2011$car_vkmyear_11, na.rm = TRUE)
  #car_km_2009_2011$van_vkmyear_11_norm = car_km_2009_2011$van_vkmyear_11 / sum(car_km_2009_2011$van_vkmyear_11, na.rm = TRUE)

  # Anonymised MOT data total km is 1.276804 time larger than Motoring along car + van
  # Difference due to (non-van e.g. bikes), but also company cars.
  # Subtract excess km
  # excess_km = sum(car_km_pc$`2011`, na.rm = TRUE) -
  #   sum(car_km_2009_2011$car_km_11, na.rm = TRUE) -
  #   sum(car_km_2009_2011$van_km_11, na.rm = TRUE)
  #
  # excess_ratio_2011 = excess_km / sum(car_km_pc$`2011`, na.rm = TRUE) #0.147

  #
  # fizz = foo2[foo2$PC_AREA == "B",]
  # fizz = fizz[fizz$year == 2011,]
  # sum(fizz$car_share)
  # (sum(fizz$car_km)) / fizz$pc_car_km[1]
  # (sum(fizz$van_km)) / fizz$pc_van_km[1]


  # Coomapr back to original data

  # chk = dplyr::left_join(foo2[foo2$year == 2011,],
  #                        car_km_2009_2011[,c("LSOA21CD","cars_total_11","vans_total_11","car_km_11","van_km_11")],
  #                        by = "LSOA21CD")
  #
  # plot(chk$cars_total_11, chk$Cars_Private)
  # abline(0,1, col = "red")
  #
  # plot(chk$van_km_11, chk$van_km)
  # abline(0,1, col = "red")
  # sum(chk$van_km_11)
  # sum(chk$van_km, na.rm = TRUE)
  # summary(lm(formula = van_km ~ van_km_11, data = chk))
  #
  # plot(chk$car_km_11, chk$car_km)
  # abline(0,1, col = "red")
  # sum(chk$car_km_11)
  # sum(chk$car_km, na.rm = TRUE)
  # summary(lm(formula = car_km ~ car_km_11, data = chk))
  #
  # chk$van_diff = chk$van_km_11 - chk$van_km
  # chk$car_diff = chk$car_km_11 - chk$car_km
  #
  # tar_load(lookup_lsoa_2011_21)
  # lookup_lsoa_2011_21 = lookup_lsoa_2011_21[,c("LSOA21CD","CHGIND")]
  # lookup_lsoa_2011_21 = lookup_lsoa_2011_21[!duplicated(lookup_lsoa_2011_21$LSOA21CD),]
  #
  # chk = left_join(chk, lookup_lsoa_2011_21, by = "LSOA21CD")
  #
  # library(ggplot2)
  # ggplot(chk, aes(x = cars_total_11, y = Cars_Private, colour = CHGIND))+
  #   geom_point()


  # #Get Ratio km per vehicle for car and van in 2011
  # car_km_2009_2011$car_van_km_ratio_11 = (car_km_2009_2011$car_km_11 / car_km_2009_2011$cars_total_11)  / # km per car
  #   ((car_km_2009_2011$car_km_11 + car_km_2009_2011$van_km_11) / (car_km_2009_2011$cars_total_11 + car_km_2009_2011$vans_total_11)) # km per car + van
  # car_km_2009_2011$car_van_km_ratio_11[is.nan(car_km_2009_2011$car_van_km_ratio_11)] = 0 # 0 / 0 error for new LSOAs
  #
  # # MOTORing along cars count most closely matches "Cars_Private_Licensed"
  # # vans most closely matches Other body types_Private_Licensed
  #
  #
  #
  # # Work out ratio of private to company nationally, by year
  # national_private_company = dplyr::group_by(vehicle_registrations_21, year)
  # national_private_company = dplyr::summarise(national_private_company,
  #                                            private_cars = sum(Cars_Private_Licensed, na.rm = TRUE),
  #                                            private_vans = sum(`Other body types_Private_Licensed`, na.rm = TRUE),
  #                                            company_or_bike = sum(Cars_Company_Licensed,
  #                                                                  `Other body types_Company_Licensed`,
  #                                                                  Motorcycles_Company_Licensed,
  #                                                                  Motorcycles_Private_Licensed,
  #                                                                  na.rm = TRUE
  #                                                                  ))
  # national_private_company$company_or_bike_national_share = national_private_company$company_or_bike /
  #   rowSums(national_private_company[,c("private_cars","private_vans","company_or_bike")])
  #
  #
  #
  #
  # # Add private compnay share
  #
  # car_km_pc_long = dplyr::left_join(car_km_pc_long,
  #                                   national_private_company[,c("year","company_or_bike_national_share")],
  #                                   by = c("year"))
  #
  # car_km_pc_long$pc_private_km = car_km_pc_long$pc_total_km * (1- car_km_pc_long$company_or_bike_national_share)
  # car_km_pc_long$pc_company_bike_km = car_km_pc_long$pc_total_km * (car_km_pc_long$company_or_bike_national_share)
  #
  # # From Motoring along get 2011 basline LSOA shares
  # # These adjust LSOA above or below the average per vehicle later on
  # # i.e a place with high miles in 2011 will given have more than average miles per vehicle in 2019
  # baseline_LSOA_share = car_km_2009_2011[,c("LSOA21CD","PC_AREA","car_km_11","van_km_11","car_van_km_ratio_11")]
  # baseline_LSOA_share$van_km_11[is.na(baseline_LSOA_share$van_km_11)] = 0
  # baseline_LSOA_share = dplyr::group_by(baseline_LSOA_share, PC_AREA)
  # baseline_LSOA_share = dplyr::mutate(baseline_LSOA_share,
  #                        car_km_share_11 = car_km_11 /  sum(car_km_11, na.rm = TRUE),
  #                        van_km_share_11 = van_km_11 /  sum(van_km_11, na.rm = TRUE),
  #                        car_km_share_11_norm = car_km_share_11 /  mean(car_km_share_11, na.rm = TRUE),
  #                        van_km_share_11_norm = van_km_share_11 /  mean(van_km_share_11, na.rm = TRUE)
  # )
  # baseline_LSOA_share = dplyr::ungroup(baseline_LSOA_share)
  # baseline_LSOA_share = baseline_LSOA_share[,c("LSOA21CD","PC_AREA","car_km_share_11_norm","van_km_share_11_norm","car_van_km_ratio_11")]
  #
  #
  # # Get vehicle_registrations_21 trends
  # vehicle_registrations_21 = dplyr::group_by(vehicle_registrations_21, LSOA21CD)
  # vehicle_registrations_21 = dplyr::mutate(vehicle_registrations_21,
  #                     car_trend = Cars_Private_Licensed / Cars_Private_Licensed[year == 2011],
  #                     van_trend = `Other body types_Private_Licensed` / `Other body types_Private_Licensed`[year == 2011])
  # vehicle_registrations_21 = dplyr::ungroup(vehicle_registrations_21)
  #
  # vehicle_registrations_21$car_trend[is.na(vehicle_registrations_21$car_trend)] = 0
  # vehicle_registrations_21$van_trend[is.na(vehicle_registrations_21$van_trend)] = 0
  #
  # vehicle_registrations_21$company_or_bike_total = rowSums(vehicle_registrations_21[,
  #   c("Motorcycles_Company_Licensed","Motorcycles_Private_Licensed",
  #     "Cars_Company_Licensed","Other body types_Company_Licensed")],
  #   na.rm = TRUE)
  #
  # vehicle_registrations_21$company_or_bike_share = vehicle_registrations_21$company_or_bike_total /
  #   rowSums(vehicle_registrations_21[,
  #                                    c("Cars_Private_Licensed","Other body types_Private_Licensed",
  #                                      "Motorcycles_Company_Licensed","Motorcycles_Private_Licensed",
  #                                      "Cars_Company_Licensed","Other body types_Company_Licensed")],
  #           na.rm = TRUE)
  # vehicle_registrations_21$company_or_bike_share[is.na(vehicle_registrations_21$company_or_bike_share)] = 0
  #
  # # Join on baseline share, then adjust share based on trend
  #
  # foo = dplyr::left_join(vehicle_registrations_21, baseline_LSOA_share, by = c("LSOA21CD"))
  #
  # foo$Motorcycles_Company_Licensed[is.na(foo$Motorcycles_Company_Licensed)] = 0
  # foo$Motorcycles_Private_Licensed[is.na(foo$Motorcycles_Private_Licensed)] = 0
  #
  # foo = dplyr::group_by(foo, PC_AREA, year)
  # foo = dplyr::mutate(foo,
  #                    car_share = Cars_Private_Licensed / sum(Cars_Private_Licensed, na.rm = TRUE ),
  #                    van_share = `Other body types_Private_Licensed` / sum(`Other body types_Private_Licensed`, na.rm = TRUE ),
  #                    company_or_bike_share = (Cars_Company_Licensed +
  #                                 `Other body types_Company_Licensed` +
  #                                 `Motorcycles_Company_Licensed` +
  #                                 `Motorcycles_Private_Licensed`) /
  #                      sum(Cars_Company_Licensed,
  #                            `Other body types_Company_Licensed`,
  #                            `Motorcycles_Company_Licensed`,
  #                            `Motorcycles_Private_Licensed`, na.rm = TRUE ),
  #                    )
  # foo = dplyr::ungroup(foo)
  #
  # # km share in 2019 is vehicle share in 2019 * km share in 2011
  # foo$car_km_share = foo$car_share * foo$car_km_share_11_norm * foo$car_van_km_ratio_11
  # foo$van_km_share = foo$van_share * foo$van_km_share_11_norm * foo$car_van_km_ratio_11
  # foo$company_or_bike_km_share = foo$company_or_bike_share # Don't have pre-existing data so based only on vehicle numbers
  #
  # #Normalise
  # foo = dplyr::group_by(foo, PC_AREA, year)
  # foo = dplyr::mutate(foo,
  #                     car_km_share_norm = (car_km_share / mean(car_km_share)),
  #                     van_km_share_norm = (van_km_share / mean(van_km_share))
  #                     )
  # foo = dplyr::mutate(foo,
  #                     # car_km_share_norm2 = car_km_share_norm * (1 / sum(car_km_share_norm, van_km_share_norm)),
  #                     # van_km_share_norm2 = van_km_share_norm * (1 / sum(car_km_share_norm, van_km_share_norm))
  #                     car_km_share_norm2 = car_km_share_norm * (1 / sum(car_km_share_norm)),
  #                     van_km_share_norm2 = van_km_share_norm * (1 / sum(van_km_share_norm))
  #                     )
  # foo = dplyr::ungroup(foo)
  #
  #
  # # Join on Post Code Totals
  # foo2 = dplyr::left_join(foo, car_km_pc_long, by = c("PC_AREA", "year"))
  #
  # foo2$car_km = foo2$pc_private_km * foo2$car_km_share_norm2 * foo2$car_van_km_ratio_11
  # foo2$van_km = foo2$pc_private_km * foo2$van_km_share_norm2 * (1 - foo2$car_van_km_ratio_11)
  #
  #
  # buzz = foo2 %>% group_by(PC_AREA, year) %>% summarise(car_km = sum(car_km),
  #                                                       van_km = sum(van_km),
  #                                                       car_km_share_norm2 = sum(car_km_share_norm2),
  #                                                       van_km_share_norm2=sum(van_km_share_norm2),
  #                                                       car_van_km_ratio_11 = sum(car_van_km_ratio_11)
  #                                                       )
  #
  #
  # fizz = foo2[foo2$PC_AREA == "B",]
  # fizz = fizz[fizz$year == 2011,]
  # (sum(fizz$car_km) + sum(fizz$van_km)) / fizz$pc_private_km[1]
  #
  # fizz_j = dplyr::left_join(fizz, car_km_2009_2011[,c("LSOA21CD","car_km_11" )], by = "LSOA21CD")
  #
  # fizz2 = foo2[foo2$LSOA21CD == "E01034929",]
  # car_km_2009_2011$car_km_11[car_km_2009_2011$LSOA21CD == "E01034929"] / fizz2$car_km[fizz2$year == 2011]
  # stop("Motoring along car km in 2011 is 15x estimate!")
  #
  # library(ggplot2)
  # ggplot(fizz2, aes(x = year)) +
  #   geom_line(aes(y = car_km), color = "red") +
  #   geom_line(aes(y = van_km), color = "blue") +
  #   geom_line(aes(y = Cars_Private_Licensed))
  #
  # coeff = 1e3
  # ggplot(fizz2, aes(x=year)) +
  #   geom_line( aes(y=Cars_Private_Licensed), color="red") +
  #   geom_line( aes(y=car_km / coeff), color="blue") +
  #   scale_y_continuous(
  #     name = "LSOA",
  #     sec.axis = sec_axis(~.*coeff, name="Postcode")
  #   )
  #
  #
  # summary(fizz$car_km_share_norm)
  # summary(fizz$car_km_share_norm2)
  # sum(fizz$car_km_share_norm2)
  # summary(fizz$car_share)
  #
  # sum(fizz$car_km_share_11_norm) / sum(fizz$car_km_share)
  #
  # sum(fizz$car_km_share)
  # # Extrapolate car and van km based on Postcode Area Trend and trend in total vehicles
  #
  # ckm = car_km_2009_2011[,c("LSOA21CD","cars_total_11","vans_total_11","car_km_11","van_km_11","PC_AREA")]
  #
  #
  #
  # # Find difference in km per vehicle for private/company and car/van in 2011
  # vehicle_registrations_21 = dplyr::left_join(vehicle_registrations_21, ckm, by = "LSOA21CD")
  #
  # pc_vr = dplyr::group_by(vehicle_registrations_21, PC_AREA, year)
  # pc_vr = dplyr::summarise(pc_vr,
  #                          private_cars = sum(Cars_Private_Licensed, na.rm = TRUE),
  #                          private_vans = sum(`Other body types_Private_Licensed`, na.rm = TRUE),
  #                          company_or_bike_total = sum(company_or_bike_total, na.rm = TRUE))
  # pc_vr = dplyr::ungroup(pc_vr)
  # pc_vr = dplyr::group_by(pc_vr, PC_AREA)
  # pc_vr = dplyr::mutate(pc_vr,
  #                       company_or_bike_trend = company_or_bike_total / company_or_bike_total[year == 2011],
  #                       company_or_bike_share_2011 = company_or_bike_total[year == 2011] /
  #                         c(company_or_bike_total[year == 2011] + private_cars[year == 2011] + private_cars[year == 2011]),
  #                       company_or_bike_vehicle_share = company_or_bike_share_2011 * company_or_bike_trend
  #                       )
  # pc_vr = dplyr::ungroup(pc_vr)
  #
  # pc_vr = pc_vr[,c("PC_AREA","year","private_cars","private_vans",
  #                  "company_or_bike_total","company_or_bike_vehicle_share")]
  # car_km_pc_long = dplyr::left_join(car_km_pc_long, pc_vr, by = c("PC_AREA","year"))
  # car_km_pc_long = car_km_pc_long[car_km_pc_long$year > 2009,]

  # # Have company_or_bike_vehicle_share now get km share in 2011
  # pc_km_share = vehicle_registrations_21[vehicle_registrations_21$year == 2011,]
  # pc_km_share = dplyr::group_by(pc_km_share, PC_AREA, year)
  # pc_km_share = dplyr::summarise(pc_km_share,
  #                                car_km_11 = sum(car_km_11, na.rm = TRUE),
  #                                van_km_11 = sum(van_km_11, na.rm = TRUE))
  #
  # pc_km_share = dplyr::left_join(pc_km_share, car_km_pc_long, by = c("PC_AREA","year"))
  # pc_km_share$company_or_bike_total_km = pc_km_share$pc_total_km - pc_km_share$car_km_11 - pc_km_share$van_km_11
  #
  # stop("5 areas DL, W, SM, BR, SN have more km in MOTORing along than in Anoymised MOT data")
  # # Solution to aportion non-private km nationally not locally, wait for CARS project
  #
  # chk = dplyr::group_by(car_km_2009_2011, PC_AREA)
  # chk = dplyr::summarise(chk,
  #                        cars_total_11 = sum(cars_total_11, na.rm = TRUE),
  #                        vans_total_11 = sum(vans_total_11, na.rm = TRUE),
  #                        )
  #
  # #
  #
  # car_km_pc_long$pc_km_private = car_km_pc_long$pc_total_km * (1 - car_km_pc_long$company_or_bike_vehicle_share)
  # car_km_pc_long$pc_km_company_bike = car_km_pc_long$pc_total_km * car_km_pc_long$company_or_bike_vehicle_share
  #
  #
  #
  # foo = dplyr::left_join(foo, car_km_pc_long, by = c("PC_AREA","year"))
  #
  # bar = foo[foo$year == 2011,]
  # bar = bar[bar$PC_AREA == "LS",] # testing
  # bar_other_km = bar$pc_total_km[1] - sum(c(bar$car_km_11, bar$van_km_11), na.rm = TRUE) # 7% km done by non-priate car/van
  #
  # # Esimtat of LSOA km driven buy vehciles that are not private cars/vans in 2011
  # bar$pc_other_km_total = bar_other_km * bar$company_or_bike_total / sum(bar$company_or_bike_total, na.rm = TRUE)
  #
  # # Next step extrapolate share of other vehicle km based on share of other vehicles count
  # # i.e if number of company cars/vans doubles share of total km doubles
  #
  #
  #
  # # bar = dplyr::group_by(foo, year, PC_AREA)
  # # bar = dplyr::mutate(bar,
  # #
  # #                     )





}



car_km_11_to_21 = function(car_km_lsoa_11, lsoa_11_21_tools){

  names(car_km_lsoa_11)[1] = "LSOA11CD"

  car_km_S = car_km_lsoa_11[car_km_lsoa_11$LSOA11CD %in% lsoa_11_21_tools$lookup_split$LSOA11CD,]
  car_km_M = car_km_lsoa_11[car_km_lsoa_11$LSOA11CD %in% lsoa_11_21_tools$lookup_merge$LSOA11CD,]
  car_km_U = car_km_lsoa_11[car_km_lsoa_11$LSOA11CD %in% lsoa_11_21_tools$lookup_unchanged$LSOA11CD,]

  #Unchanged
  car_km_U = dplyr::left_join(car_km_U, lsoa_11_21_tools$lookup_unchanged, by = "LSOA11CD")

  # Merge
  car_km_M = dplyr::left_join(car_km_M, lsoa_11_21_tools$lookup_merge, by = "LSOA11CD")
  car_km_M = dplyr::select(car_km_M, -LSOA11CD)
  car_km_M = dplyr::group_by(car_km_M, LSOA21CD)
  car_km_M = dplyr::summarise_all(car_km_M, sum, na.rm = TRUE)
  car_km_M = dplyr::ungroup(car_km_M)

  #Split
  lookup_split = lsoa_11_21_tools$lookup_split
  lookup_split = lookup_split[,c("LSOA11CD","LSOA21CD","year","household_ratio")]
  lookup_split = lookup_split[lookup_split$year %in% 2009:2023,]

  names(car_km_S) = gsub("vans_total","vanstotal",names(car_km_S))
  names(car_km_S) = gsub("van_km","vankm",names(car_km_S))
  names(car_km_S) = gsub("car_km","carkm",names(car_km_S))

  car_km_S = dplyr::left_join(lsoa_11_21_tools$lookup_split, car_km_S,
                                    by = "LSOA11CD", relationship = "many-to-many")
  car_km_S = as.data.frame(car_km_S)
  for(i in 5:6){
    car_km_S[i] = car_km_S[,i ,drop = TRUE] * car_km_S$pop_ratio
  }

  nms = c("LSOA21CD",paste0("van_km_",10:23),paste0("car_km_",10:23))

  car_km_S = car_km_S[,nms]
  car_km_M = car_km_M[,nms]
  car_km_U = car_km_U[,nms]

  final = rbind(car_km_S, car_km_M, car_km_U)
  final

}

