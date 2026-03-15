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
                                     centroids_dz22,
                                    vehicle_registrations_21, lookup_lsoa_2011_21, lookup_dz_2011_22, years = 2010:2023){

  vehicle_registrations_21$year = as.integer(gsub(" Q1","",vehicle_registrations_21$quarter))

  vehicle_registrations_21 = vehicle_registrations_21[vehicle_registrations_21$year %in% years,]
  vehicle_registrations_21 = vehicle_registrations_21[substr(vehicle_registrations_21$LSOA21CD,1,1) %in% c("E","W","S"),]

  pc_geom = car_km_pc[,"PC_AREA"]
  pc_geom = sf::st_transform(pc_geom, 27700)
  car_km_pc = sf::st_drop_geometry(car_km_pc)
  car_km_pc = car_km_pc[,c("PC_AREA",years)]


  car_km_2009_2011$vans_total_11[is.na(car_km_2009_2011$vans_total_11)] = 0
  car_km_2009_2011$van_km_11[is.na(car_km_2009_2011$van_km_11)] = 0


  # Combine Scot and EW
  centroids_dz22 = centroids_dz22[,c("LSOA21CD","geometry")]
  names(centroids_dz22) =  c("LSOA21CD","geometry")
  centroids_lsoa21 = rbind(centroids_lsoa21, centroids_dz22)

  centroids_lsoa21 <- sf::st_join(centroids_lsoa21, pc_geom)
  centroids_lsoa21 <- sf::st_drop_geometry(centroids_lsoa21)
  centroids_lsoa21 <- centroids_lsoa21[,c("LSOA21CD","PC_AREA")]

  # Merged LSOA get data from one 2011 LSOA
  lookup_lsoa_2011_21 = lookup_lsoa_2011_21[,c("LSOA21CD","LSOA11CD")]
  lookup_lsoa_2011_21 = lookup_lsoa_2011_21[!duplicated(lookup_lsoa_2011_21$LSOA21CD),]

  lookup_dz_2011_22 = lookup_dz_2011_22[order(lookup_dz_2011_22$splitshare, decreasing = TRUE),]
  lookup_dz_2011_22 = lookup_dz_2011_22[!duplicated(lookup_dz_2011_22$LSOA21CD),]

  lookup_lsoa_2011_21 = rbind(lookup_lsoa_2011_21, lookup_dz_2011_22[,c("LSOA21CD","LSOA11CD")])

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
  vehicle_registrations_21$company_bike = rowSums(vehicle_registrations_21[,c("Cars_COMPANY","Other vehicles_COMPANY",
                                                                              "Motorcycles_COMPANY","Motorcycles_PRIVATE")],
                                                  na.rm = TRUE)
  vehicle_registrations_21$all_vehicles = rowSums(vehicle_registrations_21[,c("Cars_PRIVATE","Other vehicles_PRIVATE","company_bike")],
                                                  na.rm = TRUE)
  vehicle_registrations_21 = vehicle_registrations_21[,c("LSOA21CD","year","Cars_PRIVATE",
                                                         "Other vehicles_PRIVATE","company_bike",
                                                         "all_vehicles")]
  names(vehicle_registrations_21) = gsub("Other vehicles","vans",names(vehicle_registrations_21))

  # Add on PC

  vehicle_registrations_21 = dplyr::left_join(vehicle_registrations_21, centroids_lsoa21, by = "LSOA21CD")
  vehicle_registrations_21$PC_AREA[vehicle_registrations_21$PC_AREA %in% c("EC","WC")] = "W"

  vr_pc_summary = dplyr::group_by(vehicle_registrations_21, PC_AREA, year)
  vr_pc_summary = dplyr::summarise(vr_pc_summary,
                                   Cars_Private = sum(Cars_PRIVATE),
                                   vans_Private = sum(vans_PRIVATE),
                                   company_bike = sum(company_bike),
                                   all_vehicles = sum(all_vehicles))

  vr_pc_summary$company_ratio = vr_pc_summary$company_bike /  vr_pc_summary$all_vehicles
  vr_pc_summary$car_ratio = vr_pc_summary$Cars_Private /  (vr_pc_summary$Cars_Private + vr_pc_summary$vans_Private)

  # Pivot Longer Postcode data

  names(car_km_pc) = gsub("^20","total_20",names(car_km_pc))
  car_km_pc_long = tidyr::pivot_longer(car_km_pc,
                                       cols = paste0("total_",years),
                                       names_sep = "_",
                                       names_to = c(".value","year"))
  car_km_pc_long$year = as.numeric(car_km_pc_long$year)
  names(car_km_pc_long) = c("PC_AREA","year","pc_total_km")

  # Weight By total number of vehicles
  vr_pc_summary = dplyr::left_join(vr_pc_summary, car_km_pc_long, by = c("PC_AREA","year"))
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
                      car_share = Cars_PRIVATE * car_vkmyear_11 / sum(Cars_PRIVATE * car_vkmyear_11, na.rm = TRUE),
                      van_share = vans_PRIVATE  * van_vkmyear_11 / sum(vans_PRIVATE * van_vkmyear_11, na.rm = TRUE),
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

  vehicle_registrations_21 = vehicle_registrations_21[,c("LSOA21CD","year","Cars_PRIVATE","vans_PRIVATE","company_bike",
                                                         "all_vehicles","PC_AREA","car_km","van_km","company_km")]

  return(vehicle_registrations_21)


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

