#' Read total vehicle-km per postcode area from anonymised MOT data
#'
#' @description Reads the pre-cleaned Rds of total annual vehicle km per
#'   postcode area (2005-2023) derived from anonymised MOT odometer records
#'   (secure data). Used by the `car_km_pc` target, which provides the trend
#'   over time for `extraplote_car_km_trends2()`.
#' @param path Path to `postcode_total_vkm_2005_2023.Rds`.
#' @return An sf data frame with `PC_AREA`, one column per year and postcode
#'   area geometry.
#' @keywords internal
read_mot_km_pc = function(path = file.path(parameters$path_secure_data,"CARS/Anoymised MOT/clean/postcode_total_vkm_2005_2023.Rds")){
  vkm = readRDS(path)
  vkm
}




#' Estimate annual car/van/company km per zone, 2010-2023
#'
#' @description Downscales postcode-area total vehicle-km (from anonymised
#'   MOT data) to GB zones. Each zone is assigned to a postcode area by
#'   centroid; the area total is split into company/private then car/van
#'   using DfT registration ratios; each zone's share is weighted by its
#'   vehicle count times its 2011 per-vehicle km (from the RAC/MOT 2009-11
#'   data, carried over boundary changes; City of London EC/WC areas borrow
#'   the W profile as they have no MOT tests). Used by the `car_km_lsoa_21`
#'   target, feeding `calculate_car_emissions()`.
#' @param car_km_pc Postcode-area total vkm (`car_km_pc` target).
#' @param car_km_2009_2011 RAC/MOT LSOA outputs (`car_km_2009_2011` target).
#' @param centroids_lsoa21 E&W LSOA centroids (`centroids_lsoa21` target).
#' @param centroids_dz22 Scottish DZ centroids (`centroids_dz22` target).
#' @param vehicle_registrations_21 DfT registrations
#'   (`vehicle_registrations` target).
#' @param lookup_lsoa_2011_21 ONS 2011-to-2021 LSOA lookup.
#' @param lookup_dz_2011_22 Data Zone 2011-to-2022 split shares.
#' @param years Years to estimate.
#' @return A data frame per zone-year with vehicle counts, `PC_AREA`,
#'   `car_km`, `van_km` and `company_km`.
#' @keywords internal
extraplote_car_km_trends2 = function(car_km_pc,
                                     car_km_2009_2011,
                                     centroids_lsoa21,
                                     centroids_dz22,
                                     vehicle_registrations_21,
                                     lookup_lsoa_2011_21,
                                     lookup_dz_2011_22,
                                     years = 2010:2023){

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


