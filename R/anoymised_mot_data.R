read_mot_km_pc = function(path = file.path(parameters$path_secure_data,"CARS/Anoymised MOT/clean/postcode_total_vkm_2005_2023.Rds")){
  vkm = readRDS(path)
  vkm
}

extraplote_car_km_trends = function(car_km_pc, car_km_2009_2011, centroids_lsoa11, centroids_dz11){
  # Combine Scot and EW
  centroids_lsoa11 = rbind(centroids_lsoa11, centroids_dz11)

  # Total scaling fo account for difference between datasets
  #sum(car_km_pc$`2011`, na.rm = T) / sum(car_km_2009_2011$car_km_11) # 1.342153

  pc_geom = car_km_pc[,"PC_AREA"]
  pc_geom = sf::st_transform(pc_geom, 27700)
  car_km_pc = sf::st_drop_geometry(car_km_pc)

  #TODO: Update to 2023
  car_km_pc$baseline <- car_km_pc$`2011`
  car_km_pc[as.character(2005:2023)] <- lapply(car_km_pc[as.character(2005:2023)], function(x){
    x <- x/car_km_pc$baseline * 1.342153
    x
  })
  #mot_postcode_baseline <- as.data.frame(mot_postcode_baseline)
  #names(mot_postcode_baseline) <- paste0("base_",names(mot_postcode_baseline))
  #mot_postcode <- cbind(mot_postcode, mot_postcode_baseline)

  # LSOA data is fluctuating from 2009 to 2011
  # 1.0448, 1.02517, 1 (mean)
  # Postcode data is increasing
  # 1.0757, 1.120, 1.155 (mean)
  summary(car_km_2009_2011$vans_total_09 / car_km_2009_2011$vans_total_11)
  summary(car_km_pc$`2009`)
  summary(car_km_2009_2011$vans_total_10 / car_km_2009_2011$vans_total_11)
  summary(car_km_pc$`2010`)

  centroids_lsoa11 <- sf::st_join(centroids_lsoa11, pc_geom)
  centroids_lsoa11 <- sf::st_drop_geometry(centroids_lsoa11)
  centroids_lsoa11 <- centroids_lsoa11[,c("LSOA11CD","PC_AREA")]

  car_km_2009_2011 <- dplyr::left_join(car_km_2009_2011, centroids_lsoa11, by = c("LSOA11" = "LSOA11CD"))
  #No MOT tests done in City of London (EC & WC)
  #So use us neighbouring W as example
  car_km_2009_2011$PC_AREA[car_km_2009_2011$PC_AREA %in% c("EC","WC")] = "W"
  car_km_pc <- car_km_pc[,c("PC_AREA",as.character(2006:2023))]

  car_km_2009_2011 <- dplyr::left_join(car_km_2009_2011, car_km_pc, by = c("PC_AREA" = "PC_AREA"))

  car_km_2009_2011$car_km_11_orig <- car_km_2009_2011$car_km_11
  car_km_2009_2011$van_km_11_orig <- car_km_2009_2011$van_km_11

  car_km_2009_2011$car_km_09 <- car_km_2009_2011$car_km_11_orig * car_km_2009_2011$`2009`
  car_km_2009_2011$car_km_10 <- car_km_2009_2011$car_km_11_orig * car_km_2009_2011$`2010`
  car_km_2009_2011$car_km_11 <- car_km_2009_2011$car_km_11_orig * car_km_2009_2011$`2011`
  car_km_2009_2011$car_km_12 <- car_km_2009_2011$car_km_11_orig * car_km_2009_2011$`2012`
  car_km_2009_2011$car_km_13 <- car_km_2009_2011$car_km_11_orig * car_km_2009_2011$`2013`
  car_km_2009_2011$car_km_14 <- car_km_2009_2011$car_km_11_orig * car_km_2009_2011$`2014`
  car_km_2009_2011$car_km_15 <- car_km_2009_2011$car_km_11_orig * car_km_2009_2011$`2015`
  car_km_2009_2011$car_km_16 <- car_km_2009_2011$car_km_11_orig * car_km_2009_2011$`2016`
  car_km_2009_2011$car_km_17 <- car_km_2009_2011$car_km_11_orig * car_km_2009_2011$`2017`
  car_km_2009_2011$car_km_18 <- car_km_2009_2011$car_km_11_orig * car_km_2009_2011$`2018`
  car_km_2009_2011$car_km_19 <- car_km_2009_2011$car_km_11_orig * car_km_2009_2011$`2019`
  car_km_2009_2011$car_km_20 <- car_km_2009_2011$car_km_11_orig * car_km_2009_2011$`2020`
  car_km_2009_2011$car_km_21 <- car_km_2009_2011$car_km_11_orig * car_km_2009_2011$`2021`
  car_km_2009_2011$car_km_22 <- car_km_2009_2011$car_km_11_orig * car_km_2009_2011$`2022`
  car_km_2009_2011$car_km_23 <- car_km_2009_2011$car_km_11_orig * car_km_2009_2011$`2023`

  car_km_2009_2011$van_km_09 <- car_km_2009_2011$van_km_11_orig * car_km_2009_2011$`2009`
  car_km_2009_2011$van_km_10 <- car_km_2009_2011$van_km_11_orig * car_km_2009_2011$`2010`
  car_km_2009_2011$van_km_11 <- car_km_2009_2011$van_km_11_orig * car_km_2009_2011$`2011`
  car_km_2009_2011$van_km_12 <- car_km_2009_2011$van_km_11_orig * car_km_2009_2011$`2012`
  car_km_2009_2011$van_km_13 <- car_km_2009_2011$van_km_11_orig * car_km_2009_2011$`2013`
  car_km_2009_2011$van_km_14 <- car_km_2009_2011$van_km_11_orig * car_km_2009_2011$`2014`
  car_km_2009_2011$van_km_15 <- car_km_2009_2011$van_km_11_orig * car_km_2009_2011$`2015`
  car_km_2009_2011$van_km_16 <- car_km_2009_2011$van_km_11_orig * car_km_2009_2011$`2016`
  car_km_2009_2011$van_km_17 <- car_km_2009_2011$van_km_11_orig * car_km_2009_2011$`2017`
  car_km_2009_2011$van_km_18 <- car_km_2009_2011$van_km_11_orig * car_km_2009_2011$`2018`
  car_km_2009_2011$van_km_19 <- car_km_2009_2011$van_km_11_orig * car_km_2009_2011$`2019`
  car_km_2009_2011$van_km_20 <- car_km_2009_2011$van_km_11_orig * car_km_2009_2011$`2020`
  car_km_2009_2011$van_km_21 <- car_km_2009_2011$van_km_11_orig * car_km_2009_2011$`2021`
  car_km_2009_2011$van_km_22 <- car_km_2009_2011$van_km_11_orig * car_km_2009_2011$`2022`
  car_km_2009_2011$van_km_23 <- car_km_2009_2011$van_km_11_orig * car_km_2009_2011$`2023`

  car_km_2009_2011 <- car_km_2009_2011[,c("LSOA11","vans_total_09","vans_total_10","vans_total_11",
                                  paste0("van_km_",10:23),
                                  paste0("car_km_",10:23))]

  car_km_2009_2011



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

