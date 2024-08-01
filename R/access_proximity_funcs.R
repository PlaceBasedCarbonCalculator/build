summarise_access_proximity = function(access_poi_circle_15min,
                                      access_poi_iso_15min,
                                      access_poi_circle_30min,
                                      access_poi_iso_30min,
                                      access_poi_circle_45min,
                                      access_poi_iso_45min,
                                      access_poi_circle_60min,
                                      access_poi_iso_60min,
                                      lookup_oa2021_lsoa2021,
                                      area_classifications
                                      ){



  lookup_oa2021_lsoa2021$nearest_OA2021_dist = NULL

  access_poi_iso_15min = dplyr::left_join(access_poi_iso_15min,
                                          lookup_oa2021_lsoa2021,
                                          by = c("OA21CD" = "nearest_OA2021"),
                                          relationship = "many-to-many")
  access_poi_iso_15min$OA21CD = NULL

  access_poi_iso_30min = dplyr::left_join(access_poi_iso_30min,
                                          lookup_oa2021_lsoa2021,
                                          by = c("OA21CD" = "nearest_OA2021"),
                                          relationship = "many-to-many")
  access_poi_iso_30min$OA21CD = NULL

  access_poi_iso_45min = dplyr::left_join(access_poi_iso_45min,
                                          lookup_oa2021_lsoa2021,
                                          by = c("OA21CD" = "nearest_OA2021"),
                                          relationship = "many-to-many")
  access_poi_iso_45min$OA21CD = NULL

  access_poi_iso_60min = dplyr::left_join(access_poi_iso_60min,
                                          lookup_oa2021_lsoa2021,
                                          by = c("OA21CD" = "nearest_OA2021"),
                                          relationship = "many-to-many")
  access_poi_iso_60min$OA21CD = NULL


  corenms = c("LSOA21CD","groupname","categoryname","classname","population")

  access_poi_iso_15min = access_poi_iso_15min[,c(corenms,"sp10kp_diff_SD")]
  access_poi_circle_15min = access_poi_circle_15min[,c(corenms,"sp10kp_diff_SD")]
  names(access_poi_iso_15min)[5:6] = c("pop_access_15","access_15")
  names(access_poi_circle_15min)[5:6] = c("pop_proximity_15","proximity_15")

  access_poi_iso_30min = access_poi_iso_30min[,c(corenms,"sp10kp_diff_SD")]
  access_poi_circle_30min = access_poi_circle_30min[,c(corenms,"sp10kp_diff_SD")]
  names(access_poi_iso_30min)[5:6] = c("pop_access_30","access_30")
  names(access_poi_circle_30min)[5:6] = c("pop_proximity_30","proximity_30")

  access_poi_iso_45min = access_poi_iso_45min[,c(corenms,"sp10kp_diff_SD")]
  access_poi_circle_45min = access_poi_circle_45min[,c(corenms,"sp10kp_diff_SD")]
  names(access_poi_iso_45min)[5:6] = c("pop_access_45","access_45")
  names(access_poi_circle_45min)[5:6] = c("pop_proximity_45","proximity_45")

  access_poi_iso_60min = access_poi_iso_60min[,c(corenms,"sp10kp_diff_SD","nat_pps")]
  access_poi_circle_60min = access_poi_circle_60min[,c(corenms,"sp10kp_diff_SD")]
  names(access_poi_iso_60min)[5:6] = c("pop_access_60","access_60")
  names(access_poi_circle_60min)[5:6] = c("pop_proximity_60","proximity_60")


  #foo = access_poi_iso_15min[access_poi_iso_15min$LSOA21CD == "E01019468",]

  corenms = c("LSOA21CD","groupname","categoryname","classname")

  ap = dplyr::full_join(access_poi_iso_15min, access_poi_circle_15min, by = corenms)
  ap = dplyr::full_join(ap, access_poi_iso_30min, by = corenms)
  ap = dplyr::full_join(ap, access_poi_circle_30min, by = corenms)
  ap = dplyr::full_join(ap, access_poi_iso_45min, by = corenms)
  ap = dplyr::full_join(ap, access_poi_circle_45min, by = corenms)
  ap = dplyr::full_join(ap, access_poi_iso_60min, by = corenms)
  ap = dplyr::full_join(ap, access_poi_circle_60min, by = corenms)


  # Add Missing combinations
  #miss = access_poi_iso_60min[!duplicated(access_poi_iso_60min$classname),]
  #miss = miss[,c("groupname","categoryname","classname","nat_pps")]
  #miss = miss[rep(1:nrow(miss), length(unique(access_poi_circle_60min$LSOA21CD))),] # 35672 LSOA in England and Wales
  #miss$LSOA21CD = rep(unique(lookup_oa2021_lsoa2021$LSOA21CD), each = 385) # 385 number of location types

  #ap = dplyr::full_join(ap, miss, by = c("LSOA21CD","groupname","categoryname","classname"))
  #ap$nat_pps = dplyr::if_else(!is.na(ap$nat_pps.x), ap$nat_pps.x, ap$nat_pps.y)
  #ap$nat_pps.x = NULL
  #ap$nat_pps.y = NULL
  ap = as.data.frame(ap)

  #Crop to +/- lim
  # Reserve -3 for insufficient people
  lim_pos = 3
  lim_neg = -2.9

  nms = names(ap)
  nms = nms[grepl("access|proximity",nms)]
  nms = nms[!grepl("pop_",nms)]
  for(i in nms){
    ap[,i] = dplyr::if_else(ap[,i] < lim_neg ,lim_neg,ap[,i])
    ap[,i] = dplyr::if_else(ap[,i] > lim_pos ,lim_pos,ap[,i])
  }

  # Fill in missing populations
  # ap = dplyr::group_by(ap, LSOA21CD)
  # ap = dplyr::mutate(ap, pop_access_15 = dplyr::if_else(is.na(pop_access_15),max(pop_access_15, na.rm = TRUE),pop_access_15))
  # ap = dplyr::mutate(ap, pop_access_30 = dplyr::if_else(is.na(pop_access_30),max(pop_access_30, na.rm = TRUE),pop_access_30))
  # ap = dplyr::mutate(ap, pop_access_45 = dplyr::if_else(is.na(pop_access_45),max(pop_access_45, na.rm = TRUE),pop_access_45))
  # ap = dplyr::mutate(ap, pop_access_60 = dplyr::if_else(is.na(pop_access_60),max(pop_access_60, na.rm = TRUE),pop_access_60))
  # ap = dplyr::mutate(ap, pop_proximity_15 = dplyr::if_else(is.na(pop_proximity_15),max(pop_proximity_15, na.rm = TRUE),pop_proximity_15))
  # ap = dplyr::mutate(ap, pop_proximity_30 = dplyr::if_else(is.na(pop_proximity_30),max(pop_proximity_30, na.rm = TRUE),pop_proximity_30))
  # ap = dplyr::mutate(ap, pop_proximity_45 = dplyr::if_else(is.na(pop_proximity_45),max(pop_proximity_45, na.rm = TRUE),pop_proximity_45))
  # ap = dplyr::mutate(ap, pop_proximity_60 = dplyr::if_else(is.na(pop_proximity_60),max(pop_proximity_60, na.rm = TRUE),pop_proximity_60))
  # ap = dplyr::ungroup(ap)

  # nms = names(ap)
  # nms = nms[grepl("pop_",nms)]
  # for(i in nms){
  #   ap[,i] = dplyr::if_else(is.na(ap[,i]),max(ap[,i], na.rm = TRUE),ap[,i])
  # }


  ap_orig = ap

  # Flag Insufficient people
  nms = names(ap)
  nms = nms[grepl("access|proximity",nms)]
  nms = nms[!grepl("pop_",nms)]
  for(i in nms){
    ap[,i] = dplyr::if_else(is.na(ap[,i]) & (ap[,paste0("pop_",i)] / ap$nat_pps < 0.5),-3,ap[,i])
    ap[,i] = dplyr::if_else(is.na(ap[,i]),-2.9,ap[,i])
  }

    # Sort
  ap = ap[order(ap$LSOA21CD, ap$groupname, ap$categoryname, ap$classname),]

  ap
}

# foo = ap[ap$LSOA21CD == "E01012992",]
# qtm(zones[zones$OA21CD == "E00065509",])
# #
# tar_load(access_poi_circle_15min)
# tar_load(access_poi_circle_30min)
# tar_load(access_poi_circle_45min)
# tar_load(access_poi_circle_60min)
# tar_load(access_poi_iso_15min)
# tar_load(access_poi_iso_30min)
# tar_load(access_poi_iso_45min)
# tar_load(access_poi_iso_60min)
# tar_load(lookup_oa2021_lsoa2021)
# tar_load(area_classifications)
# # #  E01012992
#
# tar_load(ons_isochrones)
#
# zones = ons_isochrones[ons_isochrones$iso_cutoff == 900,] # 15 min
# zones = zones[zones$OA21CD %in% lookup_oa2021_lsoa2021$nearest_OA2021,]
# #
# lookup_oa2021_lsoa2021[lookup_oa2021_lsoa2021$LSOA21CD == "E01012992",]
# #
# # zones = zones[zones$OA21CD == "E00065509",]
# #
#  tar_load(poi)
#  tar_load(centroids_oa21)
#  tar_load(population_oa21)
#  pop = population_oa21
#  gb_pop = 67.33e6
#  oa = centroids_oa21

#
# x = c(-10,-3,-2,1,3,10,NA)
# dplyr::if_else(x < -3 ,-3,x)
