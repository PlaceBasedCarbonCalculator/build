summarise_access_proximity = function(access_poi_circle_15min,
                                      access_poi_iso_15min,
                                      access_poi_circle_30min,
                                      access_poi_iso_30min,
                                      access_poi_circle_45min,
                                      access_poi_iso_45min,
                                      access_poi_circle_60min,
                                      access_poi_iso_60min,
                                      lookup_oa2021_lsoa2011,
                                      area_classifications
                                      ){

  # TODO: Switch to 2021 LSOA
  lookup_oa2021_lsoa2011$nearest_OA2021_dist = NULL

  access_poi_iso_15min = dplyr::left_join(access_poi_iso_15min,
                                          lookup_oa2021_lsoa2011,
                                          by = c("OA21CD" = "nearest_OA2021"),
                                          relationship = "many-to-many")
  access_poi_iso_15min$OA21CD = NULL

  access_poi_iso_30min = dplyr::left_join(access_poi_iso_30min,
                                          lookup_oa2021_lsoa2011,
                                          by = c("OA21CD" = "nearest_OA2021"),
                                          relationship = "many-to-many")
  access_poi_iso_30min$OA21CD = NULL

  access_poi_iso_45min = dplyr::left_join(access_poi_iso_45min,
                                          lookup_oa2021_lsoa2011,
                                          by = c("OA21CD" = "nearest_OA2021"),
                                          relationship = "many-to-many")
  access_poi_iso_45min$OA21CD = NULL

  access_poi_iso_60min = dplyr::left_join(access_poi_iso_60min,
                                          lookup_oa2021_lsoa2011,
                                          by = c("OA21CD" = "nearest_OA2021"),
                                          relationship = "many-to-many")
  access_poi_iso_60min$OA21CD = NULL


  corenms = c("LSOA11CD","groupname","categoryname","classname")

  access_poi_iso_15min = access_poi_iso_15min[,c(corenms,"sp10kp_diff_SD")]
  access_poi_circle_15min = access_poi_circle_15min[,c(corenms,"sp10kp_diff_SD")]
  names(access_poi_iso_15min)[5] = "access_15"
  names(access_poi_circle_15min)[5] = "proximity_15"

  access_poi_iso_30min = access_poi_iso_30min[,c(corenms,"sp10kp_diff_SD")]
  access_poi_circle_30min = access_poi_circle_30min[,c(corenms,"sp10kp_diff_SD")]
  names(access_poi_iso_30min)[5] = "access_30"
  names(access_poi_circle_30min)[5] = "proximity_30"

  access_poi_iso_45min = access_poi_iso_45min[,c(corenms,"sp10kp_diff_SD")]
  access_poi_circle_45min = access_poi_circle_45min[,c(corenms,"sp10kp_diff_SD")]
  names(access_poi_iso_45min)[5] = "access_45"
  names(access_poi_circle_45min)[5] = "proximity_45"

  access_poi_iso_60min = access_poi_iso_60min[,c(corenms,"sp10kp_diff_SD")]
  access_poi_circle_60min = access_poi_circle_60min[,c(corenms,"sp10kp_diff_SD")]
  names(access_poi_iso_60min)[5] = "access_60"
  names(access_poi_circle_60min)[5] = "proximity_60"


  ap = dplyr::full_join(access_poi_iso_15min, access_poi_circle_15min, by = corenms)
  ap = dplyr::full_join(ap, access_poi_iso_30min, by = corenms)
  ap = dplyr::full_join(ap, access_poi_circle_30min, by = corenms)
  ap = dplyr::full_join(ap, access_poi_iso_45min, by = corenms)
  ap = dplyr::full_join(ap, access_poi_circle_45min, by = corenms)
  ap = dplyr::full_join(ap, access_poi_iso_60min, by = corenms)
  ap = dplyr::full_join(ap, access_poi_circle_60min, by = corenms)


  # Add Missing combincations
  miss = access_poi_circle_60min[!duplicated(access_poi_circle_60min$classname),]
  miss = miss[,c("groupname","categoryname","classname")]
  miss = miss[rep(1:nrow(miss), 34753),] # 34753 LSOA in England and Wales
  miss$LSOA11CD = rep(unique(lookup_oa2021_lsoa2011$LSOA11CD), each = 385) # 385 number of location types

  ap = dplyr::full_join(ap, miss, by = c("LSOA11CD","groupname","categoryname","classname"))
  ap = as.data.frame(ap)

  lim = 3
  #Crop to +/- lim
  nms = names(ap)
  nms = nms[grepl("access|proximity",nms)]
  for(i in nms){
    ap[,i] = dplyr::if_else(is.na(ap[,i]),-lim,ap[,i])
    ap[,i] = dplyr::if_else(ap[,i] < -lim ,-lim,ap[,i])
    ap[,i] = dplyr::if_else(ap[,i] > lim ,lim,ap[,i])
  }


  ap


}
