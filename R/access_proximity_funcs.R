#' Summarise Access Proximity
#'
#' @description Summarise access proximity into a compact table suitable for analysis.
#' @details This function is used to prepare intermediate analysis tables for later pipeline targets.
#' @param access_poi_circle_15min Input object or parameter named `access_poi_circle_15min`.
#' @param access_poi_iso_15min Input object or parameter named `access_poi_iso_15min`.
#' @param access_poi_circle_30min Input object or parameter named `access_poi_circle_30min`.
#' @param access_poi_iso_30min Input object or parameter named `access_poi_iso_30min`.
#' @param access_poi_circle_45min Input object or parameter named `access_poi_circle_45min`.
#' @param access_poi_iso_45min Input object or parameter named `access_poi_iso_45min`.
#' @param access_poi_circle_60min Input object or parameter named `access_poi_circle_60min`.
#' @param access_poi_iso_60min Input object or parameter named `access_poi_iso_60min`.
#' @param lookup_oa2021_lsoa2021 OA-to-LSOA lookup table.
#' @param area_classifications Area classification lookup table.
#' @return A summary data frame with aggregated metrics.
#' @keywords internal
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

  corenms = c("LSOA21CD","groupname","categoryname","classname")

  ap = dplyr::full_join(access_poi_iso_15min, access_poi_circle_15min, by = corenms)
  ap = dplyr::full_join(ap, access_poi_iso_30min, by = corenms)
  ap = dplyr::full_join(ap, access_poi_circle_30min, by = corenms)
  ap = dplyr::full_join(ap, access_poi_iso_45min, by = corenms)
  ap = dplyr::full_join(ap, access_poi_circle_45min, by = corenms)
  ap = dplyr::full_join(ap, access_poi_iso_60min, by = corenms)
  ap = dplyr::full_join(ap, access_poi_circle_60min, by = corenms)


  # Add Missing combinations
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


#
# x = c(-10,-3,-2,1,3,10,NA)
# dplyr::if_else(x < -3 ,-3,x)
