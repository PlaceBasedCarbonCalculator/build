#' Oac To 2021
#'
#' @description Perform processing for OAC to 2021.
#' @param lookup_OA_LSOA_MSOA_classifications Lookup table used to map area codes or classifications.
#' @param lookup_lsoa_2011_21){ Lookup table used to map area codes or classifications.
#' @return An sf object containing spatial data.
#' @keywords internal
OAC_to_2021 = function(lookup_OA_LSOA_MSOA_classifications, lookup_lsoa_2011_21){

  lookup_lsoa_2011_21 = lookup_lsoa_2011_21[,c("LSOA11CD","LSOA21CD")]
  lookup_lsoa_2011_21 = lookup_lsoa_2011_21[!duplicated(lookup_lsoa_2011_21$LSOA21CD),]

  oac = lookup_OA_LSOA_MSOA_classifications[,c("LSOA11CD","SOAC11NM")]
  oac = oac[!duplicated(oac$LSOA11CD),]

  oac_scot = oac[substr(oac$LSOA11CD,1,1) == "S",]
  oac_scot$LSOA21CD = oac_scot$LSOA11CD

  oas_21 = dplyr::left_join(lookup_lsoa_2011_21, oac, by = "LSOA11CD")

  final = rbind(oas_21, oac_scot)
  final

}


#' Load Oac21
#'
#' @description Load OAC21 data from the source path and return it as an R object.
#' @details This function is used as part of the pipeline input ingestion stage.
#' @param path File or directory path.
#' @return An sf object containing the loaded spatial data.
#' @keywords internal
load_OAC21 = function(path = file.path(parameters$path_data,"area_classifications/oac21ew.csv")){
  oac21 = readr::read_csv(path)
  oac21
}

#' Oac21 Lsoa21
#'
#' @description Perform processing for OAC21 lsoa21.
#' @param oac21 Input object or parameter named `oac21`.
#' @param lookup_postcode_OA_LSOA_MSOA_2021){ Lookup table used to map area codes or classifications.
#' @return An sf object containing spatial data.
#' @keywords internal
OAC21_lsoa21 = function(oac21, lookup_postcode_OA_LSOA_MSOA_2021){

  lookup_postcode_OA_LSOA_MSOA_2021 = lookup_postcode_OA_LSOA_MSOA_2021[,c("oa21cd","lsoa21cd")]
  lookup_postcode_OA_LSOA_MSOA_2021 = lookup_postcode_OA_LSOA_MSOA_2021[!duplicated(lookup_postcode_OA_LSOA_MSOA_2021$oa21cd),]

  oac21 = dplyr::left_join(oac21, lookup_postcode_OA_LSOA_MSOA_2021, by = "oa21cd")

  lsoa21 = dplyr::group_by(oac21, lsoa21cd)
  lsoa21 = dplyr::summarise(lsoa21,
                            supergroup = list(as.data.frame(table(supergroup))),
                            group = list(as.data.frame(table(group))),
                            subgroup = list(as.data.frame(table(subgroup)))

                            )
  lsoa21

}

#' Oac11 Dz22
#'
#' @description Perform processing for OAC11 dz22.
#' @param centroids_oa11_scotland Centroid geometries used for area matching.
#' @param bounds_dz22 Input object or parameter named `bounds_dz22`.
#' @param lookup_OA_LSOA_MSOA_classifications){ Lookup table used to map area codes or classifications.
#' @return An sf object containing spatial data.
#' @keywords internal
OAC11_dz22 = function(centroids_oa11_scotland, bounds_dz22, lookup_OA_LSOA_MSOA_classifications){

  lookup_OA_LSOA_MSOA_classifications = lookup_OA_LSOA_MSOA_classifications[,c("OA11CD","OAC11CD")]

  oa = sf::st_join(centroids_oa11_scotland, bounds_dz22)
  oa_buff = oa[is.na(oa$DataZone22),]
  oa_buff$DataZone22 = NULL
  oa_buff = sf::st_buffer(oa_buff, 50)
  oa_buff = sf::st_join(oa_buff, bounds_dz22)
  oa_buff = oa_buff[!duplicated(oa_buff$OA11),]

  oa = oa[!is.na(oa$DataZone22),]

  oa = sf::st_drop_geometry(oa)
  oa_buff = sf::st_drop_geometry(oa_buff)

  oa = rbind(oa, oa_buff)

  oa = dplyr::left_join(oa, lookup_OA_LSOA_MSOA_classifications, by = c("OA11" = "OA11CD"))

  lsoa21 = dplyr::group_by(oa, DataZone22)
  lsoa21 = dplyr::summarise(lsoa21,
                            OAC11CD = list(as.data.frame(table(OAC11CD))))


  lsoa21_missing = bounds_dz22[!bounds_dz22$DataZone22 %in% lsoa21$DataZone22,]

  nn = nngeo::st_nn(lsoa21_missing, centroids_oa11_scotland)

  lsoa21_missing$OA11CD = centroids_oa11_scotland$OA11[unlist(nn)]
  lsoa21_missing = dplyr::left_join(lsoa21_missing, lookup_OA_LSOA_MSOA_classifications, by = c("OA11CD"))

  lsoa21_missing$OAC11CD = lapply(lsoa21_missing$OAC11CD, function(x){
    x = as.data.frame(table(x))
    names(x) = c("OAC11CD","Freq")
    x
  })

  lsoa21_missing = sf::st_drop_geometry(lsoa21_missing)
  lsoa21_missing$OA11CD = NULL

  lsoa21b = rbind(lsoa21, lsoa21_missing)

  names(lsoa21b) = c("LSOA21CD","OAC")

  lsoa21b

}

#' Oac11 Lsoa21
#'
#' @description Perform processing for OAC11 lsoa21.
#' @param centroids_oa11 Centroid geometries used for area matching.
#' @param bounds_lsoa21_full Input object or parameter named `bounds_lsoa21_full`.
#' @param lookup_OA_LSOA_MSOA_classifications){ Lookup table used to map area codes or classifications.
#' @return An sf object containing spatial data.
#' @keywords internal
OAC11_lsoa21 = function(centroids_oa11, bounds_lsoa21_full, lookup_OA_LSOA_MSOA_classifications){

  bounds_lsoa21_full$LSOA21NM = NULL
  lookup_OA_LSOA_MSOA_classifications = lookup_OA_LSOA_MSOA_classifications[,c("OA11CD","OAC11CD")]

  oa = sf::st_join(centroids_oa11, bounds_lsoa21_full)
  oa_buff = oa[is.na(oa$LSOA21CD),]
  oa_buff$LSOA21CD = NULL
  oa_buff$LSOA21NM = NULL
  oa_buff = sf::st_buffer(oa_buff, 50)
  oa_buff = sf::st_join(oa_buff, bounds_lsoa21_full)
  oa_buff = oa_buff[!duplicated(oa_buff$OA11CD),]

  oa = oa[!is.na(oa$LSOA21CD),]

  oa = sf::st_drop_geometry(oa)
  oa_buff = sf::st_drop_geometry(oa_buff)

  oa = rbind(oa, oa_buff)

  oa = dplyr::left_join(oa, lookup_OA_LSOA_MSOA_classifications, by = c("OA11CD"))

  lsoa21 = dplyr::group_by(oa, LSOA21CD)
  lsoa21 = dplyr::summarise(lsoa21,
                            OAC11CD = list(as.data.frame(table(OAC11CD))))


  lsoa21_missing = bounds_lsoa21_full[!bounds_lsoa21_full$LSOA21CD %in% lsoa21$LSOA21CD,]

  nn = nngeo::st_nn(lsoa21_missing, centroids_oa11)

  lsoa21_missing$OA11CD = centroids_oa11$OA11CD[unlist(nn)]
  lsoa21_missing = left_join(lsoa21_missing, lookup_OA_LSOA_MSOA_classifications, by = c("OA11CD"))

  lsoa21_missing$OAC11CD = lapply(lsoa21_missing$OAC11CD, function(x){
    x = as.data.frame(table(x))
    names(x) = c("OAC11CD","Freq")
    x
  })

  lsoa21_missing = sf::st_drop_geometry(lsoa21_missing)
  lsoa21_missing$OA11CD = NULL

  lsoa21b = rbind(lsoa21, lsoa21_missing)

  names(lsoa21b) = c("LSOA21CD","OAC")

  lsoa21b

}

#' Oac01 Lsoa21
#'
#' @description Perform processing for OAC01 lsoa21.
#' @param centroids_oa01 Centroid geometries used for area matching.
#' @param bounds_lsoa21_full Input object or parameter named `bounds_lsoa21_full`.
#' @param oac01){ Input object or parameter named `oac01){`.
#' @return An sf object containing spatial data.
#' @keywords internal
OAC01_lsoa21 = function(centroids_oa01, bounds_lsoa21_full, oac01){

  bounds_lsoa21_full$LSOA21NM = NULL
  oac01 = oac01[,c("OA_CODE","Subgroup Code")]
  names(oac01) = c("OA01CDOLD","OAC01")

  oa = sf::st_join(centroids_oa01, bounds_lsoa21_full)
  oa_buff = oa[is.na(oa$LSOA21CD),]
  oa_buff$LSOA21CD = NULL
  oa_buff$LSOA21NM = NULL
  oa_buff = sf::st_buffer(oa_buff, 130)
  oa_buff = sf::st_join(oa_buff, bounds_lsoa21_full)
  oa_buff = oa_buff[!duplicated(oa_buff$OA01CDOLD),]

  oa = oa[!is.na(oa$LSOA21CD),]

  oa = sf::st_drop_geometry(oa)
  oa_buff = sf::st_drop_geometry(oa_buff)

  oa = rbind(oa, oa_buff)

  oa = dplyr::left_join(oa, oac01, by = c("OA01CDOLD"))

  lsoa21 = dplyr::group_by(oa, LSOA21CD)
  lsoa21 = dplyr::summarise(lsoa21,
                            OAC01 = list(as.data.frame(table(OAC01))))


  lsoa21_missing = bounds_lsoa21_full[!bounds_lsoa21_full$LSOA21CD %in% lsoa21$LSOA21CD,]

  nn = nngeo::st_nn(lsoa21_missing, centroids_oa01)

  lsoa21_missing$OA01CDOLD = centroids_oa01$OA01CDOLD[unlist(nn)]
  lsoa21_missing = dplyr::left_join(lsoa21_missing, oac01, by = c("OA01CDOLD"))

  lsoa21_missing$OAC01 = lapply(lsoa21_missing$OAC01, function(x){
    x = as.data.frame(table(x))
    names(x) = c("OAC01","Freq")
    x
  })

  lsoa21_missing = sf::st_drop_geometry(lsoa21_missing)
  lsoa21_missing$OA01CDOLD = NULL

  lsoa21b = rbind(lsoa21, lsoa21_missing)

  names(lsoa21b) = c("LSOA21CD","OAC")

  lsoa21b

}


#' Oac01 Dz22
#'
#' @description Perform processing for OAC01 dz22.
#' @param centroids_oa01_scotland Centroid geometries used for area matching.
#' @param bounds_dz22 Input object or parameter named `bounds_dz22`.
#' @param oac01){ Input object or parameter named `oac01){`.
#' @return An sf object containing spatial data.
#' @keywords internal
OAC01_dz22 = function(centroids_oa01_scotland, bounds_dz22, oac01){

  oac01 = oac01[,c("OA_CODE","Subgroup Code")]
  names(oac01) = c("OA01CDOLD","OAC01")

  oa = sf::st_join(centroids_oa01_scotland, bounds_dz22)
  oa_buff = oa[is.na(oa$DataZone22),]
  oa_buff$DataZone22 = NULL
  oa_buff = sf::st_buffer(oa_buff, 130)
  oa_buff = sf::st_join(oa_buff, bounds_dz22)
  oa_buff = oa_buff[!duplicated(oa_buff$NRSoldOutp),]

  oa = oa[!is.na(oa$DataZone22),]

  oa = sf::st_drop_geometry(oa)
  oa_buff = sf::st_drop_geometry(oa_buff)

  oa = rbind(oa, oa_buff)

  oa = dplyr::left_join(oa, oac01, by = c("NRSoldOutp" = "OA01CDOLD"))

  lsoa21 = dplyr::group_by(oa, DataZone22)
  lsoa21 = dplyr::summarise(lsoa21,
                            OAC01 = list(as.data.frame(table(OAC01))))


  lsoa21_missing = bounds_dz22[!bounds_dz22$DataZone22 %in% lsoa21$DataZone22,]

  nn = nngeo::st_nn(lsoa21_missing, centroids_oa01_scotland)

  lsoa21_missing$NRSoldOutp = centroids_oa01_scotland$NRSoldOutp[unlist(nn)]
  lsoa21_missing = dplyr::left_join(lsoa21_missing, oac01, by = c("NRSoldOutp" = "OA01CDOLD"))

  lsoa21_missing$OAC01 = lapply(lsoa21_missing$OAC01, function(x){
    x = as.data.frame(table(x))
    names(x) = c("OAC01","Freq")
    x
  })

  lsoa21_missing = sf::st_drop_geometry(lsoa21_missing)
  lsoa21_missing$NRSoldOutp = NULL

  lsoa21b = rbind(lsoa21, lsoa21_missing)

  names(lsoa21b) = c("LSOA21CD","OAC")

  lsoa21b

}


#' Read Centroids Oa11
#'
#' @description Read centroids oa11 from disk into an R object.
#' @details This function is used as part of the pipeline input ingestion stage.
#' @param path File or directory path.
#' @return An sf object containing the loaded spatial data.
#' @keywords internal
read_centroids_oa11 = function(path = "../inputdata/boundaries/"){
  oa = sf::st_read(file.path(path,"Output_Areas_Dec_2011_PWC_2022_2937497644548359762.gpkg"))
  oa$GlobalID = NULL
  oa
}

#' Read Centroids Oa01
#'
#' @description Read centroids oa01 from disk into an R object.
#' @details This function is used as part of the pipeline input ingestion stage.
#' @param path File or directory path.
#' @return An sf object containing the loaded spatial data.
#' @keywords internal
read_centroids_oa01 = function(path = "../inputdata/boundaries/"){
  oa = sf::st_read(file.path(path,"Output_Areas_2001_EW_PWC_6679101571236103446.gpkg"))
  oa$GlobalID = NULL
  oa
}


#' Read Oac01
#'
#' @description Read OAC01 from disk into an R object.
#' @details This function is used as part of the pipeline input ingestion stage.
#' @param path File or directory path.
#' @return A data frame containing the loaded dataset.
#' @keywords internal
read_OAC01 = function(path = "../inputdata/area_classifications/2001/OAC_2001.Rds"){
  oac = readRDS(path)
  oac
}
