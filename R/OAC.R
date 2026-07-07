#' Carry the 2011 LSOA classification (SOAC) name onto 2021 zones
#'
#' @description Assigns each 2021 LSOA the SOAC11 classification name of its
#'   first-matching 2011 LSOA; Scottish 2011 Data Zones pass through with
#'   their own code. Used by the `SOAC_11` target.
#' @param lookup_OA_LSOA_MSOA_classifications 2011 lookup with `SOAC11NM`.
#' @param lookup_lsoa_2011_21 ONS 2011-to-2021 LSOA lookup.
#' @return A data frame with `LSOA21CD`, `LSOA11CD` and `SOAC11NM`.
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


#' Load the 2021 Output Area Classification (OAC21) for England & Wales
#'
#' @description Reads the OAC 2021 CSV (supergroup/group/subgroup per 2021
#'   OA). Used by the `oac21` target.
#' @param path Path to `oac21ew.csv`.
#' @return A data frame of OAC21 codes per `oa21cd`.
#' @keywords internal
load_OAC21 = function(path = file.path(parameters$path_data,"area_classifications/oac21ew.csv")){
  oac21 = readr::read_csv(path)
  oac21
}

#' Summarise the OA-level OAC21 mix within each 2021 LSOA
#'
#' @description Joins the OAC21 codes to LSOAs via the OA lookup and stores,
#'   for each LSOA, frequency tables of the supergroups/groups/subgroups of
#'   its OAs as list-columns. Used by the `lsoa21_OAC21_summary` target.
#' @param oac21 OAC21 table (`oac21` target).
#' @param lookup_postcode_OA_LSOA_MSOA_2021 Postcode/OA/LSOA lookup.
#' @return A data frame with `lsoa21cd` and list-columns `supergroup`,
#'   `group`, `subgroup` of frequency tables.
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

#' Summarise the 2011 OAC mix within each 2022 Scottish Data Zone
#'
#' @description Assigns 2011 Scottish OA centroids to 2022 Data Zones by
#'   point-in-polygon (with a 50 m buffer retry for coastal misses), then
#'   stores each zone's OAC11 code frequency table as a list-column. Zones
#'   containing no OA centroid inherit the nearest OA's code. Used by the
#'   `oac11dz22` target, which matches LCFS households by area type.
#' @param centroids_oa11_scotland Scottish 2011 OA centroids.
#' @param bounds_dz22 2022 Data Zone boundaries.
#' @param lookup_OA_LSOA_MSOA_classifications 2011 lookup with `OAC11CD`.
#' @return A data frame with `LSOA21CD` (2022 DZ) and list-column `OAC`.
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

#' Summarise the 2011 OAC mix within each 2021 E&W LSOA
#'
#' @description As `OAC11_dz22()` but for England & Wales: 2011 OA centroids
#'   are assigned to 2021 LSOAs (50 m buffer retry), each LSOA gets a
#'   frequency table of OAC11 codes, and LSOAs without a centroid inherit
#'   the nearest OA's code. Used by the `oac11lsoa21` target.
#' @param centroids_oa11 E&W 2011 OA centroids (`centroids_oa11` target).
#' @param bounds_lsoa21_full 2021 LSOA boundaries.
#' @param lookup_OA_LSOA_MSOA_classifications 2011 lookup with `OAC11CD`.
#' @return A data frame with `LSOA21CD` and list-column `OAC`.
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

#' Summarise the 2001 OAC mix within each 2021 E&W LSOA
#'
#' @description As `OAC11_lsoa21()` but using 2001 Output Areas and the 2001
#'   OAC (subgroup codes), with a wider 130 m buffer retry. Used by the
#'   `oac01lsoa21` target, which supports LCFS matching for survey years
#'   before 2014.
#' @param centroids_oa01 E&W 2001 OA centroids (`centroids_oa01` target).
#' @param bounds_lsoa21_full 2021 LSOA boundaries.
#' @param oac01 2001 OAC table (`oac01` target).
#' @return A data frame with `LSOA21CD` and list-column `OAC`.
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


#' Summarise the 2001 OAC mix within each 2022 Scottish Data Zone
#'
#' @description As `OAC01_lsoa21()` but for Scotland's 2001 OAs and 2022
#'   Data Zones. Used by the `oac01dz22` target.
#' @param centroids_oa01_scotland Scottish 2001 OA centroids.
#' @param bounds_dz22 2022 Data Zone boundaries.
#' @param oac01 2001 OAC table (`oac01` target).
#' @return A data frame with `LSOA21CD` (2022 DZ) and list-column `OAC`.
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


#' Read 2011 Output Area population-weighted centroids (England & Wales)
#'
#' @description Reads the 2011 OA PWC GeoPackage from the boundaries folder.
#'   Used by the `centroids_oa11` target.
#' @param path Boundaries folder (the `dl_boundaries` target).
#' @return An sf POINT data frame with `OA11CD`.
#' @keywords internal
read_centroids_oa11 = function(path = "../inputdata/boundaries/"){
  oa = sf::st_read(file.path(path,"Output_Areas_Dec_2011_PWC_2022_2937497644548359762.gpkg"))
  oa$GlobalID = NULL
  oa
}

#' Read 2001 Output Area population-weighted centroids (England & Wales)
#'
#' @description Reads the 2001 OA PWC GeoPackage from the boundaries folder.
#'   Used by the `centroids_oa01` target.
#' @param path Boundaries folder (the `dl_boundaries` target).
#' @return An sf POINT data frame including `OA01CDOLD`.
#' @keywords internal
read_centroids_oa01 = function(path = "../inputdata/boundaries/"){
  oa = sf::st_read(file.path(path,"Output_Areas_2001_EW_PWC_6679101571236103446.gpkg"))
  oa$GlobalID = NULL
  oa
}


#' Read the 2001 Output Area Classification table
#'
#' @description Reads the pre-saved 2001 OAC Rds (subgroup codes per 2001
#'   OA). Used by the `oac01` target.
#' @param path Path to `OAC_2001.Rds`.
#' @return A data frame with `OA_CODE` and classification columns.
#' @keywords internal
read_OAC01 = function(path = "../inputdata/area_classifications/2001/OAC_2001.Rds"){
  oac = readRDS(path)
  oac
}
