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


load_OAC21 = function(path = file.path(parameters$path_data,"area_classifications/oac21ew.csv")){
  oac21 = readr::read_csv(path)
  oac21
}

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

  lsoa21b

  # Old method

  # lookup_postcode_OA_LSOA_MSOA_2021 = lookup_postcode_OA_LSOA_MSOA_2021[,c("oa21cd","lsoa21cd")]
  # lookup_postcode_OA_LSOA_MSOA_2021 = lookup_postcode_OA_LSOA_MSOA_2021[!duplicated(lookup_postcode_OA_LSOA_MSOA_2021$oa21cd),]
  #
  # lookup_OA_LSOA_MSOA_classifications = lookup_OA_LSOA_MSOA_classifications[,c("OA11CD","OAC11CD","OAC11NM","LSOA11CD")]
  #
  # oas_unchanges = lookup_OA_LSOA_MSOA_classifications[lookup_OA_LSOA_MSOA_classifications$OA11CD %in% lookup_postcode_OA_LSOA_MSOA_2021$oa21cd,]
  # oas_changes = lookup_OA_LSOA_MSOA_classifications[!lookup_OA_LSOA_MSOA_classifications$OA11CD %in% lookup_postcode_OA_LSOA_MSOA_2021$oa21cd,]
  #
  # lookup_lsoa_2011_21 = lookup_lsoa_2011_21[,c("LSOA11CD","LSOA21CD","CHGIND")]
  # lookup_lsoa_2011_21 = lookup_lsoa_2011_21[!duplicated(lookup_lsoa_2011_21$LSOA11CD),]
  #
  # oas_changes = dplyr::left_join(oas_changes, lookup_lsoa_2011_21, by = c("LSOA11CD"))
  #
  # oas_unchanges = dplyr::left_join(oas_unchanges, lookup_lsoa_2011_21, by = c("LSOA11CD"))
  #
  # oac21 = rbind(oas_unchanges, oas_changes)
  #
  # lsoa21 = dplyr::group_by(oac21, LSOA21CD)
  # lsoa21 = dplyr::summarise(lsoa21,
  #                           OAC11CD = list(as.data.frame(table(OAC11CD))))
  # lsoa21

}

read_centroids_oa11 = function(path = "../inputdata/boundaries/"){
  oa = sf::st_read(file.path(path,"Output_Areas_Dec_2011_PWC_2022_2937497644548359762.gpkg"))
  oa$GlobalID = NULL
  oa
}

