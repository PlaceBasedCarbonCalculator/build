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
