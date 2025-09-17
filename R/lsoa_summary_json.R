make_lsoa_overview_json = function(lsoa_admin, area_classifications_11_21, lsoa_warnings){

  lsoa_admin = sf::st_drop_geometry(lsoa_admin)

  lsoa = dplyr::left_join(lsoa_admin, area_classifications_11_21, by = "LSOA21CD")


  lsoa$warnings = vector("list", nrow(lsoa))
  for(i in 1:nrow(lsoa)){
    sub = lsoa_warnings[lsoa_warnings$LSOA21CD == lsoa$LSOA21CD[i],]
    if(nrow(sub) > 0){
      lsoa$warnings[[i]] = sub$warningcode
    }
  }

  export_zone_json(lsoa, idcol = "LSOA21CD", rounddp = 1, path = "outputdata/json/lsoa_overview", dataframe = "rows",
                   reduce = FALSE, zip = FALSE)


}
