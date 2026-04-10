#' Make Lsoa Overview Json
#'
#' @description Build lsoa overview json and return the generated output.
#' @param lsoa_admin Input object or parameter named `lsoa_admin`.
#' @param area_classifications_11_21 Input object or parameter named `area_classifications_11_21`.
#' @param lsoa_warnings){ Input object or parameter named `lsoa_warnings){`.
#' @return A generated data object, usually a data frame or spatial feature collection.
#' @keywords internal
make_lsoa_overview_json = function(lsoa_admin, area_classifications_11_21, lsoa_warnings){

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
