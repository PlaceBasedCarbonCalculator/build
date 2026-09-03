#' Write the per-zone overview data binary
#'
#' @description Builds a small JSON record per zone giving its administrative
#'   context (ward, parish, constituency, local authority), area
#'   classification and any data-quality warning codes, then writes them as a
#'   date-stamped `lsoa_overview` bin + index in `outputdata/jsonbin` via
#'   `export_zone_bin()`. Used by the `build_overview_jsons` target.
#' @param lsoa_admin Zone-to-admin lookup (`lsoa_admin` target).
#' @param area_names Canonical ward and parish names (`area_names` target).
#' @param area_classifications_11_21 Classifications on 2021 zones
#'   (`area_classifications_11_21` target).
#' @param lsoa_warnings Warning codes (`lsoa_warnings` target).
#' @return The output paths from `export_zone_bin()`.
#' @keywords internal
make_lsoa_overview_json = function(lsoa_admin, area_names, area_classifications_11_21,
                                   lsoa_warnings){

  
  # Include the administrative-area *codes* as well as names, so the website can
  # link an LSOA popup straight to the ward / parish / constituency / LA report
  # (which are keyed by WD25CD / PAR23CD / PCON24CD / LAD25CD).
  lsoa_admin = lsoa_admin[,c("LSOA21CD",
                             "WD25CD","WD25NM",
                             "PAR23CD","PAR23NM",
                             "PCON24CD","PCON24NM",
                             "LAD25CD","LAD25NM")]

  # An LSOA is shown the ward and parish holding its centroid, but under the
  # name the ward and parish reports themselves use, so the link text matches
  # the page it opens (see build_area_names())
  wd = area_names$ward$name[match(lsoa_admin$WD25CD, area_names$ward$id)]
  lsoa_admin$WD25NM[!is.na(wd)] = wd[!is.na(wd)]
  par = area_names$parish$name[match(lsoa_admin$PAR23CD, area_names$parish$id)]
  lsoa_admin$PAR23NM[!is.na(par)] = par[!is.na(par)]
  lsoa = dplyr::left_join(lsoa_admin, area_classifications_11_21, by = "LSOA21CD")


  lsoa$warnings = vector("list", nrow(lsoa))
  for(i in 1:nrow(lsoa)){
    sub = lsoa_warnings[lsoa_warnings$LSOA21CD == lsoa$LSOA21CD[i],]
    if(nrow(sub) > 0){
      lsoa$warnings[[i]] = sub$warningcode
    }
  }

  export_zone_bin(lsoa, idcol = "LSOA21CD", rounddp = 1, name = "lsoa_overview", dataframe = "rows")


}
