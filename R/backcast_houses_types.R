# tar_load(dwellings_tax_band)
# tar_load(dwellings_type)


#' Backcast dwelling-type counts to 2010-2019 from council tax bands
#'
#' @description Dwelling types (bungalow/flat/terraced/semi/detached etc.)
#'   are only published from 2020 (CTSOP3), but dwellings per council tax
#'   band go back further (CTSOP1). For each LSOA this estimates the type mix
#'   for 2010-2019 by scaling the 2020 type-by-band matrix to match each
#'   year's band totals (`build_backcasts_dwellings()`), then appends the
#'   observed 2020+ data. Used by the `dwellings_type_backcast` target, an
#'   input to `match_LCFS_synth_pop()`.
#' @param dwellings_tax_band CTSOP1 table (`dwellings_tax_band` target).
#' @param dwellings_type CTSOP3 table (`dwellings_type` target).
#' @return A data frame per `lsoa21cd`-year of dwelling-type counts.
#' @keywords internal
backcast_dwelling_types = function(dwellings_tax_band, dwellings_type){

  dwellings_tax_band = dwellings_tax_band[dwellings_tax_band$year > 2009 &
                                            dwellings_tax_band$year < 2021,]
  dwellings_tax_band$all_properties = NULL
  dwellings_tax_band$band_i[is.na(dwellings_tax_band$band_i)] = 0

  dwellings_type = dwellings_type[,c("ecode","year","band","bungalow_total","flat_mais_total",
                                     "house_terraced_total","house_semi_total",
                                     "house_detached_total","annexe",
                                     "caravan_houseboat_mobilehome", "unknown")]
  names(dwellings_type) = gsub("_total","",names(dwellings_type))

  dwellings_type_post20 = dwellings_type[dwellings_type$year > 2019,]
  dwellings_type_post20 = dwellings_type_post20[dwellings_type_post20$band == "All",]
  dwellings_type_post20$band = NULL

  dwellings_type = dwellings_type[dwellings_type$band != "All",]
  dwellings_type = dwellings_type[dwellings_type$year == 2020,]

  dwellings_type_lst = dplyr::group_split(dwellings_type, ecode)
  dwellings_tax_band_lst = dplyr::group_split(dwellings_tax_band, ecode)


  res = purrr::map2(.x = dwellings_type_lst,
                    .y = dwellings_tax_band_lst,
                    .f = build_backcasts_dwellings,
                    .progress = TRUE)
  res = data.table::rbindlist(res)
  res = as.data.frame(res)

  names(dwellings_type_post20)[names(dwellings_type_post20) == "ecode"] = "lsoa21cd"

  res = rbind(res, dwellings_type_post20)
  res = res[order(res$lsoa21cd, res$year),]
  res

}


#' Backcast one LSOA's dwelling types from its band totals
#'
#' @description Worker for `backcast_dwelling_types()`. Builds the 2020
#'   type-by-band matrix for one LSOA (adding an empty band I if absent) and,
#'   for each year 2010-2019, rescales its columns to that year's band counts
#'   with `match_matrix_csums()`, summing rows to get type totals.
#' @param sub 2020 dwelling types by band for one LSOA.
#' @param sub_tax Dwellings by band per year for the same LSOA.
#' @return A wide data frame (`lsoa21cd`, `year`, one column per type).
#' @keywords internal
build_backcasts_dwellings = function(sub, sub_tax){

  if(!all(unique(sub$ecode) %in% unique(sub_tax$ecode))){
    stop("LSOA don't match ", unique(sub$ecode))
  }

  if(!"I" %in% sub$band){
    sub_I = sub[1,]
    sub_I$band = "I"
    sub_I[3:ncol(sub_I)] = lapply(sub_I[3:ncol(sub_I)],function(x){0})
    sub = rbind(sub, sub_I)
  }


  mat_sub = t(as.matrix(sub[,c("bungalow","flat_mais","house_terraced","house_semi","house_detached","annexe", "caravan_houseboat_mobilehome","unknown")]))
  colnames(mat_sub) = sub$band

  mat_past = list()

  for(i in 1:10){
    mat_yr = sub_tax[sub_tax$year == i + 2009,]
    mat_yr = as.matrix(mat_yr[,c("band_a","band_b","band_c","band_d","band_e","band_f","band_g","band_h","band_i")])
    mat_yr[is.na(mat_yr)] = 0
    mat_yr = match_matrix_csums(mat1 = mat_yr, mat2 = mat_sub)
    mat_yr = as.data.frame(rowSums(mat_yr))
    mat_yr$building_type = rownames(mat_yr)
    rownames(mat_yr) = NULL
    names(mat_yr)[1] = "properties"
    mat_yr$year = i + 2009
    mat_past[[i]] = mat_yr
  }

  mat_past = dplyr::bind_rows(mat_past)

  mat_past$lsoa21cd = sub$ecode[1]

  mat_past = tidyr::pivot_wider(mat_past, names_from = "building_type", values_from = "properties")

  mat_past

}


