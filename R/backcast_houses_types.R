# tar_load(dwellings_tax_band)
# tar_load(dwellings_type)


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


# lsoa = "E01034220"
#
#
# sub_tax = dwellings_tax_band[dwellings_tax_band$ecode == lsoa,]
# sub_tax$all_properties = NULL
#
# sub = dwellings_type[dwellings_type$ecode == lsoa,]
#
#
# sub_tax_2020 = sub_tax[sub_tax$year == 2020,]
# sub = sub[sub$year == 2020,]
# sub = sub[,c("ecode","band","bungalow_total","flat_mais_total","house_terraced_total",
#              "house_semi_total","house_detached_total","annexe","caravan_houseboat_mobilehome", "unknown")]
# sub = sub[sub$band != "All",]
# sub_I = sub[1,]
# sub_I$band = "I"
# sub_I[3:ncol(sub_I)] = lapply(sub_I[3:ncol(sub_I)],function(x){0})
# sub = rbind(sub, sub_I)
#
#
# # > head(sub)
# # # A tibble: 6 × 10
# # ecode     band  bungalow_total flat_mais_total house_terraced_total house_semi_total house_detached_total annexe caravan_houseboat_mobilehome unknown
# # <chr>     <chr>          <int>           <int>                <int>            <int>                <int>  <int>                        <int>   <int>
# #   1 E01034220 A                  0              90                    0                0                    0      0                            0       0
# # 2 E01034220 B                  0              10                    0                0                    0      0                            0       0
# # 3 E01034220 C                  0             290                    0                0                    0      0                            0       0
# # 4 E01034220 D                  0             330                    0                0                    0      0                            0      10
# # 5 E01034220 E                  0             140                    0                0                    0      0                            0       0
# # 6 E01034220 F                  0             100                    0                0                    0      0                            0       0
#
# # head(sub_tax)
# # A tibble: 6 × 12
# # ecode      year band_a band_b band_c band_d band_e band_f band_g band_h band_i
# # <chr>     <int>  <int>  <int>  <int>  <int>  <int>  <int>  <int>  <int>  <int>
# # 1 E01034220  1993      0      0      0      0      0      0      0      0     NA
# # 2 E01034220  1994      0      0      0      0      0      0      0      0     NA
# # 3 E01034220  1995      0      0      0      0      0      0      0      0     NA
# # 4 E01034220  1996      0      0      0      0      0      0      0      0     NA
# # 5 E01034220  1997      0      0      0      0      0      0      0      0     NA
# # 6 E01034220  1998      0      0      0      0      0      0      0      0     NA
#
# # Make a function that works out the number of each typ of house in sub exists in each year of sub_tax
#
#
#
