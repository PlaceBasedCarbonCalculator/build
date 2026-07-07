#' Build household types (community photo) for Scottish Data Zones
#'
#' @description Scottish equivalent of `build_household_types()`: estimates
#'   households per 2022 Data Zone by 10-category household composition,
#'   5-group NS-SEC and 3-group ethnicity from the Scotland Census 2022. IPF
#'   (`mipfp::Ipfp`) is run in two stages: first at Intermediate Zone level
#'   (composition x NS-SEC cross-tab constrained to the national three-way
#'   seed and IZ ethnicity), then Data Zone level constrained to the IZ result
#'   and DZ marginals. Runs in parallel via furrr. Used by the
#'   `household_clusters_scot` target, feeding the community-pictures JSON.
#' @param path Folder of Scotland Census 2022 CSV extracts.
#' @param bounds_iz22 2022 Intermediate Zone boundaries (`bounds_iz22`
#'   target); used to match IZ names to codes.
#' @param lookup_DataZone_2022 Data Zone lookup (`lookup_DataZone_2022`
#'   target) linking DZ22 to IZ22 codes.
#' @return A data frame with `DataZone`, `householdComp10`, `NSSEC5`,
#'   `ethnic3`, `households`, plus IPF convergence (`conv`) and error (`MAE`)
#'   diagnostics.
#' @keywords internal
make_community_photo_scotland = function(path = "../inputdata/population_scotland/", bounds_iz22, lookup_DataZone_2022){

  bounds_iz22 = sf::st_drop_geometry(bounds_iz22)
  lookup_DataZone_2022 = lookup_DataZone_2022[,c("DZ22_Code","IZ22_Code")]


  # Scotland
  hhComp_nssec_ethnic_scot = read_hhComp_nssec_ethnic_scot(file.path(path,"scotlandcenus2022_householdComp10_nssec10_ethnic8_Scotland.csv"))

  #IZ
  hhcomp_nssec = read_hhComp_nssec_iz_scot(file.path(path,"scotlandcensus2022_NSSEC10_householdComp10_IntermiedateZone_wide2.csv"), bounds_iz22)
  ethnic8 = read_ethnic8_iz_scot(file.path(path,"scotlandcenus2022_ethnic8_IntermiedateZone.csv"), bounds_iz22)

  # Reduce Variables
  ethnic8$white = rowSums(ethnic8[,c("WhiteScottish","WhiteBritish","OtherWhite")])
  ethnic8$black = rowSums(ethnic8[,c("African","Carribbean")])
  ethnic8$other = rowSums(ethnic8[,c("Mixed","Asian","Other")])

  ethnic8 = ethnic8[,c("IZCode","white","black","other")]

  hhcomp_nssec$NSSEC5 = nssec10_to_nssec5(hhcomp_nssec$NSSEC10)



  hhcomp_nssec = hhcomp_nssec |>
    dplyr::group_by(IZCode, NSSEC5, householdComp10) |>
    dplyr::summarise(households = sum(households))

  hhcomp_nssec = hhcomp_nssec[hhcomp_nssec$NSSEC5 != "DNA",]

  # Pivot
  hhComp_nssec_ethnic_scot = hhComp_nssec_ethnic_scot[,!grepl("DNA",names(hhComp_nssec_ethnic_scot))]

  hhComp_nssec_ethnic_scot = hhComp_nssec_ethnic_scot |>
    tidyr::pivot_longer(-householdComp10,
                        names_sep = "_",
                        values_to = "households",
                        names_to = c("NSSEC10","ethnic8"))

  hhComp_nssec_ethnic_scot$NSSEC5 = nssec10_to_nssec5(hhComp_nssec_ethnic_scot$NSSEC10)
  hhComp_nssec_ethnic_scot$ethnic8[hhComp_nssec_ethnic_scot$ethnic8 %in% c("WhiteScottish","WhiteBritish","OtherWhite")] = "white"
  hhComp_nssec_ethnic_scot$ethnic8[hhComp_nssec_ethnic_scot$ethnic8 %in% c("African","Carribbean")] = "black"
  hhComp_nssec_ethnic_scot$ethnic8[hhComp_nssec_ethnic_scot$ethnic8 %in% c("Mixed","Asian","Other")] = "other"

  hhComp_nssec_ethnic_scot = hhComp_nssec_ethnic_scot[hhComp_nssec_ethnic_scot$ethnic8 != "Total",]

  hhComp_nssec_ethnic_scot = hhComp_nssec_ethnic_scot |>
    dplyr::group_by(householdComp10, NSSEC5, ethnic8) |>
    dplyr::summarise(households = sum(households))


  hhcomp_nssec = hhcomp_nssec[order(hhcomp_nssec$IZCode),]
  ethnic8 = ethnic8[order(ethnic8$IZCode),]

  hhcomp_nssec = dplyr::group_split(dplyr::ungroup(hhcomp_nssec), IZCode)
  ethnic8 = dplyr::group_split(dplyr::ungroup(ethnic8), IZCode)

  # Combine
  future::plan("multisession")
  res_all = furrr::future_pmap(.l = list(hhns = hhcomp_nssec,
                                         eth = ethnic8),
                               .f = syth_communit_photo_scot,
                               seed = hhComp_nssec_ethnic_scot,
                               .progress = TRUE,  .options = furrr::furrr_options(seed = 1234))
  future::plan("sequential")
  res_all = dplyr::bind_rows(res_all)

  dz_nssec10 = read_nssec10_dz_scot(file.path(path,"scotlandcenus2022_nssec10_DataZone.csv"))
  dz_HouseholdComp = read_hhcomp10_dz_scot(file.path(path,"scotlandcenus2022_householdComp10_DataZone.csv"))
  dz_ethnic2 = read_ethnic2_dz_scot(file.path(path,"scotlandcensus2022_Ethnic2_People_DataZone.csv"))

  dz_nssec10 = dplyr::left_join(dz_nssec10, lookup_DataZone_2022, by = c("DataZone" = "DZ22_Code"))
  dz_HouseholdComp = dplyr::left_join(dz_HouseholdComp, lookup_DataZone_2022, by = c("DataZone" = "DZ22_Code"))
  dz_ethnic2 = dplyr::left_join(dz_ethnic2, lookup_DataZone_2022, by = c("DataZone" = "DZ22_Code"))

  dz_nssec10 = tidyr::pivot_longer(dz_nssec10,
                                   cols = c("DNA","L1L2L3","L4L5L6","L7","L8L9",
                                            "L10L11","L12","L13","L14","L15"),
                                   names_to = "nssec10",
                                   values_to = "households"
                                   )
  dz_nssec10$NSSEC5 = nssec10_to_nssec5(dz_nssec10$nssec10)

  dz_nssec10 = dz_nssec10 |>
    dplyr::group_by(DataZone, IZ22_Code,NSSEC5) |>
    dplyr::summarise(households = sum(households))

  dz_HouseholdComp = tidyr::pivot_longer(dz_HouseholdComp,
                                         cols = c("OnePersonOver66","OnePersonOther","FamilyOver66",
                                                  "CoupleNoChildren","CoupleChildren","CoupleNonDepChildren",
                                                  "LoneParent","LoneParentNonDepChildren","OtherChildren",
                                                  "OtherIncStudentOrOver66"),
                                         names_to = "hhcomp10",
                                         values_to = "households"
  )

  dz_HouseholdComp = dz_HouseholdComp[order(dz_HouseholdComp$IZ22_Code),]
  dz_nssec10 = dz_nssec10[order(dz_nssec10$IZ22_Code),]
  dz_ethnic2 = dz_ethnic2[order(dz_ethnic2$IZ22_Code),]

  res_all = res_all[order(res_all$IZCode),]

  dz_HouseholdComp = dplyr::group_split(dplyr::ungroup(dz_HouseholdComp), IZ22_Code)
  dz_nssec10 = dplyr::group_split(dplyr::ungroup(dz_nssec10), IZ22_Code)
  dz_ethnic2 = dplyr::group_split(dplyr::ungroup(dz_ethnic2), IZ22_Code)
  res_all_lst = dplyr::group_split(dplyr::ungroup(res_all), IZCode)

  future::plan("multisession")
  res_all_dz = furrr::future_pmap(.l = list(ra = res_all_lst,
                                            nssec = dz_nssec10,
                                            comp = dz_HouseholdComp,
                                            eth = dz_ethnic2),
                               .f = syth_communit_photo_dz_scot,
                               .progress = TRUE,
                               .options = furrr::furrr_options(seed = 1234))
  future::plan("sequential")
  res_all_dz = dplyr::bind_rows(res_all_dz)

  res_all_dz


}

# hhns = hhcomp_nssec[[266]]
# eth = ethnic8[[266]]
# seed = hhComp_nssec_ethnic_scot
#
# res_all = purrr::pmap(.l = list(hhns = hhcomp_nssec,
#                                        eth = ethnic8),
#                              .f = syth_communit_photo_scot,
#                              seed = hhComp_nssec_ethnic_scot,
#                              .progress = TRUE)

# ra = res_all_lst[[645]]
# comp = dz_HouseholdComp[[645]]
# nssec = dz_nssec10[[645]]
# eth = dz_ethnic2[[645]]


#' Downscale one Intermediate Zone's household types to its Data Zones
#'
#' @description Worker for `make_community_photo_scotland()` (stage 2). Takes
#'   the IZ-level composition x NS-SEC x ethnicity estimate and IPF-fits a
#'   4-dimensional array adding the Data Zone dimension, constrained by DZ
#'   NS-SEC and composition marginals and a DZ household-ethnicity estimate
#'   derived from the population ethnicity split. Results are integerised
#'   with `int_trs()` and validated against the marginals.
#' @param ra IZ-level result from `syth_communit_photo_scot()` for one IZ.
#' @param nssec DZ households by NS-SEC for the Data Zones in this IZ.
#' @param comp DZ households by 10-category composition for the same zones.
#' @param eth DZ population by White/Other ethnicity for the same zones.
#' @return A data frame with `householdComp10`, `NSSEC5`, `ethnic3`,
#'   `DataZone`, `households` (positive cells only), `conv` and `MAE`.
#' @keywords internal
syth_communit_photo_dz_scot = function(ra, nssec, comp, eth){

  if(length(unique(c(ra$IZCode,
                     nssec$IZ22_Code,
                     comp$IZ22_Code,
                     eth$IZ22_Code
  ))) != 1){
    stop("More than one IZCode")
  }

  householdComp10n = c("OnePersonOver66","OnePersonOther","FamilyOver66","CoupleNoChildren","CoupleChildren","CoupleNonDepChildren","LoneParent","LoneParentNonDepChildren","OtherChildren","OtherIncStudentOrOver66")
  ethnic3n = c("black","other","white")
  NSSEC5n = c("higher","intermediate","routine","students","unemployed")
  DataZonen = unique(nssec$DataZone)

  nssec = nssec[nssec$NSSEC5 != "DNA",] # Only a handful in all of scotland

  # Get total housheolds per DataZone
  dz_hh = comp |>
    dplyr::group_by(DataZone) |>
    dplyr::summarise(households = sum(households))

  nssec= nssec|>
    dplyr::mutate(
      DataZone   = factor(DataZone, levels = DataZonen),
      NSSEC5 = factor(NSSEC5, levels = NSSEC5n)
    ) |>
    dplyr::arrange(NSSEC5, DataZone)

  nssec_array = array(nssec$households, dim = c(length(DataZonen),5), dimnames = list(DataZonen, NSSEC5n))


  comp= comp|>
    dplyr::mutate(
      DataZone   = factor(DataZone, levels = DataZonen),
      hhcomp10 = factor(hhcomp10, levels = householdComp10n)
    ) |>
    dplyr::arrange(hhcomp10, DataZone)

  comp_array = array(comp$households, dim = c(length(DataZonen),10), dimnames = list(DataZonen, householdComp10n))

  ra= ra|>
    dplyr::mutate(
      NSSEC5 = factor(NSSEC5, levels = NSSEC5n),
      householdComp10 = factor(householdComp10, levels = householdComp10n),
      ethnic3 = factor(ethnic3, levels = ethnic3n)
    ) |>
    dplyr::arrange(householdComp10, NSSEC5, ethnic3)

  ra_array = array(ra$households, dim = c(3,5,10), dimnames = list(ethnic3n, NSSEC5n, householdComp10n))

  names(dimnames(ra_array)) = c("ethnic3","NSSEC5","householdComp10")

  med_pop = median(c(sum(nssec_array),sum(comp_array),sum(ra_array)))

  # Adjust population ethnic3 to housholds ethnic3
  hh_eth = ra |>
    dplyr::group_by(ethnic3) |>
    dplyr::summarise(households = sum(households))

  eth = dplyr::left_join(eth, dz_hh, by = "DataZone")

  #eth$Whitehh = eth$White * (hh_eth$households[hh_eth$ethnic3 == "white"]/sum(eth$White))
  #eth$Whitehh = eth$households * (eth$White/(eth$White + eth$Other))

  eth$weight_households = eth$households/sum(eth$households)
  eth$weight_white = (eth$White/(eth$White + eth$Other))
  eth$weight_othercomb = (eth$Other/(eth$White + eth$Other))

  eth$households_weighted = sum(hh_eth$households) * eth$weight_households

  eth$diff0 = eth$households - eth$households_weighted

  #eth$Whitehh = hh_eth$households[hh_eth$ethnic3 == "white"] * (eth$weight_households * eth$weight_white)/(sum(eth$weight_households * eth$weight_white))
  eth$Whitehh = eth$households_weighted * eth$weight_white
  #eth$OtherComhh = eth$Other * (sum(hh_eth$households[hh_eth$ethnic3 %in% c("black","other")])/sum(eth$Other))
  #eth$OtherComhh = eth$households * (eth$Other/(eth$White + eth$Other))
  #eth$OtherComhh = sum(hh_eth$households[hh_eth$ethnic3 %in% c("black","other")]) * (eth$weight_households * eth$weight_othercomb)/(sum(eth$weight_households * eth$weight_othercomb))
  eth$OtherComhh = eth$households_weighted * eth$weight_othercomb



  # for(i in seq_len(nrow(eth))){
  #   if(eth$Whitehh[i] %% 1 == 0 & eth$OtherComhh[i] %% 1 == 0){
  #     next #Aready Integer result
  #   }
  #   intspop = int_trs(c(eth$Whitehh[i],eth$OtherComhh[i]))
  #   eth$Whitehh[i] = intspop[1]
  #   eth$OtherComhh[i] = intspop[2]
  # }

  eth$diff1 = eth$households_weighted - eth$Whitehh - eth$OtherComhh

  eth$Blackhh = (eth$OtherComhh * hh_eth$households[hh_eth$ethnic3 == "black"]/sum(hh_eth$households[hh_eth$ethnic3 %in% c("black","other")]))
  eth$Otherhh = (eth$OtherComhh * hh_eth$households[hh_eth$ethnic3 == "other"]/sum(hh_eth$households[hh_eth$ethnic3 %in% c("black","other")]))

  for(i in seq_len(nrow(eth))){
    if(eth$Blackhh[i] %% 1 == 0 &
       eth$Otherhh[i] %% 1 == 0 &
       eth$Whitehh[i] %% 1 == 0){
      next #Aready Integer result
    }
    intspop = int_trs(c(eth$Blackhh[i],eth$Otherhh[i],eth$Whitehh[i]))
    eth$Blackhh[i] = intspop[1]
    eth$Otherhh[i] = intspop[2]
    eth$Whitehh[i] = intspop[3]
  }

  eth$diff2 = eth$households_weighted - eth$Whitehh - eth$Blackhh - eth$Otherhh

  eth = eth[,c("DataZone","Whitehh","Blackhh","Otherhh","IZ22_Code")]
  names(eth) = c("DataZone","white","black","other","IZ22_Code")
  eth = eth |>
    tidyr::pivot_longer(cols = ethnic3n,
                        names_to = "ethnic3",
                        values_to = "households")

  eth= eth|>
    dplyr::mutate(
      DataZone   = factor(DataZone, levels = DataZonen),
      ethnic3 = factor(ethnic3, levels = ethnic3n)
    ) |>
    dplyr::arrange(ethnic3, DataZone)

  eth_array = array(eth$households, dim = c(length(DataZonen),3), dimnames = list(DataZonen, ethnic3n))


  seed2 = array(1, dim = c(10,5,3,length(DataZonen)))
  dimnames(seed2) = list(householdComp10n, NSSEC5n, ethnic3n, DataZonen)

  seed_weighted = (seed2 /sum(seed2)) * med_pop

  res <- mipfp::Ipfp(seed_weighted,
                     list(c(3,2,1),c(4,1),c(4,2),c(4,3)),
                     list(
                       ra_array,
                       comp_array,
                       nssec_array,
                       eth_array
                     ), iter = 1000, tol = 1e-10)

  dimnames(res$p.hat) = dimnames(seed_weighted)
  res2 = int_trs(res$p.hat * med_pop)

  result_df <- as.data.frame.table(res2)
  names(result_df) = c("householdComp10", "NSSEC5", "ethnic3","DataZone","households")
  result_df$conv = res$conv

  chk1 = vaidate_syth_pop2(x = result_df, y = nssec_array,var1 = "DataZone", var2 = "NSSEC5")
  chk2 = vaidate_syth_pop2(result_df,comp_array,"DataZone","householdComp10")
  chk3 = vaidate_syth_pop2(x = result_df, y = ra_array,var1 = "NSSEC5", var2 = "ethnic3")
  chk4 = vaidate_syth_pop2(x = result_df, y = ra_array,var1 = "NSSEC5", var2 = "householdComp10")
  chk5 = vaidate_syth_pop2(x = result_df, y = ra_array,var1 = "ethnic3", var2 = "householdComp10")

  result_df = result_df[result_df$households > 0,]
  result_df$MAE = max(c(chk1, chk2, chk3, chk4, chk5))
  #result_df$IZCode = hhns$IZCode[1]
  result_df


}


#' Estimate composition x NS-SEC x ethnicity for one Intermediate Zone
#'
#' @description Worker for `make_community_photo_scotland()` (stage 1). IPF-fits
#'   a 10 x 5 x 3 array for one IZ, seeded with the national composition x
#'   NS-SEC x ethnicity cross-tab (zeros replaced with 0.5) and constrained by
#'   the IZ's composition x NS-SEC table and ethnicity totals (rescaled to the
#'   household total). Integerised with `int_trs()`.
#' @param hhns IZ households by composition and NS-SEC (long format).
#' @param eth One row of IZ household counts by white/black/other.
#' @param seed National composition x NS-SEC x ethnicity cross-tab.
#' @return A data frame with `householdComp10`, `NSSEC5`, `ethnic3`,
#'   `households`, `MAE` and `IZCode`.
#' @keywords internal
syth_communit_photo_scot = function(hhns, eth, seed){

  if(length(unique(c(hhns$IZCode,
                    eth$IZCode
  ))) != 1){
    stop("More than one IZCode")
  }

  householdComp10n = c("OnePersonOver66","OnePersonOther","FamilyOver66","CoupleNoChildren","CoupleChildren","CoupleNonDepChildren","LoneParent","LoneParentNonDepChildren","OtherChildren","OtherIncStudentOrOver66")
  ethnic3n = c("black","other","white")
  NSSEC5n = c("higher","intermediate","routine","students","unemployed")

  hhns= hhns|>
    dplyr::mutate(
      householdComp10 = factor(householdComp10, levels = householdComp10n),
      NSSEC5 = factor(NSSEC5, levels = NSSEC5n)
    ) |>
    dplyr::arrange(NSSEC5, householdComp10)

  Meth_array = as.numeric(eth[,ethnic3n])
  names(Meth_array) = ethnic3n
  Meth_array = array(Meth_array, dim = c(length(Meth_array)), dimnames = list(ethnic3n))
  Mhhns_array = array(hhns$households, dim = c(10,5), dimnames = list(householdComp10n, NSSEC5n))

  seed <- seed |>
    dplyr::mutate(
      householdComp10 = factor(householdComp10, levels = householdComp10n),
      ethnic3 = factor(ethnic8, levels = ethnic3n),
      NSSEC5 = factor(NSSEC5, levels = NSSEC5n)
    ) |>
    dplyr::arrange(ethnic3, NSSEC5, householdComp10)

  # Can't have 0s in seeds
  seed$households[seed$households == 0] = 0.5
  seed2 = array(seed$households, dim = c(10,5,3))
  dimnames(seed2) = list(householdComp10n, NSSEC5n, ethnic3n)

  med_pop = sum(Mhhns_array)

  #TODO: Fix bug in int_trs when inputs are all interger to start with
  if(med_pop/sum(Meth_array) != 1){
    Meth_array = int_trs(Meth_array * med_pop/sum(Meth_array))
  }

  seed_weighted = (seed2 /sum(seed2)) * med_pop

  res <- mipfp::Ipfp(seed_weighted,
                     list(c(1,2),c(3)),
                     list(
                       Mhhns_array,
                       Meth_array
                     ))

  dimnames(res$x.hat) = dimnames(seed_weighted)

  res2 = int_trs(res$x.hat)

  result_df <- as.data.frame.table(res2)
  names(result_df) = c("householdComp10", "NSSEC5", "ethnic3","households")

  chk1 = vaidate_syth_pop2(result_df,Mhhns_array,"householdComp10","NSSEC5")

  #result_df = result_df[result_df$households > 0,]
  result_df$MAE = max(chk1)
  result_df$IZCode = hhns$IZCode[1]
  result_df

}




#' Read Scotland 2022 households by composition and NS-SEC per IZ
#'
#' @description Parses the wide Scotland Census 2022 extract of households by
#'   10-category composition x 10-category NS-SEC per Intermediate Zone
#'   (columns named positionally), attaches IZ codes by matching names against
#'   `bounds_iz22`, and pivots to long format.
#' @param path Path to the wide CSV extract.
#' @param bounds_iz22 IZ boundaries providing `IZCode`/`IZName`; row order
#'   must match the CSV (checked by name).
#' @return A long data frame with `IZCode`, `IZName`, `householdComp10`,
#'   `NSSEC10` and `households`.
#' @keywords internal
read_hhComp_nssec_iz_scot = function(path = "../inputdata/population_scotland/scotlandcensus2022_NSSEC10_householdComp10_IntermiedateZone_wide2.csv", bounds_iz22){

  raw = readr::read_csv(path, show_col_types = FALSE, skip = 12, col_names = FALSE)

  names(raw) = c("ItermediateZone",
                 paste0(
                   rep(c("OnePersonOver66","OnePersonOther","FamilyOver66",
                         "CoupleNoChildren","CoupleChildren","CoupleNonDepChildren",
                         "LoneParent","LoneParentNonDepChildren",
                         "OtherChildren","OtherIncStudentOrOver66","total"), each = 10),
                   "_",
                   rep(c("DNA","L1L2L3","L4L5L6","L7","L8L9","L10L11","L12","L13","L14","L15"), times = 11)
                 ), "dud")

  raw$dud = NULL
  raw = raw[,!grepl("total",names(raw))]
  raw = raw[!raw$ItermediateZone %in% c("Total","INFO","(c) Copyright WingArc Australia 2018"),]


  raw2 = cbind(raw, bounds_iz22)

  if(any(raw2$ItermediateZone != raw2$IZName)){
    stop("Can't match IZ names")
  }

  raw2$ItermediateZone = NULL

  raw3 = tidyr::pivot_longer(raw2, cols = names(raw2)[!names(raw2) %in% c("IZCode","IZName")],
                             names_to = c("householdComp10","NSSEC10"),
                             names_sep = "_",
                             values_to = "households"
                             )
  raw3

}

#' Read the national Scotland 2022 composition x NS-SEC x ethnicity table
#'
#' @description Parses the Scotland-wide census extract of households by
#'   10-category composition x 10-category NS-SEC x 8-group ethnicity
#'   (columns named positionally) and shortens the composition labels. Used
#'   as the IPF seed in `make_community_photo_scotland()`.
#' @param path Path to the wide CSV extract.
#' @return A wide data frame: one row per composition category, one column
#'   per `<NSSEC10>_<ethnic8>` combination.
#' @keywords internal
read_hhComp_nssec_ethnic_scot = function(path = "../inputdata/population_scotland/scotlandcenus2022_householdComp10_nssec10_ethnic8_Scotland.csv"){

  raw = readr::read_csv(path, show_col_types = FALSE, skip = 12, col_names = FALSE)

  nssec10 = c("DNA","L1L2L3","L4L5L6","L7","L8L9","L10L11","L12","L13","L14","L15")
  ethnic8 = c("WhiteScottish","WhiteBritish","OtherWhite","Mixed","Asian","African","Carribbean","Other","Total")

  names(raw) = c("householdComp10",
                 paste0(
                   rep(nssec10, times = 9),
                   "_",
                   rep(ethnic8, each = 10)),
                 "dud")
  raw$dud = NULL

  raw = raw[1:10,]

  raw$householdComp10[raw$householdComp10 == "One person household: Aged 66 and over"] = "OnePersonOver66"
  raw$householdComp10[raw$householdComp10 == "One person household: Aged under 66" ] = "OnePersonOther"
  raw$householdComp10[raw$householdComp10 == "One family household: All aged 66 and over"] = "FamilyOver66"
  raw$householdComp10[raw$householdComp10 == "One family household: Couple family: No children" ] = "CoupleNoChildren"
  raw$householdComp10[raw$householdComp10 == "One family household: Couple family: With dependent children"] = "CoupleChildren"
  raw$householdComp10[raw$householdComp10 == "One family household: Couple family: All children non-dependent"] = "CoupleNonDepChildren"
  raw$householdComp10[raw$householdComp10 == "One family household: Lone parent: With dependent children" ] = "LoneParent"
  raw$householdComp10[raw$householdComp10 == "One family household: Lone parent: All children non-dependent" ] = "LoneParentNonDepChildren"
  raw$householdComp10[raw$householdComp10 == "Other household types: With dependent children" ] = "OtherChildren"
  raw$householdComp10[raw$householdComp10 == "Other household types: Other (including all full-time students and all aged 66 and over)"] = "OtherIncStudentOrOver66"

  raw

}

#' Read Scotland 2022 households by 8-group ethnicity per IZ
#'
#' @description Parses the census extract of household ethnicity per
#'   Intermediate Zone and attaches IZ codes by matching names against
#'   `bounds_iz22`.
#' @param path Path to the CSV extract.
#' @param bounds_iz22 IZ boundaries providing `IZCode`/`IZName`; row order
#'   must match the CSV (checked by name).
#' @return A data frame with `IZCode`, `IZName` and one column per ethnic
#'   group.
#' @keywords internal
read_ethnic8_iz_scot = function(path = "../inputdata/population_scotland/scotlandcenus2022_ethnic8_IntermiedateZone.csv", bounds_iz22){

  raw = readr::read_csv(path, show_col_types = FALSE, skip = 11, col_names = FALSE)

  names(raw) = c("ItermediateZone","WhiteScottish","WhiteBritish","OtherWhite","Mixed","Asian","African","Carribbean","Other","Total","dud")
  raw$Total = NULL
  raw$dud = NULL
  raw = raw[!is.na(raw$ItermediateZone),]
  raw = raw[!is.na(raw$WhiteScottish),]
  raw = raw[raw$ItermediateZone != "Total",]

  raw2 = cbind(bounds_iz22, raw)

  if(any(raw2$ItermediateZone != raw2$IZName)){
    stop("Can't match IZ names")
  }

  raw2$ItermediateZone = NULL

  raw2

}

#' Read Scotland 2022 households by NS-SEC per Data Zone
#'
#' @description Parses the census extract of households by 10-category NS-SEC
#'   per 2022 Data Zone (columns named positionally, metadata rows removed).
#' @param path Path to the CSV extract.
#' @return A data frame with `DataZone` and one column per NS-SEC category.
#' @keywords internal
read_nssec10_dz_scot = function(path = "../inputdata/population_scotland/scotlandcenus2022_nssec10_DataZone.csv"){

  # Read the CSV (skip the metadata rows)
  raw = readr::read_csv(path, show_col_types = FALSE, skip = 11, col_names = FALSE)

  # NSSEC10 categories used in other readers in this file
  nssec10 = c("DNA","L1L2L3","L4L5L6","L7","L8L9","L10L11","L12","L13","L14","L15")

  # Assign names: first column is DataZone label, then the 10 NSSEC columns, then optional dud
  names(raw) = c("DataZone", nssec10, "total","dud")

  # Drop dud/trailing column if present
  raw$dud = NULL
  raw$total = NULL

  # Remove rows that are clearly not data
  raw = raw[!is.na(raw$DataZone),]
  raw = raw[!raw$DataZone %in% c("Total","INFO","(c) Copyright WingArc Australia 2018"),]


  raw

}

#' Read Scotland 2022 households by composition per Data Zone
#'
#' @description Parses the census extract of households by 10-category
#'   household composition per 2022 Data Zone (columns named positionally,
#'   metadata rows removed). Note: this function was previously defined twice
#'   in this file with identical bodies; the duplicate has been removed.
#' @param path Path to the CSV extract.
#' @return A data frame with `DataZone` and one column per composition
#'   category.
#' @keywords internal
read_hhcomp10_dz_scot = function(path = "../inputdata/population_scotland/scotlandcenus2022_householdComp10_DataZone.csv"){

  # Read the CSV (skip the metadata rows)
  raw = readr::read_csv(path, show_col_types = FALSE, skip = 11, col_names = FALSE)

  # Household composition categories used in other readers in this file
  householdComp10n = c("OnePersonOver66","OnePersonOther","FamilyOver66","CoupleNoChildren","CoupleChildren","CoupleNonDepChildren","LoneParent","LoneParentNonDepChildren","OtherChildren","OtherIncStudentOrOver66")

  # Assign names: first column is DataZone label, then the 10 household composition columns, then optional dud
  names(raw) = c("DataZone", householdComp10n,"total","dud")

  # Drop dud/trailing column if present
  raw$dud = NULL
  raw$total = NULL

  # Remove rows that are clearly not data
  raw = raw[!is.na(raw$DataZone),]
  raw = raw[!raw$DataZone %in% c("Total","INFO","(c) Copyright WingArc Australia 2018"),]

  raw
}

#' Read Scotland 2022 population by White/Other ethnicity per Data Zone
#'
#' @description Parses the census extract of people (note: population, not
#'   households) by 2-group ethnicity per 2022 Data Zone. Used to estimate the
#'   household ethnicity split in `syth_communit_photo_dz_scot()`.
#' @param path Path to the CSV extract.
#' @return A data frame with `DataZone`, `White` and `Other`.
#' @keywords internal
read_ethnic2_dz_scot = function(path = "../inputdata/population_scotland/scotlandcensus2022_Ethnic2_People_DataZone.csv"){

  # Read the CSV (skip the metadata rows)
  raw = readr::read_csv(path, show_col_types = FALSE, skip = 11, col_names = FALSE)

  # The expected columns are: DataZone, two ethnic group columns (e.g. White and Other / Minority),
  # then optional total/dud columns produced by the source CSV. Name them conservatively and
  # drop the trailing total/dud columns if present.
  names(raw) = c("DataZone", "White", "Other", "total", "dud")

  # Drop helper/total columns if present
  if("total" %in% names(raw)) raw$total = NULL
  if("dud" %in% names(raw)) raw$dud = NULL

  # Remove non-data rows and return
  raw = raw[!is.na(raw$DataZone),]
  raw = raw[!raw$DataZone %in% c("Total","INFO","(c) Copyright WingArc Australia 2018"),]

  raw

}
