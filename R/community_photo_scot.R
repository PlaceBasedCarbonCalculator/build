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

  dz_nssec10 = dplyr::left_join(dz_nssec10, lookup_DataZone_2022, by = c("DataZone" = "DZ22_Code"))
  dz_HouseholdComp = dplyr::left_join(dz_HouseholdComp, lookup_DataZone_2022, by = c("DataZone" = "DZ22_Code"))

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
  res_all = res_all[order(res_all$IZCode),]

  dz_HouseholdComp = dplyr::group_split(dplyr::ungroup(dz_HouseholdComp), IZ22_Code)
  dz_nssec10 = dplyr::group_split(dplyr::ungroup(dz_nssec10), IZ22_Code)
  res_all_lst = dplyr::group_split(dplyr::ungroup(res_all), IZCode)




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

ra = res_all_lst[[1]]
comp = dz_HouseholdComp[[1]]
nssec = dz_nssec10[[1]]


syth_communit_photo_dz_scot = function(ra, nssec, comp){

  if(length(unique(c(ra$IZCode,
                     nssec$IZ22_Code,
                     comp$IZ22_Code
  ))) != 1){
    stop("More than one IZCode")
  }

  householdComp10n = c("OnePersonOver66","OnePersonOther","FamilyOver66","CoupleNoChildren","CoupleChildren","CoupleNonDepChildren","LoneParent","LoneParentNonDepChildren","OtherChildren","OtherIncStudentOrOver66")
  ethnic3n = c("black","other","white")
  NSSEC5n = c("higher","intermediate","routine","students","unemployed")
  DataZonen = unique(nssec$DataZone)

  nssec = nssec[nssec$NSSEC5 != "DNA",] # Only a handful in all of scotland

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

  seed2 = array(1, dim = c(10,5,3,length(DataZonen)))
  dimnames(seed2) = list(householdComp10n, NSSEC5n, ethnic3n, DataZonen)

  seed_weighted = (seed2 /sum(seed2)) * med_pop

  res <- mipfp::Ipfp(seed_weighted,
                     list(c(3,2,1),c(4,1),c(4,2)),
                     list(
                       ra_array,
                       comp_array,
                       nssec_array
                     ))

  dimnames(res$x.hat) = dimnames(seed_weighted)
  res2 = int_trs(res$x.hat * med_pop)

  result_df <- as.data.frame.table(res2)
  names(result_df) = c("householdComp10", "NSSEC5", "ethnic3","DataZone","households")

  chk1 = vaidate_syth_pop2(result_df,nssec_array,"DataZone","NSSEC5")
  chk2 = vaidate_syth_pop2(result_df,comp_array,"DataZone","householdComp10")
  chk3 = vaidate_syth_pop2(x = result_df, y = ra_array,var1 = "NSSEC5", var2 = "ethnic3")
  chk4 = vaidate_syth_pop2(x = result_df, y = ra_array,var1 = "NSSEC5", var2 = "householdComp10")
  chk5 = vaidate_syth_pop2(x = result_df, y = ra_array,var1 = "ethnic3", var2 = "householdComp10")

}


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




read_hhComp_nssec_iz_scot = function(path = "../inputdata/population_scotland/scotlandcensus2022_NSSEC10_householdComp10_IntermiedateZone_wide2.csv", bounds_iz22){

  raw = readr::read_csv(path, show_col_types = FALSE, skip = 12, col_names = FALSE)

  names(raw) = c("ItermediateZone",
                 paste0(
                   rep(c("OnePersonOver66","OnePersonOther","FamilyOver66",
                         "CoupleNoChildren","CoupleChildren","CoupleNonDepChildren",
                         "LoneParent","LoneParentNonDepChildren",
                         "OtherChildren","OtherIncStudentOrOver66","total"), each = 10),
                   "_",
                   rep(c("L1L2L3","L4L5L6","L7","L8L9","L10L11","L12","L13","L14","L15","DNA"), times = 11)
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
