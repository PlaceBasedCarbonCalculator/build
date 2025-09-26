make_community_photo_scotland = function(path = "../inputdata/population_scotland/"){

  # Scotland
  hhComp_nssec_ethnic_scot = read_hhComp_nssec_ethnic_scot(file.path(path,"scotlandcenus2022_householdComp10_nssec10_ethnic8_Scotland.csv"))

  #IZ
  hhcomp_nssec = read_hhComp_nssec_iz_scot(file.path(path,"scotlandcenus2022_householdComp10_nssec10_IntermediateZone.csv"))
  ethnic8 = read_ethnic8_iz_scot(file.path(path,"scotlandcenus2022_ethnic8_IntermiedateZone.csv"))

  # Reduce Variables

  ethnic8$white = rowSums(ethnic8[,c("WhiteScottish","WhiteBritish","OtherWhite")])
  ethnic8$black = rowSums(ethnic8[,c("African","Carribbean")])
  ethnic8$other = rowSums(ethnic8[,c("Mixed","Asian","Other")])

  ethnic8 = ethnic8[,c("ItermediateZone","white","black","other")]

  hhcomp_nssec$NSSEC5 = nssec10_to_nssec5(hhcomp_nssec$NSSEC10)

  hhcomp_nssec = hhcomp_nssec |>
    dplyr::group_by(ItermediateZone, NSSEC5, householdComp10) |>
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


  hhcomp_nssec = hhcomp_nssec[order(hhcomp_nssec$ItermediateZone),]
  ethnic8 = ethnic8[order(ethnic8$ItermediateZone),]

  hhcomp_nssec = dplyr::group_split(dplyr::ungroup(hhcomp_nssec), ItermediateZone)
  ethnic8 = dplyr::group_split(dplyr::ungroup(ethnic8), ItermediateZone)

  # Combine
  future::plan("multisession")
  res_all = furrr::future_pmap(.l = list(hhcomp_nssec,
                                         ethnic8),
                               .f = syth_communit_photo_scot,
                               seed = hhComp_nssec_ethnic_scot,
                               .progress = TRUE,  .options = furrr::furrr_options(seed = 1234))
  future::plan("sequential")
  res_all = dplyr::bind_rows(res_all)



}

# hhns = hhcomp_nssec[[26]]
# eth = ethnic8[[26]]
# seed = hhComp_nssec_ethnic_scot

syth_communit_photo_scot = function(hhns, eth, seed){

  if(length(unique(c(hhns$ItermediateZone,
                    eth$ItermediateZone
  ))) != 1){
    stop("More than one ItermediateZone")
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

  Meth_array = as.numeric(eth[,ethnic3])
  names(Meth_array) = ethnic3
  Meth_array = array(Meth_array, dim = c(length(Meth_array)), dimnames = list(ethnic3n))
  Mhhns_array = array(hhns$households, dim = c(10,5), dimnames = list(householdComp10, NSSEC5))

  seed <- seed |>
    dplyr::mutate(
      householdComp10 = factor(householdComp10, levels = householdComp10n),
      ethnic3 = factor(ethnic3, levels = ethnic3n),
      NSSEC5 = factor(NSSEC5, levels = NSSEC5n)
    ) |>
    dplyr::arrange(ethnic3, NSSEC5, householdComp10)

  # Can't have 0s in seeds
  seed$households[seed$households == 0] = 0.5
  seed2 = array(seed$households, dim = c(10,5,3))
  dimnames(seed2) = list(householdComp10, NSSEC5, ethnic3)

  med_pop = sum(Mhhns_array)

  Meth_array = int_trs(Meth_array * med_pop/sum(Meth_array))

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

  chk1 = vaidate_syth_pop(result_df,Mhhns_array,"householdComp10","NSSEC5")

  result_df = result_df[result_df$households > 0,]
  result_df$MAE = max(chk1)
  result_df$ItermediateZone = hhns$ItermediateZone[1]
  result_df

}




read_hhComp_nssec_iz_scot = function(path = "../inputdata/population_scotland/scotlandcenus2022_householdComp10_nssec10_IntermediateZone.csv"){

  raw = readr::read_csv(path, show_col_types = FALSE, skip = 10, col_names = FALSE)

  names(raw) = c("counting","ItermediateZone","NSSEC10","householdComp10","households")
  raw$counting = NULL
  raw = raw[!is.na(raw$ItermediateZone),]
  raw = raw[!is.na(raw$NSSEC10),]
  raw = raw[raw$ItermediateZone != "Total",]
  raw = raw[raw$NSSEC10 != "Total",]

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


  raw$NSSEC10[raw$NSSEC10 == "Higher managerial, administrative and professional occupations"] = "L1L2L3"
  raw$NSSEC10[raw$NSSEC10 == "Lower managerial, administrative and professional occupations"] = "L4L5L6"
  raw$NSSEC10[raw$NSSEC10 == "Intermediate occupations"] = "L7"
  raw$NSSEC10[raw$NSSEC10 == "Small employers and own account workers"] = "L8L9"
  raw$NSSEC10[raw$NSSEC10 == "Lower supervisory and technical occupations"] = "L10L11"
  raw$NSSEC10[raw$NSSEC10 == "Semi-routine occupations"] = "L12"
  raw$NSSEC10[raw$NSSEC10 == "Routine occupations"] = "L13"
  raw$NSSEC10[raw$NSSEC10 == "Never worked and long-term unemployed"] = "L14"
  raw$NSSEC10[raw$NSSEC10 == "Full-time students"] = "L15"
  raw$NSSEC10[raw$NSSEC10 == "Not applicable (aged less than 16) "] = "DNA"
  raw$NSSEC10[grepl("applicable",raw$NSSEC10)] = "DNA"

  raw

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

read_ethnic8_iz_scot = function(path = "../inputdata/population_scotland/scotlandcenus2022_ethnic8_IntermiedateZone.csv"){

  raw = readr::read_csv(path, show_col_types = FALSE, skip = 11, col_names = FALSE)

  names(raw) = c("ItermediateZone","WhiteScottish","WhiteBritish","OtherWhite","Mixed","Asian","African","Carribbean","Other","Total","dud")
  raw$Total = NULL
  raw$dud = NULL
  raw = raw[!is.na(raw$ItermediateZone),]
  raw = raw[!is.na(raw$WhiteScottish),]
  raw = raw[raw$ItermediateZone != "Total",]

  raw

}
