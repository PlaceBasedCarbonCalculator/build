
#' Read 2021 census residents by NS-SEC and ethnicity for LSOAs
#'
#' @description Reads the (partial) custom census table of usual residents by
#'   10-category NS-SEC and 6-group ethnicity per LSOA, simplifying the
#'   category labels. Used by the `NSSEC_ethinic_residents` target, an input
#'   to `build_household_types()` (the household "community photo"
#'   clustering).
#' @param path Path to the LSOA-level NS-SEC x ethnicity CSV.
#' @return A data frame with `LSOA21CD`, `NSSEC10`, `ethnic6` and `residents`.
#' @keywords internal
read_NSSEC_ethinic = function(path = "../inputdata/population/census2021EW_Resdidents_NSSEC10_Ethnicity_LSOA_partial.csv"
                              #,path2 = "../inputdata/population/census2021EW_Residents_Ethnicity_LSOA.csv"
                              ){

  raw = read.csv(path)

  names(raw) = c("LSOA21CD","LSOA21NM","NSSEC10CD","NSSEC10","ethnic6CD","ethnic6","residents")
  raw = raw[,c("LSOA21CD","NSSEC10","ethnic6","residents")]

  raw$NSSEC10 = simplify_nssec(raw$NSSEC10)
  raw$ethnic6 = simplify_ethnic6(raw$ethnic6)
  raw = raw[raw$ethnic6 != "DNA",] # 0 For all rows so remove

  raw
}



#' Build households by NS-SEC (5) and household composition (15) per LSOA
#'
#' @description Combines several 2021 census tables to estimate, for every
#'   E&W LSOA, the number of households in each combination of 5-category
#'   NS-SEC and 15-category household composition (married/cohabiting merged
#'   into "Couple"). Where the full LSOA cross-tab was published (the
#'   "partial" file) it is used directly; for the remaining LSOAs the
#'   marginals (household composition, NS-SEC, and the NS-SEC x 6-category
#'   composition table) are Furness-balanced in `balance_nssec2()` using the
#'   MSOA-level cross-tab as the seed. Used by the `NSSEC_household` target,
#'   an input to `build_household_types()`.
#' @param path1 LSOA households by 15-category composition CSV.
#' @param path2 Partial LSOA NS-SEC x composition-15 cross-tab CSV.
#' @param path Zip containing the NS-SEC and NS-SEC x composition-6 tables.
#' @param path_msoa MSOA-level NS-SEC x composition-15 cross-tab CSV.
#' @param lookup_postcode_OA_LSOA_MSOA_2021 Postcode/OA/LSOA/MSOA lookup used
#'   to map MSOA seeds onto their LSOAs.
#' @return A wide data frame with `LSOA21CD` and one column per
#'   `<household15>_<NSSEC5>` combination.
#' @keywords internal
read_household_nssec = function(path1 = "../inputdata/population/census2021EW_Households_HouseholdComposition15_LSOA.csv",
                                path2 = "../inputdata/population/census2021EW_RefPerson_NSSEC10_Houshold15_LSOA_partial.csv",
                                path = "../inputdata/population/census2021EW_HouseholdComposition.zip",
                                path_msoa = "../inputdata/population/census2021EW_RefPerson_NSSEC10_Houshold15_MSOA_partial.csv",
                                lookup_postcode_OA_LSOA_MSOA_2021
                                ){

  dir = file.path(tempdir(),"class_ethnic")
  dir.create(dir)
  unzip(path, exdir = dir)
  raw6 = read.csv(file.path(dir,"census2021EW_HouseholdComposition_NSSEC6.csv"))
  raw_nssec = read.csv(file.path(dir,"census2021EW_NSSEC.csv"))
  unlink(dir, recursive = TRUE)
  raw_hc = read.csv(path1)
  raw_nssec_hc_part = read.csv(path2)
  raw_msoa = read.csv(path_msoa)

  lookup_postcode_OA_LSOA_MSOA_2021 = lookup_postcode_OA_LSOA_MSOA_2021[,c("lsoa21cd","msoa21cd")]
  lookup_postcode_OA_LSOA_MSOA_2021 = lookup_postcode_OA_LSOA_MSOA_2021[!duplicated(lookup_postcode_OA_LSOA_MSOA_2021$lsoa21cd),]
  lookup_postcode_OA_LSOA_MSOA_2021 = lookup_postcode_OA_LSOA_MSOA_2021[!substr(lookup_postcode_OA_LSOA_MSOA_2021$lsoa21cd,1,1) %in% c("S","L","M","N"),]
  lookup_postcode_OA_LSOA_MSOA_2021 = lookup_postcode_OA_LSOA_MSOA_2021[!is.na(lookup_postcode_OA_LSOA_MSOA_2021$lsoa21cd),]

  names(raw_nssec_hc_part) = c("LSOA21CD","LSOA21NM","household15CD","household15","NSSEC10CD","NSSEC10","count")
  names(raw_hc) = c("LSOA21CD","LSOA21NM","household15CD","household15","count")
  names(raw_nssec) = c("LSOA21CD","LSOA21NM","NSSEC10CD","NSSEC10","count")
  names(raw6) = c("LSOA21CD","LSOA21NM","NSSEC10CD","NSSEC10","household6CD","household6","count")
  names(raw_msoa) = c("MSOA21CD","MSOA21NM","NSSEC10CD","NSSEC10","household15CD","household15","count")

  raw_nssec_hc_part = raw_nssec_hc_part[,c("LSOA21CD","NSSEC10","household15","count")]
  raw_hc = raw_hc[,c("LSOA21CD","household15","count")]
  raw_nssec = raw_nssec[,c("LSOA21CD","NSSEC10","count")]
  raw6 = raw6[,c("LSOA21CD","NSSEC10","household6","count")]
  raw_msoa = raw_msoa[,c("MSOA21CD","NSSEC10","household15","count")]

  raw_hc = raw_hc[!raw_hc$LSOA21CD %in% raw_nssec_hc_part$LSOA21CD,]
  raw_nssec = raw_nssec[!raw_nssec$LSOA21CD %in% raw_nssec_hc_part$LSOA21CD,]
  raw6 = raw6[!raw6$LSOA21CD %in% raw_nssec_hc_part$LSOA21CD,]

  raw_nssec_hc_part$NSSEC10 = simplify_nssec(raw_nssec_hc_part$NSSEC10)
  raw_nssec$NSSEC10 = simplify_nssec(raw_nssec$NSSEC10)
  raw6$NSSEC10 = simplify_nssec(raw6$NSSEC10)
  raw_msoa$NSSEC10 = simplify_nssec(raw_msoa$NSSEC10)

  # Collapse NSSEC10 into 5
  raw6$NSSEC5 = nssec10_to_nssec5(raw6$NSSEC10)
  raw_nssec$NSSEC5 = nssec10_to_nssec5(raw_nssec$NSSEC10)
  raw_nssec_hc_part$NSSEC5 = nssec10_to_nssec5(raw_nssec_hc_part$NSSEC10)
  raw_msoa$NSSEC5 = nssec10_to_nssec5(raw_msoa$NSSEC10)

  raw_nssec_hc_part$household15 = simplify_household15(raw_nssec_hc_part$household15)
  raw_hc$household15 = simplify_household15(raw_hc$household15)
  raw6$household6 = simplify_household6(raw6$household6)
  raw_msoa$household15 = simplify_household15(raw_msoa$household15)

  raw_msoa$household15 = gsub("Married","Couple",raw_msoa$household15)
  raw_msoa$household15 = gsub("Cohabit","Couple",raw_msoa$household15)

  raw_nssec = dplyr::group_by(raw_nssec, LSOA21CD, NSSEC5)
  raw_nssec = dplyr::summarise(raw_nssec, count = sum(count, na.rm = TRUE))
  raw_nssec = dplyr::ungroup(raw_nssec)

  raw6 = dplyr::group_by(raw6, LSOA21CD, NSSEC5, household6)
  raw6 = dplyr::summarise(raw6, count = sum(count, na.rm = TRUE))
  raw6 = dplyr::ungroup(raw6)

  raw_msoa = dplyr::group_by(raw_msoa, MSOA21CD, NSSEC5, household15)
  raw_msoa = dplyr::summarise(raw_msoa, count = sum(count, na.rm = TRUE))
  raw_msoa = dplyr::ungroup(raw_msoa)

  # Exclude DNA when always 0
  raw_hc = raw_hc[raw_hc$household15 != "DNA",]
  raw6 = raw6[raw6$household6 != "DNA",]
  raw_msoa = raw_msoa[raw_msoa$household15 != "DNA",]
  raw_nssec_hc_part = raw_nssec_hc_part[raw_nssec_hc_part$household15 != "DNA",]

  # Add LSOA to MSOA

  msoa_check = dplyr::left_join(lookup_postcode_OA_LSOA_MSOA_2021, raw_msoa,
                                by = c("msoa21cd" = "MSOA21CD"), relationship = "many-to-many")
  msoa_check = msoa_check[!msoa_check$lsoa21cd %in% raw_nssec_hc_part$LSOA21CD, ]

  # Add Household6 to Houshold15
  msoa_check$household6 = ""
  msoa_check$household6[msoa_check$household15 %in% c("OnePersonOver66","OnePersonOther")] = "OnePerson"
  msoa_check$household6[msoa_check$household15 %in% c("FamilyOver66")] = "FamilyOver66"
  msoa_check$household6[msoa_check$household15 %in% c("CoupleNoChildren",
                                              "CoupleChildren","CoupleNonDepChildren")] = "CoupleFamily"
  msoa_check$household6[msoa_check$household15 %in% c("LoneParent","LoneParentNonDepChildren")] = "LoneParent"
  msoa_check$household6[msoa_check$household15 %in% c("OtherNoChildren","OtherChildren","OtherIncStudentOrOver66")] = "Other6"

  raw_hc$household6 = ""
  raw_hc$household6[raw_hc$household15 %in% c("OnePersonOver66","OnePersonOther")] = "OnePerson"
  raw_hc$household6[raw_hc$household15 %in% c("FamilyOver66")] = "FamilyOver66"
  raw_hc$household6[raw_hc$household15 %in% c("MarriedNoChildren",
                                              "MarriedChildren","MarriedNonDepChildren",
                                              "CohabitNoChildren","CohabitChildren",
                                              "CohabitNonDepChildren")] = "CoupleFamily"
  raw_hc$household6[raw_hc$household15 %in% c("LoneParent","LoneParentNonDepChildren")] = "LoneParent"
  raw_hc$household6[raw_hc$household15 %in% c("OtherNoChildren","OtherChildren","OtherIncStudentOrOver66")] = "Other6"


  # Combine Married and Cohabiting
  raw_hc$household15 = gsub("Married","Couple",raw_hc$household15)
  raw_hc$household15 = gsub("Cohabit","Couple",raw_hc$household15)

  raw_hc = dplyr::group_by(raw_hc, LSOA21CD, household15, household6)
  raw_hc = dplyr::summarise(raw_hc, count = sum(count, na.rm = TRUE))
  raw_hc = dplyr::ungroup(raw_hc)

  raw_hc = raw_hc[order(raw_hc$LSOA21CD),]
  raw_nssec = raw_nssec[order(raw_nssec$LSOA21CD),]
  raw6 = raw6[order(raw6$LSOA21CD),]
  msoa_check = msoa_check[order(msoa_check$lsoa21cd),]

  lst_hc = dplyr::group_split(raw_hc, raw_hc$LSOA21CD, .keep = FALSE)
  lst_nssec = dplyr::group_split(raw_nssec, raw_nssec$LSOA21CD, .keep = FALSE)
  lst_raw6 = dplyr::group_split(raw6, raw6$LSOA21CD, .keep = FALSE)
  lst_msoa = dplyr::group_split(msoa_check, msoa_check$lsoa21cd, .keep = FALSE)

  lst_bal = purrr::pmap(list(hc = lst_hc, nssec = lst_nssec, both =lst_raw6, msoa = lst_msoa),
                        balance_nssec2, .progress = TRUE)


  lst_bal = dplyr::bind_rows(lst_bal)

  # Finish up
  raw_nssec_hc_part$household15 = gsub("Married","Couple",raw_nssec_hc_part$household15)
  raw_nssec_hc_part$household15 = gsub("Cohabit","Couple",raw_nssec_hc_part$household15)


  raw_nssec_hc_part = dplyr::group_by(raw_nssec_hc_part, LSOA21CD, NSSEC5, household15)
  raw_nssec_hc_part = dplyr::summarise(raw_nssec_hc_part, count = sum(count, na.rm = TRUE))
  raw_nssec_hc_part = dplyr::ungroup(raw_nssec_hc_part)

  raw_nssec_hc_part = tidyr::pivot_wider(raw_nssec_hc_part, names_from = c("household15","NSSEC5"),
                           values_from = "count", id_cols = "LSOA21CD")

  res = dplyr::bind_rows(list(raw_nssec_hc_part, lst_bal))
  res
}

#' Shorten census NS-SEC category labels to codes
#'
#' @description Reduces the long census NS-SEC labels (e.g. "L1, L2 and L3:
#'   Higher managerial...") to compact codes ("L1L2L3"); "Does not apply"
#'   becomes "DNA".
#' @param nsec Character vector of census NS-SEC labels.
#' @return Character vector of shortened codes.
#' @keywords internal
simplify_nssec = function(nsec){
  nsec = strsplit(nsec,":")
  nsec = sapply(nsec, `[[`, 1)
  nsec = gsub(" and ","",nsec)
  nsec = gsub(", ","",nsec)
  nsec = gsub("L14.1L14.2","L14",nsec)
  nsec = gsub("Does not apply","DNA",nsec)
  nsec
}


#' Collapse 10-category NS-SEC codes into 5 groups
#'
#' @description Maps the shortened NS-SEC codes to five groups: "higher",
#'   "intermediate", "routine", "students" and "unemployed".
#' @param nsec Character vector of codes from `simplify_nssec()`.
#' @return Character vector of 5-group labels.
#' @keywords internal
nssec10_to_nssec5 = function(nsec){

  nsec[nsec == "L14"] = "unemployed"
  nsec[nsec == "L15"] = "students"
  nsec[nsec %in% c("L1L2L3","L4L5L6")] = "higher"
  nsec[nsec %in% c("L7","L8L9")] = "intermediate"
  nsec[nsec %in% c("L10L11","L12","L13")] = "routine"
  nsec
}

#' Shorten 6-category household composition labels
#'
#' @description Maps the census 6-category household composition labels to
#'   compact codes (OnePerson, FamilyOver66, CoupleFamily, LoneParent,
#'   Other6, DNA).
#' @param x Character vector of census labels.
#' @return Character vector of shortened codes.
#' @keywords internal
simplify_household6 = function(x){
  x[x=="Does not apply"] = "DNA"
  x[x=="One-person household"] = "OnePerson"
  x[x=="Single family household: All aged 66 years and over"] = "FamilyOver66"
  x[x=="Single family household: Couple family household"] = "CoupleFamily"
  x[x=="Single family household: Lone parent household"] = "LoneParent"
  x[x=="Other household types"] = "Other6"
  x

}

#' Shorten 8-category household composition labels
#'
#' @description Maps the census 8-category household composition labels to
#'   compact codes (OnePersonOver66, CoupleChildren, etc.).
#' @param x Character vector of census labels.
#' @return Character vector of shortened codes.
#' @keywords internal
simplify_household8 = function(x){
  x[x=="Does not apply"] = "DNA"
  x[x=="One-person household: Aged 66 years and over"] = "OnePersonOver66"
  x[x=="One-person household: Other"] = "OnePersonOther"
  x[x=="Single family household: Couple family household: All children non-dependent"] = "CoupleNoDepChild"
  x[x=="Single family household: Couple family household: Dependent children"] = "CoupleChildren"
  x[x=="Single family household: Couple family household: No children"] = "CoupleNoChildren"
  x[x=="Single family household: Lone parent household"] = "LoneParent"
  x[x=="Other household types"] = "Other8"
  x

}

#' Shorten 15-category household composition labels
#'
#' @description Maps the census 15-category household composition labels to
#'   compact codes (MarriedChildren, CohabitNoChildren, LoneParent, etc.).
#'   Married/cohabiting are kept separate here and merged into "Couple" by the
#'   callers.
#' @param x Character vector of census labels.
#' @return Character vector of shortened codes.
#' @keywords internal
simplify_household15 = function(x){
  x[x=="Does not apply"] = "DNA"
  x[x=="One-person household: Aged 66 years and over"] = "OnePersonOver66"
  x[x=="One-person household: Other"] = "OnePersonOther"
  x[x=="Single family household: All aged 66 years and over"] = "FamilyOver66"
  x[x=="Single family household: Married or civil partnership couple: No children"] = "MarriedNoChildren"
  x[x=="Single family household: Married or civil partnership couple: Dependent children"] = "MarriedChildren"
  x[x=="Single family household: Married or civil partnership couple: All children non-dependent"] = "MarriedNonDepChildren"
  x[x=="Single family household: Cohabiting couple family: No children"] = "CohabitNoChildren"
  x[x=="Single family household: Cohabiting couple family: With dependent children"] = "CohabitChildren"
  x[x=="Single family household: Cohabiting couple family: All children non-dependent"] = "CohabitNonDepChildren"
  x[x=="Single family household: Lone parent family: With dependent children"] = "LoneParent"
  x[x=="Single family household: Lone parent family: All children non-dependent"] = "LoneParentNonDepChildren"
  x[x=="Other household types: Other related household: Other family composition"] = "OtherNoChildren"
  x[x=="Other household types: With dependent children"] = "OtherChildren"
  x[x=="Other household types: Other, including all full-time students and all aged 66 years and over"] = "OtherIncStudentOrOver66"
  x
}



#' Shorten 6-group ethnicity labels
#'
#' @description Maps the census ethnicity labels to short codes (Asian, Black,
#'   Mixed, White, Other, DNA).
#' @param x Character vector of census labels.
#' @return Character vector of shortened codes.
#' @keywords internal
simplify_ethnic6 = function(x){
  x[x=="Does not apply"] = "DNA"
  x[x=="Asian, Asian British or Asian Welsh"] = "Asian"
  x[x=="Black, Black British, Black Welsh, Caribbean or African"] = "Black"
  x[x=="Mixed or Multiple ethnic groups"] = "Mixed"
  x[x=="Other ethnic group"] = "Other"
  x

}








#' Estimate NS-SEC (5) x household composition (15) for one LSOA
#'
#' @description Worker for `read_household_nssec()` covering LSOAs without a
#'   published cross-tab. For each 6-category composition group it
#'   Furness-balances (via `make_mat()`) the LSOA marginals against the
#'   MSOA-level cross-tab used as a seed, then assembles the groups into one
#'   wide row.
#' @param hc Households by 15-category composition for one LSOA.
#' @param nssec Households by 5-group NS-SEC for the same LSOA.
#' @param both Households by NS-SEC x 6-category composition for the LSOA.
#' @param msoa MSOA-level NS-SEC x 15-category cross-tab (seed).
#' @return A one-row wide data frame with `LSOA21CD` and
#'   `<household15>_<NSSEC5>` columns.
#' @keywords internal
balance_nssec2 = function(hc, nssec, both, msoa){
  # Check
  if(hc$LSOA21CD[1] != nssec$LSOA21CD[1]){
    stop("LSOAs of hc and nssec don't match")
  }
  if(hc$LSOA21CD[1] != both$LSOA21CD[1]){
    stop("LSOAs of hc and both don't match")
  }
  if(hc$LSOA21CD[1] != msoa$lsoa21cd[1]){
    stop("LSOAs of hc and msoa don't match")
  }

  # Make Overall Matrix
  # mat = matrix(NA, nrow = 10, ncol = 14)
  # rownames(mat) = nssec$NSSEC10
  # colnames(mat) = hc$household15

  mat_one = make_mat(both, hc, msoa, type ="OnePerson")
  mat_couple = make_mat(both, hc, msoa,  type ="CoupleFamily")
  mat_lone = make_mat(both, hc, msoa,  type ="LoneParent")
  mat_other = make_mat(both, hc, msoa,  type = "Other6")
  mat_FamilyOver66 = both[both$household6 == "FamilyOver66",]
  mat_FamilyOver66 = matrix(mat_FamilyOver66$count, ncol = 1, dimnames = list(mat_FamilyOver66$NSSEC5, "FamilyOver66"))

  mat_all = cbind(mat_one,mat_FamilyOver66, mat_couple, mat_lone, mat_other)

  mat_all = as.data.frame(mat_all)
  mat_all$NSSEC5 = rownames(mat_all)
  mat_all = tidyr::pivot_wider(mat_all, names_from = "NSSEC5",
                               values_from = c("OnePersonOther","OnePersonOver66","FamilyOver66","CoupleChildren",
                                               "CoupleNoChildren","CoupleNonDepChildren","LoneParent",
                                               "LoneParentNonDepChildren","OtherChildren","OtherIncStudentOrOver66",
                                               "OtherNoChildren"))
  mat_all$LSOA21CD = hc$LSOA21CD[1]
  mat_all

}


#' Furness-balance one household-composition group for one LSOA
#'
#' @description Helper for `balance_nssec2()`. For one 6-category composition
#'   group (`type`), balances the NS-SEC row totals against the 15-category
#'   column totals; the MSOA cross-tab is the seed matrix when available,
#'   otherwise a uniform seed of 1s is used.
#' @param both LSOA NS-SEC x 6-category composition counts (row totals).
#' @param hc LSOA 15-category composition counts (column totals).
#' @param msoa MSOA NS-SEC x 15-category seed cross-tab.
#' @param type The 6-category group to balance ("OnePerson", "CoupleFamily",
#'   "LoneParent" or "Other6").
#' @return A balanced integer matrix (NS-SEC rows x 15-category columns).
#' @keywords internal
make_mat = function(both, hc, msoa, type = "OnePerson"){
  rsum_one = both[both$household6 == type,]
  csum_one = hc[hc$household6 == type,]
  msoa_one = msoa[msoa$household6 == type,]
  if(nrow(msoa_one) > 0){
    msoa_one = tidyr::pivot_wider(msoa_one[,c("NSSEC5","household15","count")],
                                  names_from = "household15", values_from = "count")
    msoa_one = as.data.frame(msoa_one)
    rownames(msoa_one) = msoa_one$NSSEC5
    msoa_one$NSSEC5 = NULL
    msoa_one = as.matrix(msoa_one)
    rsum_one = rsum_one[match(rownames(msoa_one), rsum_one$NSSEC5),]
    csum_one = csum_one[match(colnames(msoa_one), csum_one$household15),]
    mat_fin = furness_balance(msoa_one, rsum = rsum_one$count, csum = csum_one$count,
                              n = 100, check = FALSE, int_only = TRUE, quiet = TRUE)
  } else {
    mat_one = matrix(1, nrow = nrow(rsum_one), ncol = nrow(csum_one))
    rownames(mat_one) = rsum_one$NSSEC5
    colnames(mat_one) = csum_one$household15
    mat_fin = furness_balance(mat_one, rsum = rsum_one$count, csum = csum_one$count,
                              n = 100, check = FALSE, int_only = TRUE, quiet = TRUE)
  }
  mat_fin
}


#' Rank household archetypes and allocate picture counts
#'
#' @description Sorts household types by frequency and works out how many of
#'   `n` picture slots each type should get (at least 1) for the household
#'   "community photo" visualisation.
#' @param x Data frame with a `households` count column.
#' @param n Total number of picture slots available.
#' @return `x` sorted by frequency with added `per` (percent), `cum`
#'   (cumulative percent), `pic` (allocated pictures) and `cumpic` columns.
#' @keywords internal
top_architypes = function(x, n = 48){
  x = x[order(x$households, decreasing = TRUE),]
  x$per = x$households / sum(x$households) * 100
  x$cum = cumsum(x$per)
  x$pic = round(x$per/(100/n))
  x$pic[x$pic == 0] = 1 # Can get stuck at less than 20 options
  x$cumpic = cumsum(x$pic)
  #x = x[x$cumvin <= 20,]
  x
}
