read_class_ethinic = function(path = "../inputdata/population/census2021EW_HouseholdComposition.zip"){

  dir = file.path(tempdir(),"class_ethnic")
  dir.create(dir)
  unzip(path, exdir = dir)
  raw = read.csv(file.path(dir,"census2021EW_class_ethnic.csv"))
  unlink(dir, recursive = TRUE)

  names(raw) = c("x","Total","AB","C1","C2","DE")
  raw_numb = raw[raw$x %in% c("Asian, Asian British or Asian Welsh",
                              "Black, Black British, Black Welsh, Caribbean or African",
                              "Mixed or Multiple ethnic groups",
                              "White",
                              "Other ethnic group"),]
  raw_msoa = raw$Total[raw$x == "Area Name  :"]
  raw_numb$x[raw_numb$x == "Asian, Asian British or Asian Welsh"] = "Asian"
  raw_numb$x[raw_numb$x == "Black, Black British, Black Welsh, Caribbean or African"] = "Black"
  raw_numb$x[raw_numb$x == "Mixed or Multiple ethnic groups"] = "Mixed"
  raw_numb$x[raw_numb$x == "Other ethnic group"] = "Other"
  raw_numb$Total = NULL

  raw_msoa = strsplit(raw_msoa," : ")
  raw_msoa = sapply(raw_msoa, `[[`, 1)
  raw_numb$msoa21cd = rep(raw_msoa, each = 5)
  raw_numb[2:5] = lapply(raw_numb[2:5], as.numeric)


  wide = tidyr::pivot_wider(raw_numb, names_from = "x", values_from = c("AB","C1","C2","DE"))

  wide

}

read_NSSEC_ethinic = function(path = "../inputdata/population/census2021EW_Resdidents_NSSEC10_Ethnicity_LSOA_partial.csv"
                              #,path2 = "../inputdata/population/census2021EW_Residents_Ethnicity_LSOA.csv"
                              ){

  raw = read.csv(path)


  names(raw) = c("LSOA21CD","LSOA21NM","NSSEC10CD","NSSEC10","ethnic6CD","ethnic6","residents")
  raw = raw[,c("LSOA21CD","NSSEC10","ethnic6","residents")]

  raw$NSSEC10 = simplify_nssec(raw$NSSEC10)
  raw$ethnic6 = simplify_ethnic6(raw$ethnic6)
  raw = raw[raw$ethnic6 != "DNA",] # 0 For all rows so remove

  # ethonly = read.csv(path2)
  # names(ethonly) = c("LSOA21CD","LSOA21NM","ethnic6CD","ethnic6","residents")
  # ethonly = ethonly[,c("LSOA21CD","ethnic6","residents")]
  # ethonly = ethonly[!ethonly$LSOA21CD %in% unique(raw$LSOA21CD),]
  # ethonly$ethnic6 = simplify_ethnic6(ethonly$ethnic6)

  # Check ethonly distributions
  # LSOAs with suppresed breakdown are all more the 97% white
  # chk = purrr::map(dplyr::group_split(ethonly, LSOA21CD), function(x){
  #   x = x[order(x$residents,decreasing = TRUE),]
  #   x$percent = round(x$residents / sum(x$residents) * 100,1)
  #   x = x[1,]
  #   x
  # }, .progress = T)
  # chk = dplyr::bind_rows(chk)

  raw

}


read_household_nssec_old = function(path = "../inputdata/population/census2021EW_HouseholdComposition.zip"){

  dir = file.path(tempdir(),"class_ethnic")
  dir.create(dir)
  unzip(path, exdir = dir)
  raw6 = read.csv(file.path(dir,"census2021EW_HouseholdComposition_NSSEC6.csv"))
  raw8 = read.csv(file.path(dir,"census2021EW_HouseholdComposition_NSSEC8_partial.csv"))
  raw_hc = read.csv(file.path(dir,"census2021EW_HouseholdComposition.csv"))
  raw_nssec = read.csv(file.path(dir,"census2021EW_NSSEC.csv"))
  unlink(dir, recursive = TRUE)

  names(raw6) = c("LSOA21CD","LSOA21NM","NSSEC10CD","NSSEC10","household6CD","household6","count")
  names(raw8) = c("LSOA21CD","LSOA21NM","NSSEC10CD","NSSEC10","household8CD","household8","count")
  names(raw_hc) = c("LSOA21CD","LSOA21NM","household8CD","household8","count")
  names(raw_nssec) = c("LSOA21CD","LSOA21NM","NSSEC10CD","NSSEC10","count")

  raw6 = raw6[,c("LSOA21CD","NSSEC10","household6","count")]
  raw8 = raw8[,c("LSOA21CD","NSSEC10","household8","count")]
  raw_hc = raw_hc[,c("LSOA21CD","household8","count")]
  raw_nssec = raw_nssec[,c("LSOA21CD","NSSEC10","count")]

  raw_hc = raw_hc[!raw_hc$LSOA21CD %in% raw8$LSOA21CD,]
  raw_nssec = raw_nssec[!raw_nssec$LSOA21CD %in% raw8$LSOA21CD,]

  raw6$NSSEC10 = simplify_nssec(raw6$NSSEC10)
  raw8$NSSEC10 = simplify_nssec(raw8$NSSEC10)
  raw_nssec$NSSEC10 = simplify_nssec(raw_nssec$NSSEC10)

  raw6$household6 = simplify_household6(raw6$household6)
  raw8$household8 = simplify_household8(raw8$household8)
  raw_hc$household8 = simplify_household8(raw_hc$household8)

  raw_hc = raw_hc[order(raw_hc$LSOA21CD),]
  raw_nssec = raw_nssec[order(raw_nssec$LSOA21CD),]

  # Drop DNA and Lone Parent as same in 6 and 8
  raw_hc = raw_hc[!raw_hc$household8 %in% c("LoneParent","DNA"),]

  lst_hc = dplyr::group_split(raw_hc, raw_hc$LSOA21CD, .keep = FALSE)
  lst_nssec = dplyr::group_split(raw_nssec, raw_nssec$LSOA21CD, .keep = FALSE)

  lst_bal = purrr::map2(lst_hc, lst_nssec, balance_nssec, .progress = TRUE)
  lst_bal = dplyr::bind_rows(lst_bal)

  wide6 = tidyr::pivot_wider(raw6, names_from = "household6", values_from = "count")
  wide8 = tidyr::pivot_wider(raw8, names_from = "household8", values_from = "count")
  wide8$DNA = NULL
  wide8$LoneParent = NULL
  lst_bal = lst_bal[names(wide8)]
  wide8 = rbind(wide8, lst_bal)


  wide = dplyr::left_join(wide6[,c("LSOA21CD", "NSSEC10","LoneParent","DNA")],
                          wide8, by = c("LSOA21CD", "NSSEC10"))
  # long = tidyr::pivot_longer(wide,
  #                            cols = c("LoneParent","DNA","OnePersonOver66",
  #                            "OnePersonOther","CoupleNoChildren","CoupleChildren",
  #                            "CoupleNoDepChild","Other8"))
  # long_lst = dplyr::group_split(long, long$LSOA21CD, .keep = FALSE)
  #x = long_lst[[1]]

  # cats = purrr::map(long_lst, top_architypes)
  # cats = dplyr::bind_rows(cats)
  # cats$cat_name = paste0(cats$NSSEC10,"-",cats$name)
  #
  # foo = as.data.frame(table(cats$cat_name))
  #
  # wide_miss = wide[is.na(wide$Other8),]
  # foo = wide_miss[wide_miss$LSOA21CD == "E01000117",]

  wide

}


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



simplify_nssec = function(nsec){
  nsec = strsplit(nsec,":")
  nsec = sapply(nsec, `[[`, 1)
  nsec = gsub(" and ","",nsec)
  nsec = gsub(", ","",nsec)
  nsec = gsub("L14.1L14.2","L14",nsec)
  nsec = gsub("Does not apply","DNA",nsec)
  nsec
}


nssec10_to_nssec5 = function(nsec){

  nsec[nsec == "L14"] = "unemployed"
  nsec[nsec == "L15"] = "students"
  nsec[nsec %in% c("L1L2L3","L4L5L6")] = "higher"
  nsec[nsec %in% c("L7","L8L9")] = "intermediate"
  nsec[nsec %in% c("L10L11","L12","L13")] = "routine"
  nsec
}

simplify_household6 = function(x){
  x[x=="Does not apply"] = "DNA"
  x[x=="One-person household"] = "OnePerson"
  x[x=="Single family household: All aged 66 years and over"] = "FamilyOver66"
  x[x=="Single family household: Couple family household"] = "CoupleFamily"
  x[x=="Single family household: Lone parent household"] = "LoneParent"
  x[x=="Other household types"] = "Other6"
  x

}

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



simplify_ethnic6 = function(x){
  x[x=="Does not apply"] = "DNA"
  x[x=="Asian, Asian British or Asian Welsh"] = "Asian"
  x[x=="Black, Black British, Black Welsh, Caribbean or African"] = "Black"
  x[x=="Mixed or Multiple ethnic groups"] = "Mixed"
  x[x=="Other ethnic group"] = "Other"
  x

}




balance_nssec = function(x, y){
  # Check
  if(x$LSOA21CD[1] != y$LSOA21CD[1]){
    stop("LSOAs of X and Y don't match")
  }
  mat = matrix(NA, nrow = 10, ncol = 6)
  rownames(mat) = y$NSSEC10
  colnames(mat) = x$household8


  mat = furness_partial(mat, rsum = y$count, csum = x$count, n = 200, check = FALSE, int_only = FALSE)
  mat = as.data.frame(mat)
  mat$NSSEC10 = rownames(mat)
  rownames(mat) = NULL
  mat$LSOA21CD = x$LSOA21CD[1]
  mat

}




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

  # Make Muliple Matrixes for One person, Coupel, Lone Parent, Other

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
      rsum_one = rsum_one[match(rsum_one$NSSEC5, rownames(msoa_one)),]
      csum_one = csum_one[match(csum_one$household15, colnames(msoa_one)),]
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
