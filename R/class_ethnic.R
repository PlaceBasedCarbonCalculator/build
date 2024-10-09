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


read_household_nssec = function(path = "../inputdata/population/census2021EW_HouseholdComposition.zip"){

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

simplify_nssec = function(nsec){
  nsec = strsplit(nsec,":")
  nsec = sapply(nsec, `[[`, 1)
  nsec = gsub(" and ","",nsec)
  nsec = gsub(", ","",nsec)
  nsec = gsub("L14.1L14.2","L14",nsec)
  nsec = gsub("Does not apply","DNA",nsec)
  nsec
}

simplify_household6 = function(x){
  x[x=="Does not apply"] = "DNA"
  x[x=="One-person household"] = "OnePerson"
  x[x=="Single family household: All aged 66 years and over"] = "Over66"
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

top_architypes = function(x){
  x = x[order(x$value, decreasing = TRUE),]
  x$per = x$value / sum(x$value) * 100
  x$cum = cumsum(x$per)
  x$vin = round(x$per/5)
  x$vin[x$vin == 0] = 1 # Can get stuck at less than 20 options
  x$cumvin = cumsum(x$vin)
  x = x[x$cumvin <= 20,]
  x
}
