read_Acc_hhComp = function(path = "../inputdata/population/census2021EW_Households_AccomodationType5_HouseholdComposition6_LSOA_partial.csv"){

  raw = readr::read_csv(path, show_col_types = FALSE)

  names(raw) = c("LSOA21CD","LSOA21NM","hhComp6CD","hhComp6","AccType5CD","AccType5","households")
  raw = raw[,c("LSOA21CD","AccType5","hhComp6","households")]

  raw

}

read_Acc_tenure = function(path = "../inputdata/population/census2021EW_Households_AccomodationType5_Tenure5_LSOA.csv"){

  raw = readr::read_csv(path, show_col_types = FALSE)

  names(raw) = c("LSOA21CD","LSOA21NM","Tenure5CD","Tenure5","AccType5CD","AccType5","households")
  raw = raw[,c("LSOA21CD","AccType5","Tenure5","households")]

  raw

}

read_Acc_CarVan = function(path = "../inputdata/population/census2021EW_Households_AccomodationType5_CarVan5_LSOA_partial.csv"){

  raw = readr::read_csv(path, show_col_types = FALSE)

  names(raw) = c("LSOA21CD","LSOA21NM","AccType5CD","AccType5","CarVan5CD","CarVan5","households")
  raw = raw[,c("LSOA21CD","AccType5","CarVan5","households")]

  raw

}

read_hhSize_hhComp = function(path = "../inputdata/population/census2021EW_Households_HouseholdComposition6_HouseholdSize5_LSOA_partial.csv"){

  raw = readr::read_csv(path, show_col_types = FALSE)

  names(raw) = c("LSOA21CD","LSOA21NM","hhComp6CD","hhComp6","hhSize5CD","hhSize5","households")
  raw = raw[,c("LSOA21CD","hhComp6","hhSize5","households")]

  raw

}

read_Acc_hhSize = function(path = "../inputdata/population/census2021EW_Households_AccomodationType5_HousehholdSize5_LSOA_partial.csv"){

  raw = readr::read_csv(path, show_col_types = FALSE)

  names(raw) = c("LSOA21CD","LSOA21NM","hhSize5CD","hhSize5","AccType5CD","AccType5","households")
  raw = raw[,c("LSOA21CD","hhSize5","AccType5","households")]

  raw

}

read_CarVan_hhComp = function(path = "../inputdata/population/census2021EW_Households_HouseholdComposition6_CarVan5_LSOA.csv"){

  raw = readr::read_csv(path, show_col_types = FALSE)

  names(raw) = c("LSOA21CD","LSOA21NM","hhComp6CD","hhComp6","CarVan5CD","CarVan5","households")
  raw = raw[,c("LSOA21CD","hhComp6","CarVan5","households")]

  raw

}

read_Tenure_hhSize = function(path = "../inputdata/population/census2021EW_Households_Tenure5_HouseholdSize5_LSOA.csv"){

  raw = readr::read_csv(path, show_col_types = FALSE)

  names(raw) = c("LSOA21CD","LSOA21NM","Tenure5CD","Tenure5","hhSize5CD","hhSize5","households")
  raw = raw[,c("LSOA21CD","Tenure5","hhSize5","households")]

  raw

}

read_Tenure_hhSize_CarVan = function(path = "../inputdata/population/census2021EW_Households_Tenure5_HouseholdSize5_CarVan5_LSOA_partial.csv"){

  raw = readr::read_csv(path, show_col_types = FALSE)

  names(raw) = c("LSOA21CD","LSOA21NM","CarVan5CD","CarVan5","Tenure5CD","Tenure5","hhSize5CD","hhSize5","households")
  raw = raw[,c("LSOA21CD","Tenure5","hhSize5","CarVan5","households")]

  raw

}

read_hhComp_Tenure = function(path = "../inputdata/population/census2021EW_Households_HouseholdComposition15_Tenure5_LSOA.csv"){

  raw = readr::read_csv(path, show_col_types = FALSE)

  names(raw) = c("LSOA21CD","LSOA21NM","hhComp15CD","hhComp15","Tenure5CD","Tenure5","households")
  raw = raw[,c("LSOA21CD","hhComp15","Tenure5","households")]

  raw

}

read_hhComp6_Tenure = function(path = "../inputdata/population/census2021EW_Households_HouseholdComposition6_Tenure5_LSOA.csv"){

  raw = readr::read_csv(path, show_col_types = FALSE)

  names(raw) = c("LSOA21CD","LSOA21NM","hhComp6CD","hhComp6","Tenure5CD","Tenure5","households")
  raw = raw[,c("LSOA21CD","hhComp6","Tenure5","households")]

  raw

}

read_hhSize = function(path = "../inputdata/population/census2021EW_Households_HouseholdSize7_LSOA.csv"){

  raw = readr::read_csv(path, show_col_types = FALSE)

  names(raw) = c("LSOA21CD","LSOA21NM","hhSize7CD","hhSize7","households")
  raw = raw[,c("LSOA21CD","hhSize7","households")]

  raw

}

read_CarVan = function(path = "../inputdata/population/census2021EW_Households_CarsVans6_LSOA.csv"){

  raw = readr::read_csv(path, show_col_types = FALSE)

  names(raw) = c("LSOA21CD","LSOA21NM","CarVan6CD","CarVan6","households")
  raw = raw[,c("LSOA21CD","CarVan6","households")]

  raw

}

read_tenure = function(path = "../inputdata/population/census2021EW_Households_Tenure9_LSOA.csv"){

  raw = readr::read_csv(path, show_col_types = FALSE)

  names(raw) = c("LSOA21CD","LSOA21NM","Tenure9CD","Tenure9","households")
  raw = raw[,c("LSOA21CD","Tenure9","households")]

  raw

}

sythetic_census = function(path_data = file.path(parameters$path_data,"population")){

  #Acc_hhComp = read_Acc_hhComp(file.path(path_data,"census2021EW_Households_AccomodationType5_HouseholdComposition6_LSOA_partial.csv"))  # Partial
  #Acc_CarVan = read_Acc_CarVan(file.path(path_data,"census2021EW_Households_AccomodationType5_CarVan5_LSOA_partial.csv"))  # Partial
  #hhSize_hhComp = read_hhSize_hhComp(file.path(path_data,"census2021EW_Households_HouseholdComposition6_HouseholdSize5_LSOA_partial.csv"))  # Partial
  Acc_tenure = read_Acc_tenure(file.path(path_data,"census2021EW_Households_AccomodationType5_Tenure5_LSOA.csv"))
  hhComp_Tenure = read_hhComp_Tenure(file.path(path_data,"census2021EW_Households_HouseholdComposition15_Tenure5_LSOA.csv"))
  #Tenure_hhSize = read_Tenure_hhSize(file.path(path_data,"census2021EW_Households_Tenure5_HouseholdSize5_LSOA.csv"))
  #CarVan_hhComp = read_CarVan_hhComp(file.path(path_data,"census2021EW_Households_HouseholdComposition6_CarVan5_LSOA.csv"))

  Tenure_hhSize_CarVan = read_Tenure_hhSize_CarVan(file.path(path_data,"census2021EW_Households_Tenure5_HouseholdSize5_CarVan5_LSOA_partial.csv"))

  # Remove DNA if all zeros
  # Acc_hhComp = Acc_hhComp[Acc_hhComp$hhComp6 != "Does not apply",]
  # Acc_CarVan = Acc_CarVan[Acc_CarVan$CarVan5 != "Does not apply",]
  # hhSize_hhComp = hhSize_hhComp[hhSize_hhComp$hhComp6 != "Does not apply",]
  # hhSize_hhComp = hhSize_hhComp[hhSize_hhComp$hhSize5 != "0 people in household",]
  Acc_tenure = Acc_tenure[Acc_tenure$Tenure5 != "Does not apply",]

  hhComp_Tenure = hhComp_Tenure[hhComp_Tenure$Tenure5 != "Does not apply",]
  hhComp_Tenure = hhComp_Tenure[hhComp_Tenure$hhComp15 != "Does not apply",]
  # Tenure_hhSize = Tenure_hhSize[Tenure_hhSize$Tenure5 != "Does not apply",]
  # Tenure_hhSize = Tenure_hhSize[Tenure_hhSize$hhSize5 != "0 people in household",]
  # CarVan_hhComp = CarVan_hhComp[CarVan_hhComp$hhComp6 != "Does not apply",]
  # CarVan_hhComp = CarVan_hhComp[CarVan_hhComp$CarVan5 != "Does not apply",]
  #Acc_hhSize = Acc_hhSize[Acc_hhSize$hhSize5 != "0 people in household",]

  Tenure_hhSize_CarVan = Tenure_hhSize_CarVan[Tenure_hhSize_CarVan$Tenure5 != "Does not apply",]
  Tenure_hhSize_CarVan = Tenure_hhSize_CarVan[Tenure_hhSize_CarVan$hhSize5 != "0 people in household",]
  Tenure_hhSize_CarVan = Tenure_hhSize_CarVan[Tenure_hhSize_CarVan$CarVan5 != "Does not apply",]

  Tenure_hhSize_CarVan$Tenure5 = simplify_Tenure5(Tenure_hhSize_CarVan$Tenure5)
  Tenure_hhSize_CarVan$hhSize5 = simplify_hhSize5(Tenure_hhSize_CarVan$hhSize5)
  Tenure_hhSize_CarVan$CarVan5 = simplify_CarVan5(Tenure_hhSize_CarVan$CarVan5)

  Acc_tenure$Tenure5 = simplify_Tenure5(Acc_tenure$Tenure5)
  Acc_tenure$AccType5 = simplify_AccType5(Acc_tenure$AccType5)


  # Simplify hhComp15 as don't know married etc
  hhComp_Tenure$hhComp15b = simplify_household15(hhComp_Tenure$hhComp15)
  hhComp_Tenure$hhComp15b = gsub("Cohabit|Married","Couple",hhComp_Tenure$hhComp15b)

  hhComp_Tenure2 = dplyr::group_by(hhComp_Tenure, LSOA21CD, Tenure5, hhComp15b)
  hhComp_Tenure2 = dplyr::summarise(hhComp_Tenure2, households = sum(households))

  hhComp_Tenure2$Tenure5 = simplify_Tenure5( hhComp_Tenure2$Tenure5)

  # Find common core
  # lsoa_common = Reduce(intersect,
  #                      list(unique(Acc_tenure$LSOA21CD),
  #                           unique(Acc_hhComp$LSOA21CD),
  #                           unique(Acc_CarVan$LSOA21CD),
  #                           unique(hhSize_hhComp$LSOA21CD)#,
  #                           #unique(Acc_hhSize$LSOA21CD)
  #                           )) #35267 98.9%

  lsoa_common = Reduce(intersect,
                       list(unique(Acc_tenure$LSOA21CD),
                            unique(hhComp_Tenure$LSOA21CD),
                            unique(Tenure_hhSize_CarVan$LSOA21CD)
                       )) #35148 98.5%

  Acc_tenure_com = Acc_tenure[Acc_tenure$LSOA21CD %in% lsoa_common,]
  # Acc_CarVan_com = Acc_CarVan[Acc_CarVan$LSOA21CD %in% lsoa_common,]
  # Acc_hhComp_com = Acc_hhComp[Acc_hhComp$LSOA21CD %in% lsoa_common,]
  # hhSize_hhComp_com = hhSize_hhComp[hhSize_hhComp$LSOA21CD %in% lsoa_common,]
  hhComp_Tenure_com = hhComp_Tenure2[hhComp_Tenure2$LSOA21CD %in% lsoa_common,]
  Tenure_hhSize_CarVan_com = Tenure_hhSize_CarVan[Tenure_hhSize_CarVan$LSOA21CD %in% lsoa_common,]

  # Reduce to 2 variables
  Tenure_hhSize_CarVan_com$hhSize5_CarVan5 = paste0(Tenure_hhSize_CarVan_com$hhSize5,"_",Tenure_hhSize_CarVan_com$CarVan5)
  Tenure_hhSize_CarVan_com = Tenure_hhSize_CarVan_com[,c("LSOA21CD","Tenure5","hhSize5_CarVan5","households")]

  # Pivot
  Acc_tenure_com = tidyr::pivot_wider(Acc_tenure_com, names_from = "Tenure5", values_from = "households")
  # Acc_CarVan_com = tidyr::pivot_wider(Acc_CarVan_com, names_from = "CarVan5", values_from = "households")
  # Acc_hhComp_com = tidyr::pivot_wider(Acc_hhComp_com, names_from = "hhComp6", values_from = "households")
  # hhSize_hhComp_com = tidyr::pivot_wider(hhSize_hhComp_com, names_from = "hhComp6", values_from = "households")
  hhComp_Tenure_com = tidyr::pivot_wider(hhComp_Tenure_com, names_from = "Tenure5", values_from = "households")
  Tenure_hhSize_CarVan_com = tidyr::pivot_wider(Tenure_hhSize_CarVan_com, names_from = "Tenure5", values_from = "households")


  # Order
  Acc_tenure_com    = Acc_tenure_com[order(Acc_tenure_com$LSOA21CD),]
  # Acc_CarVan_com    = Acc_CarVan_com[order(Acc_CarVan_com$LSOA21CD),]
  # Acc_hhComp_com    = Acc_hhComp_com[order(Acc_hhComp_com$LSOA21CD),]
  # hhSize_hhComp_com = hhSize_hhComp_com[order(hhSize_hhComp_com$LSOA21CD),]
  hhComp_Tenure_com    = hhComp_Tenure_com[order(hhComp_Tenure_com$LSOA21CD),]
  Tenure_hhSize_CarVan_com    = Tenure_hhSize_CarVan_com[order(Tenure_hhSize_CarVan_com$LSOA21CD),]


  # Split
  Acc_tenure_com = dplyr::group_split(Acc_tenure_com, LSOA21CD)
  Acc_CarVan_com = dplyr::group_split(Acc_CarVan_com, LSOA21CD)
  Acc_hhComp_com = dplyr::group_split(Acc_hhComp_com, LSOA21CD)
  hhSize_hhComp_com = dplyr::group_split(hhSize_hhComp_com, LSOA21CD)

  # Combine
  future::plan("multisession")
  res_com = furrr::future_pmap(.l = list(Acc_tenure_com, Acc_CarVan_com, Acc_hhComp_com, hhSize_hhComp_com),
                        .f = cenus_syth_combine, .progress = TRUE, .options = furrr::furrr_options(seed = TRUE))
  future::plan("sequential")
  res_com = dplyr::bind_rows(res_com)

  #Alt mesaure for unusual LSOA
  lsoa_alt = unique(Acc_tenure$LSOA21CD)
  lsoa_alt = lsoa_alt[!lsoa_alt %in% lsoa_common]

  Acc_tenure_alt = Acc_tenure[Acc_tenure$LSOA21CD %in% lsoa_alt,]
  hhComp_Tenure_alt = hhComp_Tenure[hhComp_Tenure$LSOA21CD %in% lsoa_alt,]
  Tenure_hhSize_alt = Tenure_hhSize[Tenure_hhSize$LSOA21CD %in% lsoa_alt,]
  CarVan_hhComp_alt = CarVan_hhComp[CarVan_hhComp$LSOA21CD %in% lsoa_alt,]

  Acc_tenure_alt = tidyr::pivot_wider(Acc_tenure_alt, names_from = "Tenure5", values_from = "households")
  hhComp_Tenure_alt = tidyr::pivot_wider(hhComp_Tenure_alt, names_from = "hhComp6", values_from = "households")
  Tenure_hhSize_alt = tidyr::pivot_wider(Tenure_hhSize_alt, names_from = "hhSize5", values_from = "households")
  CarVan_hhComp_alt = tidyr::pivot_wider(CarVan_hhComp_alt, names_from = "hhComp6", values_from = "households")

  Acc_tenure_alt    = Acc_tenure_alt[order(Acc_tenure_alt$LSOA21CD),]
  hhComp_Tenure_alt = hhComp_Tenure_alt[order(hhComp_Tenure_alt$LSOA21CD),]
  Tenure_hhSize_alt = Tenure_hhSize_alt[order(Tenure_hhSize_alt$LSOA21CD),]
  CarVan_hhComp_alt = CarVan_hhComp_alt[order(CarVan_hhComp_alt$LSOA21CD),]

  Acc_tenure_alt = dplyr::group_split(Acc_tenure_alt, LSOA21CD)
  hhComp_Tenure_alt = dplyr::group_split(hhComp_Tenure_alt, LSOA21CD)
  Tenure_hhSize_alt = dplyr::group_split(Tenure_hhSize_alt, LSOA21CD)
  CarVan_hhComp_alt = dplyr::group_split(CarVan_hhComp_alt, LSOA21CD)

  # Combine
  future::plan("multisession")
  res_alt = furrr::future_pmap(.l = list(Acc_tenure_alt, hhComp_Tenure_alt, Tenure_hhSize_alt, CarVan_hhComp_alt),
  .f = cenus_syth_combine_alt, .progress = TRUE,  .options = furrr::furrr_options(seed = TRUE))
  future::plan("sequential")
  res_alt = dplyr::bind_rows(res_alt)

  lsoaid = "E01000001"
  Acc_tenure_sub = Acc_tenure_com[Acc_tenure_com$LSOA21CD == lsoaid,]
  hhComp_Tenure_sub = hhComp_Tenure_com[hhComp_Tenure_com$LSOA21CD == lsoaid,]
  Tenure_hhSize_CarVan_sub = Tenure_hhSize_CarVan_com[Tenure_hhSize_CarVan_com$LSOA21CD == lsoaid,]

  # # Acc_tenure_sub = Acc_tenure_com[Acc_tenure_com$LSOA21CD == lsoaid,]
  # # Acc_CarVan_sub = Acc_CarVan_com[Acc_CarVan_com$LSOA21CD == lsoaid,]
  # # Acc_hhComp_sub = Acc_hhComp_com[Acc_hhComp_com$LSOA21CD == lsoaid,]
  # # hhSize_hhComp_sub = hhSize_hhComp_com[hhSize_hhComp_com$LSOA21CD == lsoaid,]
  #
  # hhComp_Tenure_sub = hhComp_Tenure_alt[hhComp_Tenure_alt$LSOA21CD == lsoaid,]
  # Tenure_hhSize_sub = Tenure_hhSize_alt[Tenure_hhSize_alt$LSOA21CD == lsoaid,]
  # CarVan_hhComp_sub = CarVan_hhComp_alt[CarVan_hhComp_alt$LSOA21CD == lsoaid,]
  # Acc_tenure_sub = Acc_tenure_alt[Acc_tenure_alt$LSOA21CD == lsoaid,]

  res_com = rbind(res_com, res_alt)

  res_com
}

simplify_Tenure5 = function(x){
  x[x=="Owned: Owns outright"] = "Outright"
  x[x=="Owned: Owns with a mortgage or loan or shared ownership"] = "Mortgage"
  x[x=="Rented: Social rented"] = "Social_rented"
  x[x=="Private rented or lives rent free"] = "Private_rented"
  x
}

simplify_hhSize5 = function(x){
  x[x=="1 person in household"] = "p1"
  x[x=="2 people in household"] = "p2"
  x[x=="3 people in household"] = "p3"
  x[x=="4 or more people in household"] = "p4+"
  x
}

simplify_CarVan5 = function(x){
  x[x=="No cars or vans in household"] = "car0"
  x[x=="1 car or van in household"] = "car1"
  x[x=="2 cars or vans in household"] = "car2"
  x[x=="3 or more cars or vans in household"] = "car3+"
  x
}

simplify_AccType5= function(x){
  x[x=="Whole house or bungalow: Detached"] = "Detached"
  x[x=="Whole house or bungalow: Semi-detached"] = "Semi"
  x[x=="Whole house or bungalow: Terraced"] = "Terraced"
  x[x=="Flat, maisonette or apartment"] = "Flat"
  x[x=="A caravan or other mobile or temporary structure"] = "caravan"
  x
}

cenus_syth_combine_alt = function(Acc_tenure_sub, hhComp_Tenure_sub, Tenure_hhSize_CarVan_sub
                                  #Tenure_hhSize_sub, CarVan_hhComp_sub
                                  ) {

  # Check LSOA match
  # if(length(unique(c(Acc_tenure_sub$LSOA21CD,
  #                    hhComp_Tenure_sub$LSOA21CD,
  #                    Tenure_hhSize_sub$LSOA21CD,
  #                    CarVan_hhComp_sub$LSOA21CD))) != 1){
  #   stop("More than one LSOA")
  # }

  # Check LSOA match
  if(length(unique(c(Acc_tenure_sub$LSOA21CD,
                     hhComp_Tenure_sub$LSOA21CD,
                     Tenure_hhSize_CarVan_sub$LSOA21CD))) != 1){
    stop("More than one LSOA")
  }

  AccByTenure = as.matrix(Acc_tenure_sub[,3:ncol(Acc_tenure_sub)])
  hhCompByTenure = as.matrix(hhComp_Tenure_sub[,3:ncol(hhComp_Tenure_sub)])
  #TenureByhhSize = as.matrix(Tenure_hhSize_sub[,3:ncol(Tenure_hhSize_sub)])
  #CarVanByhhComp = as.matrix(CarVan_hhComp_sub[,3:ncol(CarVan_hhComp_sub)])
  hhSizeCarVanByTenure = as.matrix(Tenure_hhSize_CarVan_sub[,3:ncol(Tenure_hhSize_CarVan_sub)])

  #nms_car = c("car0","car1","car2","car3+")
  #nms_tenure = c("Outright","Mortgage","Social_rented","Private_rented")
  #nms_acc = c("Detached","Semi","Terrace","Flat", "Caravan")
  #nms_comp = c("Oneperson", "family66","Couple","Loneparent","Other")
  #nms_size = c("p1", "p2","p3","p4+")

  rownames(AccByTenure) = Acc_tenure_sub$AccType5
  rownames(hhCompByTenure) = hhComp_Tenure_sub$hhComp15b
  rownames(hhSizeCarVanByTenure) = Tenure_hhSize_CarVan_sub$hhSize5_CarVan5

  # Harmonise Sums
  TenureByhhComp = match_matrix_rsums(t(AccByTenure), TenureByhhComp)
  TenureByhhSize = match_matrix_rsums(t(AccByTenure), TenureByhhSize)
  CarVanByhhComp = match_matrix_csums(TenureByhhComp, CarVanByhhComp)

  # Humanleauge 4 tables
  seed = array(rep(1,sum(AccByTenure)), dim=c(5,4,5,4,4))
  result = humanleague::qisi(seed,
                             indices = list(c(1,2), c(2,3),c(2,4),c(5,3)),
                             marginals = list(AccByTenure, TenureByhhComp,TenureByhhSize,CarVanByhhComp))

  result_df = expand.grid(nms_acc, nms_tenure,nms_comp,nms_size,nms_car)
  names(result_df) = c("Acc","Tenure","hhComp","hhSize","Car")
  result_df$households = as.numeric(result$result)
  result_df = result_df[result_df$households > 0,]
  result_df$conv = result$conv
  result_df$pValue = result$pValue


  # Integrity checks
  if(sum(result_df$households[result_df$Tenure == "Mortgage" & result_df$Acc == "Flat"]) !=
     AccByTenure["Flat","Mortgage"]){
    stop("check 1 failed")
  }
  if(sum(result_df$households[result_df$Tenure == "Outright" & result_df$hhComp == "Couple"]) !=
     TenureByhhComp["Outright","Couple"]){
    stop("check 2 failed")
  }
  if(sum(result_df$households[result_df$Tenure == "Private_rented" & result_df$hhSize == "p2"]) !=
     TenureByhhSize["Private_rented","p2"]){
    stop("check 3 failed")
  }
  if(sum(result_df$households[result_df$Car == "car1" & result_df$hhComp == "Loneparent"]) !=
     CarVanByhhComp["car1","Loneparent"]){
    stop("check 4 failed")
  }

  result_df$LSOA = Acc_tenure_sub$LSOA21CD[1]

  result_df

}

cenus_syth_combine = function(Acc_tenure_sub, Acc_CarVan_sub, Acc_hhComp_sub, hhSize_hhComp_sub#,
                              #hhComp_Tenure_sub, Tenure_hhSize_sub, CarVan_hhComp_sub, Acc_hhSize_sub
                              ) {

  # Check LSOA match
  if(length(unique(c(Acc_tenure_sub$LSOA21CD,
                     Acc_CarVan_sub$LSOA21CD,
                     Acc_hhComp_sub$LSOA21CD,
                     hhSize_hhComp_sub$LSOA21CD))) != 1){
    stop("More than one LSOA")
  }

  AccByTenure = as.matrix(Acc_tenure_sub[,3:ncol(Acc_tenure_sub)])
  AccByCarVan = as.matrix(Acc_CarVan_sub[,3:ncol(Acc_CarVan_sub)])
  AccByhhComp = as.matrix(Acc_hhComp_sub[,3:ncol(Acc_hhComp_sub)])
  hhSizeByhhComp = as.matrix(hhSize_hhComp_sub[,3:ncol(hhSize_hhComp_sub)])

  nms_car = c("car0","car1","car2","car3+")
  nms_tenure = c("Outright","Mortgage","Social_rented","Private_rented")
  nms_acc = c("Detached","Semi","Terrace","Flat", "Caravan")
  nms_comp = c("Oneperson", "family66","Couple","Loneparent","Other")
  nms_size = c("p1", "p2","p3","p4+")

  rownames(AccByTenure) = c(nms_acc)
  colnames(AccByTenure) = c(nms_tenure)

  rownames(AccByCarVan) = c(nms_acc)
  colnames(AccByCarVan) = c(nms_car)

  rownames(AccByhhComp) = c(nms_acc)
  colnames(AccByhhComp) = c(nms_comp)

  rownames(hhSizeByhhComp) = c(nms_size)
  colnames(hhSizeByhhComp) = c(nms_comp)


  # Harmonise Sums
  AccByhhComp = match_matrix_csums(hhSizeByhhComp, AccByhhComp)
  AccByTenure = match_matrix_rsums(AccByhhComp, AccByTenure)
  AccByCarVan = match_matrix_rsums(AccByTenure, AccByCarVan)

  # Humanleauge 4 tables
  seed = array(rep(1,sum(AccByTenure)), dim=c(5,4,4,5,4))
  result = humanleague::qisi(seed,
                indices = list(c(1,2), c(1,3),c(1,4),c(5,4)),
                marginals = list(AccByTenure, AccByCarVan,AccByhhComp,hhSizeByhhComp))

  result_df = expand.grid(nms_acc, nms_tenure,nms_car,nms_comp,nms_size)
  names(result_df) = c("Acc","Tenure","Car","hhComp","hhSize")
  result_df$households = as.numeric(result$result)
  result_df = result_df[result_df$households > 0,]
  result_df$conv = result$conv
  result_df$pValue = result$pValue



  # Integrity checks
  if(sum(result_df$households[result_df$hhComp == "Oneperson" & result_df$hhSize == "p1"]) !=
     hhSizeByhhComp["p1","Oneperson"]){
    stop("check 1 failed")
  }
  if(sum(result_df$households[result_df$hhComp == "Couple" & result_df$Acc == "Terrace"]) !=
     AccByhhComp["Terrace","Couple"]){
    stop("check 2 failed")
  }
  if(sum(result_df$households[result_df$Car == "car2" & result_df$Acc == "Detached"]) !=
     AccByCarVan["Detached","car2"]){
    stop("check 3 failed")
  }
  if(sum(result_df$households[result_df$Tenure == "Social_rented" & result_df$Acc == "Flat"]) !=
     AccByTenure["Flat","Social_rented"]){
    stop("check 4 failed")
  }

  result_df$LSOA = Acc_tenure_sub$LSOA21CD[1]

  result_df

}


# Adjust mat2 so that it has the same rowsums as mat1
# humnaleauge requires input matrixes to have the same total sums
# but in many cases the rows for two matrix refer to the same variaible so should match
match_matrix_rsums <- function(mat1, mat2) {
  # Calculate the row sums of both matrices
  row_sums1 <- rowSums(mat1)
  row_sums2 <- rowSums(mat2)

  if(all(row_sums1 == row_sums2)){
    return(mat2)
  }

  # Calculate the difference in row sums
  row_diff <- row_sums1 - row_sums2


  # Adjust mat2 to match the row sums of mat1
  for (i in 1:nrow(mat2)) {
    adjustment <- row_diff[i]
    while (adjustment != 0) {
      for (j in 1:ncol(mat2)) {
        if (adjustment == 0) break
        if (adjustment > 0) {
          mat2[i, j] <- mat2[i, j] + 1
          adjustment <- adjustment - 1
        } else if(mat2[i, j] > 0){
          mat2[i, j] <- mat2[i, j] - 1
          adjustment <- adjustment + 1
        }
      }
    }
  }
  return(mat2)
}

match_matrix_csums <- function(mat1, mat2) {
  # Calculate the col sums of both matrices
  col_sums1 <- colSums(mat1)
  col_sums2 <- colSums(mat2)

  if(all(col_sums1 == col_sums2)){
    return(mat2)
  }

  # Calculate the difference in col sums
  col_diff <- col_sums1 - col_sums2


  # Adjust mat2 to match the col sums of mat1
  for (i in 1:ncol(mat2)) {
    adjustment <- col_diff[i]
    while (adjustment != 0) {
      for (j in 1:nrow(mat2)) {
        if (adjustment == 0) break
        if (adjustment > 0) {
          mat2[j, i] <- mat2[j, i] + 1
          adjustment <- adjustment - 1
        } else if(mat2[j, i] > 0){
          mat2[j, i] <- mat2[j, i] - 1
          adjustment <- adjustment + 1
        }
      }
    }
  }

  return(mat2)
}
