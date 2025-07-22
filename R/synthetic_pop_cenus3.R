read_Acc_hhComp = function(path = "../inputdata/population/census2021EW_Households_HouseholdComposition15_AccomodationType5_LSOA_partial.csv"){

  raw = readr::read_csv(path, show_col_types = FALSE)

  names(raw) = c("LSOA21CD","LSOA21NM","hhComp15CD","hhComp15","AccType5CD","AccType5","households")
  raw = raw[,c("LSOA21CD","AccType5","hhComp15","households")]
  raw = raw[raw$hhComp15 != "Does not apply",]
  raw$AccType5 = simplify_AccType5(raw$AccType5)
  raw$hhComp15 = simplify_household15(raw$hhComp15)

  # Simplify hhComp15 as don't know married etc
  raw$hhComp15 = gsub("Cohabit|Married","Couple",raw$hhComp15)
  raw = dplyr::group_by(raw, LSOA21CD, AccType5, hhComp15)
  raw = dplyr::summarise(raw, households = sum(households))
  raw = dplyr::ungroup(raw)


  raw

}

read_Acc_hhComp6 = function(path = "../inputdata/population/census2021EW_Households_AccomodationType5_HouseholdComposition6_LSOA_partial.csv"){

  raw = readr::read_csv(path, show_col_types = FALSE)

  names(raw) = c("LSOA21CD","LSOA21NM","hhComp6CD","hhComp6","AccType5CD","AccType5","households")
  raw = raw[,c("LSOA21CD","AccType5","hhComp6","households")]
  raw = raw[raw$hhComp6 != "Does not apply",]
  raw$AccType5 = simplify_AccType5(raw$AccType5)
  raw$hhComp6 = simplify_household6(raw$hhComp6)

  raw

}


read_Acc_tenure = function(path = "../inputdata/population/census2021EW_Households_AccomodationType5_Tenure5_LSOA.csv"){

  raw = readr::read_csv(path, show_col_types = FALSE)

  names(raw) = c("LSOA21CD","LSOA21NM","Tenure5CD","Tenure5","AccType5CD","AccType5","households")
  raw = raw[,c("LSOA21CD","AccType5","Tenure5","households")]

  raw = raw[raw$Tenure5 != "Does not apply",]

  raw$AccType5 = simplify_AccType5(raw$AccType5)
  raw$Tenure5 = simplify_Tenure5(raw$Tenure5)

  raw

}

read_Acc_CarVan = function(path = "../inputdata/population/census2021EW_Households_AccomodationType5_CarVan5_LSOA_partial.csv"){

  raw = readr::read_csv(path, show_col_types = FALSE)

  names(raw) = c("LSOA21CD","LSOA21NM","AccType5CD","AccType5","CarVan5CD","CarVan5","households")
  raw = raw[,c("LSOA21CD","AccType5","CarVan5","households")]
  raw = raw[raw$CarVan5 != "Does not apply",]

  raw$AccType5 = simplify_AccType5(raw$AccType5)
  raw$CarVan5 = simplify_CarVan5(raw$CarVan5)

  raw

}

read_hhSize_hhComp = function(path = "../inputdata/population/census2021EW_Households_HouseholdComposition6_HouseholdSize5_LSOA_partial.csv"){

  raw = readr::read_csv(path, show_col_types = FALSE)

  names(raw) = c("LSOA21CD","LSOA21NM","hhComp6CD","hhComp6","hhSize5CD","hhSize5","households")
  raw = raw[,c("LSOA21CD","hhComp6","hhSize5","households")]
  raw = raw[raw$hhSize5 != "0 people in household",]
  raw = raw[raw$hhComp6 != "Does not apply",]
  raw$hhSize5 = simplify_hhSize5(raw$hhSize5)
  raw$hhComp6 = simplify_household6(raw$hhComp6)
  raw

}

read_Acc_hhSize = function(path = "../inputdata/population/census2021EW_Households_AccomodationType5_HousehholdSize5_LSOA_partial.csv"){

  raw = readr::read_csv(path, show_col_types = FALSE)

  names(raw) = c("LSOA21CD","LSOA21NM","hhSize5CD","hhSize5","AccType5CD","AccType5","households")
  raw = raw[,c("LSOA21CD","hhSize5","AccType5","households")]
  raw = raw[raw$hhSize5 != "0 people in household",]

  raw$hhSize5 = simplify_hhSize5(raw$hhSize5)
  raw$AccType5 = simplify_AccType5(raw$AccType5)

  raw

}

read_CarVan_hhComp = function(path = "../inputdata/population/census2021EW_Households_HouseholdComposition6_CarVan5_LSOA.csv"){

  raw = readr::read_csv(path, show_col_types = FALSE)

  names(raw) = c("LSOA21CD","LSOA21NM","hhComp6CD","hhComp6","CarVan5CD","CarVan5","households")
  raw = raw[,c("LSOA21CD","hhComp6","CarVan5","households")]
  raw = raw[raw$CarVan5 != "Does not apply",]
  raw = raw[raw$hhComp6 != "Does not apply",]

  raw$CarVan5 = simplify_CarVan5(raw$CarVan5)
  raw$hhComp6 = simplify_household6(raw$hhComp6)

  raw

}

read_Tenure_hhSize = function(path = "../inputdata/population/census2021EW_Households_Tenure5_HouseholdSize5_LSOA.csv"){

  raw = readr::read_csv(path, show_col_types = FALSE)

  names(raw) = c("LSOA21CD","LSOA21NM","Tenure5CD","Tenure5","hhSize5CD","hhSize5","households")
  raw = raw[,c("LSOA21CD","Tenure5","hhSize5","households")]
  raw = raw[raw$hhSize5 != "0 people in household",]
  raw = raw[raw$Tenure5 != "Does not apply",]

  raw$hhSize5 = simplify_hhSize5(raw$hhSize5)
  raw$Tenure5 = simplify_Tenure5(raw$Tenure5)


  raw

}

read_Tenure_CarVan = function(path = "../inputdata/population/census2021EW_Households_Tenure5_CarVan5_LSOA.csv"){

  raw = readr::read_csv(path, show_col_types = FALSE)

  names(raw) = c("LSOA21CD","LSOA21NM","CarVan5CD","CarVan5","Tenure5CD","Tenure5","households")
  raw = raw[,c("LSOA21CD","Tenure5","CarVan5","households")]
  raw = raw[raw$CarVan5 != "Does not apply",]
  raw = raw[raw$Tenure5 != "Does not apply",]

  raw$Tenure5 = simplify_Tenure5(raw$Tenure5)
  raw$CarVan5 = simplify_CarVan5(raw$CarVan5)
  raw

}


read_Tenure_CarVan_hhComp6 = function(path = "../inputdata/population/census2021EW_Households_Tenure5_HouseholdComposition6_CarVan5_LSOA_partial.csv"){

  raw = readr::read_csv(path, show_col_types = FALSE)

  names(raw) = c("LSOA21CD","LSOA21NM","hhComp6Code","hhComp6","CarVan5Code","CarVan5","Tenure5Code","Tenure5","households")
  raw = raw[,c("LSOA21CD","hhComp6","Tenure5","CarVan5","households")]
  raw = raw[raw$CarVan5 != "Does not apply",]
  raw = raw[raw$hhComp6 != "Does not apply",]
  raw = raw[raw$Tenure5 != "Does not apply",]

  raw$hhComp6 = simplify_household6(raw$hhComp6)
  raw$Tenure5 = simplify_Tenure5(raw$Tenure5)
  raw$CarVan5 = simplify_CarVan5(raw$CarVan5)
  raw

}

read_Tenure_hhSize_CarVan = function(path = "../inputdata/population/census2021EW_Households_Tenure5_HouseholdSize5_CarVan5_LSOA_partial.csv"){

  raw = readr::read_csv(path, show_col_types = FALSE)

  names(raw) = c("LSOA21CD","LSOA21NM","CarVan5CD","CarVan5","Tenure5CD","Tenure5","hhSize5CD","hhSize5","households")
  raw = raw[,c("LSOA21CD","Tenure5","hhSize5","CarVan5","households")]
  raw = raw[raw$hhSize5 != "0 people in household",]
  raw = raw[raw$Tenure5 != "Does not apply",]
  raw = raw[raw$CarVan5 != "Does not apply",]

  raw$Tenure5 = simplify_Tenure5(raw$Tenure5)
  raw$CarVan5 = simplify_CarVan5(raw$CarVan5)
  raw$hhSize5 = simplify_hhSize5(raw$hhSize5)

  raw

}

read_hhSize_CarVan = function(path = "../inputdata/population/census2021EW_Households_hhSize5_CarVan5_LSOA_partial.csv"){

  raw = readr::read_csv(path, show_col_types = FALSE)

  names(raw) = c("LSOA21CD","LSOA21NM","CarVan5CD","CarVan5","hhSize5CD","hhSize5","households")
  raw = raw[,c("LSOA21CD","hhSize5","CarVan5","households")]
  raw = raw[raw$hhSize5 != "0 people in household",]
  raw = raw[raw$CarVan5 != "Does not apply",]

  raw$CarVan5 = simplify_CarVan5(raw$CarVan5)
  raw$hhSize5 = simplify_hhSize5(raw$hhSize5)

  raw

}

read_Tenure_NSSEC = function(path = "../inputdata/population/census2021EW_RefPerson_NSSEC10_Tenure5_LSOA.csv"){

  raw = readr::read_csv(path, show_col_types = FALSE)

  names(raw) = c("LSOA21CD","LSOA21NM","Tenure5CD","Tenure5","NSSEC10CD","NSSEC10","households")
  raw = raw[,c("LSOA21CD","Tenure5","NSSEC10","households")]

  raw = raw[raw$Tenure5 != "Does not apply",]

  raw

}

read_hhComp_NSSEC = function(path = "../inputdata/population/census2021EW_RefPerson_NSSEC10_Houshold15_LSOA_partial.csv"){

  raw = readr::read_csv(path, show_col_types = FALSE)

  names(raw) = c("LSOA21CD","LSOA21NM","hhComp15CD","hhComp15","NSSEC10CD","NSSEC10","households")
  raw = raw[,c("LSOA21CD","hhComp15","NSSEC10","households")]

  raw = raw[raw$hhComp15 != "Does not apply",]

  raw

}


read_hhComp_Tenure = function(path = "../inputdata/population/census2021EW_Households_HouseholdComposition15_Tenure5_LSOA.csv"){

  raw = readr::read_csv(path, show_col_types = FALSE)

  names(raw) = c("LSOA21CD","LSOA21NM","hhComp15CD","hhComp15","Tenure5CD","Tenure5","households")
  raw = raw[,c("LSOA21CD","hhComp15","Tenure5","households")]

  raw = raw[raw$Tenure5 != "Does not apply",]
  raw = raw[raw$hhComp15 != "Does not apply",]

  raw$Tenure5 = simplify_Tenure5(raw$Tenure5)
  raw$hhComp15 = simplify_household15(raw$hhComp15)

  # Simplify hhComp15 as don't know married etc
  raw$hhComp15 = gsub("Cohabit|Married","Couple",raw$hhComp15)
  raw = dplyr::group_by(raw, LSOA21CD, Tenure5, hhComp15)
  raw = dplyr::summarise(raw, households = sum(households))
  raw = dplyr::ungroup(raw)



  raw

}

read_hhComp6_Tenure = function(path = "../inputdata/population/census2021EW_Households_Tenure5_HouseholdComposition6_LSOA.csv"){

  raw = readr::read_csv(path, show_col_types = FALSE)

  names(raw) = c("LSOA21CD","LSOA21NM","hhComp6CD","hhComp6","Tenure5CD","Tenure5","households")
  raw = raw[,c("LSOA21CD","hhComp6","Tenure5","households")]

  raw = raw[raw$Tenure5 != "Does not apply",]
  raw = raw[raw$hhComp6 != "Does not apply",]

  raw$Tenure5 = simplify_Tenure5(raw$Tenure5)
  raw$hhComp6 = simplify_household6(raw$hhComp6)

  raw

}

read_hhComp6_Tenure = function(path = "../inputdata/population/census2021EW_Households_HouseholdComposition6_Tenure5_LSOA.csv"){

  raw = readr::read_csv(path, show_col_types = FALSE)

  names(raw) = c("LSOA21CD","LSOA21NM","hhComp6CD","hhComp6","Tenure5CD","Tenure5","households")
  raw = raw[,c("LSOA21CD","hhComp6","Tenure5","households")]
  raw = raw[raw$hhComp6 != "Does not apply",]
  raw = raw[raw$Tenure5 != "Does not apply",]

  raw$Tenure5 = simplify_Tenure5(raw$Tenure5)
  raw$hhComp6 = simplify_household6(raw$hhComp6)

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

split_for_arrays = function(x, lsoa_common){
  x = x[x$LSOA21CD %in% lsoa_common,]
  x = x[order(x$LSOA21CD),]
  x = dplyr::group_split(x, LSOA21CD)
  x
}

split_for_arrays2 = function(x, lsoa_all){
  y = lapply(lsoa_all, function(lsoa){
    sub = x[x$LSOA21CD == lsoa,]
    if(nrow(sub) == 0){
      return(NULL)
    }
    sub
    })
  y
}

split_for_arrays3 = function(x, lsoa_all){
  x = x[order(x$LSOA21CD),]
  groups = dplyr::group_split(x, x$LSOA21CD)
  names(groups) = vapply(groups, function(y){y$LSOA21CD[1]},"char")
  groups = groups[match(lsoa_all, names(groups))]
  class(groups) = "list"
  groups
}


sythetic_census = function(path_data = file.path(parameters$path_data,"population"), synth_pop_seed){

  Acc_hhComp = read_Acc_hhComp(file.path(path_data,"census2021EW_Households_HouseholdComposition15_AccomodationType5_LSOA_partial.csv"))  # Partial
  Acc_hhComp6 = read_Acc_hhComp6(file.path(path_data,"census2021EW_Households_HouseholdComposition6_AccType5_LSOA_partial.csv"))  # Partial
  Acc_CarVan = read_Acc_CarVan(file.path(path_data,"census2021EW_Households_AccomodationType5_CarVan5_LSOA_partial.csv"))  # Partial
  hhSize_hhComp = read_hhSize_hhComp(file.path(path_data,"census2021EW_Households_HouseholdComposition6_HouseholdSize5_LSOA_partial.csv"))  # Partial
  Tenure_hhSize_CarVan = read_Tenure_hhSize_CarVan(file.path(path_data,"census2021EW_Households_Tenure5_HouseholdSize5_CarVan5_LSOA_partial.csv")) # Partial
  Acc_hhSize = read_Acc_hhSize(file.path(path_data,"census2021EW_Households_AccomodationType5_HousehholdSize5_LSOA_partial.csv")) # Partial
  Tenure_CarVan_hhComp6 = read_Tenure_CarVan_hhComp6(file.path(path_data,"census2021EW_Households_Tenure5_HouseholdComposition6_CarVan5_LSOA_partial.csv")) # Partial
  hhSize_CarVan = read_hhSize_CarVan(file.path(path_data,"census2021EW_Households_hhSize5_CarVan5_LSOA_partial.csv")) # Partial

  Acc_tenure = read_Acc_tenure(file.path(path_data,"census2021EW_Households_AccomodationType5_Tenure5_LSOA.csv"))
  hhComp_Tenure = read_hhComp_Tenure(file.path(path_data,"census2021EW_Households_HouseholdComposition15_Tenure5_LSOA.csv"))
  hhComp6_Tenure = read_hhComp6_Tenure(file.path(path_data,"census2021EW_Households_Tenure5_HouseholdComposition6_LSOA.csv"))
  Tenure_hhSize = read_Tenure_hhSize(file.path(path_data,"census2021EW_Households_Tenure5_HouseholdSize5_LSOA.csv"))
  CarVan_hhComp = read_CarVan_hhComp(file.path(path_data,"census2021EW_Households_HouseholdComposition6_CarVan5_LSOA.csv"))
  Tenure_CarVan = read_Tenure_CarVan(file.path(path_data,"census2021EW_Households_Tenure5_CarVan5_LSOA.csv"))

  # # Find common core round 1
  # lsoa_common = Reduce(intersect,
  #                      list(
  #                        unique(Acc_hhComp6$LSOA21CD), #35333
  #                        unique(Acc_hhComp$LSOA21CD), #20676
  #                        unique(Acc_hhSize$LSOA21CD), #35322
  #                        unique(Acc_CarVan$LSOA21CD), #35335
  #                        unique(hhSize_hhComp$LSOA21CD), #35601
  #                        unique(Tenure_hhSize_CarVan$LSOA21CD), #35148
  #                        unique(Tenure_CarVan_hhComp6$LSOA21CD) #32639
  #                      )) #19310/35672 54%
  # # Second wave fo about 34852/35672 97.7 % if drop the Acc_hhComp

  lsoa_all = unique(Acc_tenure$LSOA21CD)
  lsoa_all = lsoa_all[order(lsoa_all)]

  # Common Core Datasets
  Acc_tenure_com = split_for_arrays3(Acc_tenure, lsoa_all)
  hhComp_Tenure_com = split_for_arrays3(hhComp_Tenure, lsoa_all)
  Tenure_hhSize_CarVan_com = split_for_arrays3(Tenure_hhSize_CarVan, lsoa_all)
  Acc_hhComp_com = split_for_arrays3(Acc_hhComp, lsoa_all)
  Acc_hhComp6_com = split_for_arrays3(Acc_hhComp6, lsoa_all)
  Acc_CarVan_com = split_for_arrays3(Acc_CarVan, lsoa_all)
  hhSize_hhComp_com = split_for_arrays3(hhSize_hhComp, lsoa_all)
  Acc_hhSize_com = split_for_arrays3(Acc_hhSize, lsoa_all)
  hhComp6_Tenure_com = split_for_arrays3(hhComp6_Tenure, lsoa_all)
  Tenure_hhSize_com = split_for_arrays3(Tenure_hhSize, lsoa_all)
  CarVan_hhComp_com = split_for_arrays3(CarVan_hhComp, lsoa_all)
  Tenure_CarVan_com = split_for_arrays3(Tenure_CarVan, lsoa_all)
  Tenure_CarVan_hhComp6_com = split_for_arrays3(Tenure_CarVan_hhComp6, lsoa_all)
  hhSize_CarVan_com = split_for_arrays3(hhSize_CarVan, lsoa_all)

  # Acc_tenure_sub = Acc_tenure_com[[1]]
  # hhComp_Tenure_sub = hhComp_Tenure_com[[1]]
  # Tenure_hhSize_CarVan_sub = Tenure_hhSize_CarVan_com[[1]]
  # Acc_hhComp_sub = Acc_hhComp_com[[1]]
  # Acc_hhComp6_sub = Acc_hhComp6_com[[1]]
  # Acc_CarVan_sub = Acc_CarVan_com[[1]]
  # hhSize_hhComp_sub = hhSize_hhComp_com[[1]]
  # Acc_hhSize_sub = Acc_hhSize_com[[1]]
  # hhComp6_Tenure_sub = hhComp6_Tenure_com[[1]]
  # Tenure_hhSize_sub = Tenure_hhSize_com[[1]]
  # CarVan_hhComp_sub = CarVan_hhComp_com[[1]]
  # Tenure_CarVan_sub = Tenure_CarVan_com[[1]]
  # Tenure_CarVan_hhComp6_sub = Tenure_CarVan_hhComp6_com[[1]]
  # hhSize_CarVan_sub = hhSize_CarVan_com[[1]]

  # Iterate over LSOAs (1st round, common core about 54%)
  future::plan("multisession")
  res_1 = furrr::future_pmap(.l = list(Acc_tenure_sub = Acc_tenure_com,
                                       hhComp_Tenure_sub = hhComp_Tenure_com,
                                       Tenure_hhSize_CarVan_sub = Tenure_hhSize_CarVan_com,
                                       Acc_hhComp_sub = Acc_hhComp_com,
                                       Acc_hhComp6_sub = Acc_hhComp6_com,
                                       Acc_CarVan_sub = Acc_CarVan_com,
                                       hhSize_hhComp_sub = hhSize_hhComp_com,
                                       Acc_hhSize_sub = Acc_hhSize_com,
                                       hhComp6_Tenure_sub = hhComp6_Tenure_com,
                                       CarVan_hhComp_sub = CarVan_hhComp_com,
                                       Tenure_CarVan_hhComp6_sub = Tenure_CarVan_hhComp6_com,
                                       Tenure_hhSize_sub = Tenure_hhSize_com,
                                       Tenure_CarVan_sub = Tenure_CarVan_com,
                                       hhSize_CarVan_sub = hhSize_CarVan_com
                                       ),
                             .f = census_syth_combine_v4,
                             seed = synth_pop_seed,
                             iter = 1,
                             .progress = TRUE,
                             .options = furrr::furrr_options(seed = TRUE))
  future::plan("sequential")
  res_1 = dplyr::bind_rows(res_1)

  res_1

}



census_syth_combine_v4 = function(Acc_tenure_sub,
                                  hhComp_Tenure_sub,
                                  Tenure_hhSize_CarVan_sub,
                                  Acc_hhComp_sub,
                                  Acc_hhComp6_sub,
                                  Acc_CarVan_sub,
                                  hhSize_hhComp_sub,
                                  Acc_hhSize_sub,
                                  hhComp6_Tenure_sub,
                                  CarVan_hhComp_sub,
                                  Tenure_CarVan_hhComp6_sub,
                                  Tenure_hhSize_sub,
                                  Tenure_CarVan_sub,
                                  hhSize_CarVan_sub,
                                  seed,
                                  iter = 1000) {


  # Prefer mulivariate to bivariate versions
  if(!is.null(Tenure_hhSize_CarVan_sub)){
    Tenure_hhSize_sub <- NULL
    Tenure_CarVan_sub <- NULL
    hhSize_CarVan_sub <- NULL
  }
  if(!is.null(Tenure_CarVan_hhComp6_sub)){
    hhComp6_Tenure_sub <- NULL
    Tenure_CarVan_sub <- NULL
    CarVan_hhComp_sub <- NULL
  }

  # Check LSOA match
  lsoa_check = c(
     if(!is.null(Acc_tenure_sub)){Acc_tenure_sub$LSOA21CD},
     if(!is.null(hhComp_Tenure_sub)){hhComp_Tenure_sub$LSOA21CD},
     if(!is.null(Tenure_hhSize_CarVan_sub)){Tenure_hhSize_CarVan_sub$LSOA21CD},
     if(!is.null(Acc_hhComp_sub)){Acc_hhComp_sub$LSOA21CD},
     if(!is.null(Acc_hhComp6_sub)){Acc_hhComp6_sub$LSOA21CD},
     if(!is.null(Acc_CarVan_sub)){Acc_CarVan_sub$LSOA21CD},
     if(!is.null(hhSize_hhComp_sub)){hhSize_hhComp_sub$LSOA21CD},
     if(!is.null(Acc_hhSize_sub)){Acc_hhSize_sub$LSOA21CD},
     if(!is.null(hhComp6_Tenure_sub)){hhComp6_Tenure_sub$LSOA21CD},
     if(!is.null(CarVan_hhComp_sub)){CarVan_hhComp_sub$LSOA21CD},
     if(!is.null(Tenure_CarVan_hhComp6_sub)){Tenure_CarVan_hhComp6_sub$LSOA21CD},
     if(!is.null(Tenure_hhSize_sub)){Tenure_hhSize_sub$LSOA21CD},
     if(!is.null(Tenure_CarVan_sub)){Tenure_CarVan_sub$LSOA21CD},
     if(!is.null(hhSize_CarVan_sub)){hhSize_CarVan_sub$LSOA21CD}
  )

  if(length(unique(lsoa_check)) != 1){
    stop("More than one LSOA")
  } else {
    lsoacd = Acc_tenure_sub$LSOA21CD[1]
  }

  hhComp15 = c("OnePersonOver66","OnePersonOther","FamilyOver66","CoupleNoChildren","CoupleChildren","CoupleNonDepChildren","LoneParent","LoneParentNonDepChildren","OtherChildren","OtherNoChildren","OtherIncStudentOrOver66")
  hhComp6 = c("OnePerson","FamilyOver66","CoupleFamily","LoneParent","Other6")
  hhSize5 = c("p1","p2","p3","p4+")
  Tenure5 = c("outright","mortgage","socialrented","privaterented")
  CarVan5 = c("car0","car1","car2","car3+")
  AccType5 = c("detached","semidetached","terraced","flat","caravan")

  # Make Arrays
  if(!is.null(Acc_tenure_sub)){
    Acc_tenure_sub = array_maker(Acc_tenure_sub, AccType5, Tenure5)
  }
  if(!is.null(hhComp_Tenure_sub)){
    hhComp_Tenure_sub = array_maker(hhComp_Tenure_sub, Tenure5,  hhComp15)
  }
  if(!is.null(Acc_hhComp6_sub)){
    Acc_hhComp6_sub = array_maker(Acc_hhComp6_sub, AccType5, hhComp6)
  }
  if(!is.null(Acc_CarVan_sub)){
    Acc_CarVan_sub = array_maker(Acc_CarVan_sub, AccType5, CarVan5)
  }
  if(!is.null(hhSize_hhComp_sub)){
    hhSize_hhComp_sub = array_maker(hhSize_hhComp_sub, hhComp6, hhSize5)
  }
  if(!is.null(Acc_hhSize_sub)){
    Acc_hhSize_sub = array_maker(Acc_hhSize_sub, hhSize5, AccType5)
  }
  if(!is.null(hhSize_CarVan_sub)){
    hhSize_CarVan_sub = array_maker(hhSize_CarVan_sub, hhSize5, CarVan5)
  }
  if(!is.null(CarVan_hhComp_sub)){
    CarVan_hhComp_sub = array_maker(CarVan_hhComp_sub, hhComp6, CarVan5)
  }
  if(!is.null(hhComp6_Tenure_sub)){
    hhComp6_Tenure_sub = array_maker(hhComp6_Tenure_sub, hhComp6, Tenure5)
  }
  if(!is.null(Acc_hhComp_sub)){
    Acc_hhComp_sub = array_maker(Acc_hhComp_sub, AccType5, hhComp15)
  }
  if(!is.null(Tenure_hhSize_CarVan_sub)){
    Tenure_hhSize_CarVan_sub = array_maker(Tenure_hhSize_CarVan_sub, Tenure5, hhSize5, CarVan5)
  }
  if(!is.null(Tenure_CarVan_hhComp6_sub)){
    Tenure_CarVan_hhComp6_sub = array_maker(Tenure_CarVan_hhComp6_sub, hhComp6, CarVan5, Tenure5)
  }
  if(!is.null(Tenure_hhSize_sub)){
    Tenure_hhSize_sub = array_maker(Tenure_hhSize_sub, Tenure5, hhSize5)
  }
  if(!is.null(Tenure_CarVan_sub)){
    Tenure_CarVan_sub = array_maker(Tenure_CarVan_sub, Tenure5, CarVan5)
  }

  # Make Seed
  # hhSize5, CarVan5, Tenure5, AccType5, hhComp6, hhComp15
  dim = c(length(hhSize5), length(CarVan5), length(Tenure5), length(AccType5), length(hhComp6), length(hhComp15))
  seed = array(seed$seed, dim = dim)
  med_pop = median(
    c(if(!is.null(Acc_tenure_sub)){sum(Acc_tenure_sub)},
     if(!is.null(hhComp_Tenure_sub)){sum(hhComp_Tenure_sub)},
     if(!is.null(Tenure_hhSize_CarVan_sub)){sum(Tenure_hhSize_CarVan_sub)},
     if(!is.null(Acc_hhComp_sub)){sum(Acc_hhComp_sub)},
     if(!is.null(Acc_hhComp6_sub)){sum(Acc_hhComp6_sub)},
     if(!is.null(Acc_CarVan_sub)){sum(Acc_CarVan_sub)},
     if(!is.null(hhSize_hhComp_sub)){sum(hhSize_hhComp_sub)},
     if(!is.null(Acc_hhSize_sub)){sum(Acc_hhSize_sub)},
     if(!is.null(hhComp6_Tenure_sub)){sum(hhComp6_Tenure_sub)},
     if(!is.null(CarVan_hhComp_sub)){sum(CarVan_hhComp_sub)},
     if(!is.null(Tenure_CarVan_hhComp6_sub)){sum(Tenure_CarVan_hhComp6_sub)},
     if(!is.null(Tenure_hhSize_sub)){sum(Tenure_hhSize_sub)},
     if(!is.null(Tenure_CarVan_sub)){sum(Tenure_CarVan_sub)},
     if(!is.null(hhSize_CarVan_sub)){sum(hhSize_CarVan_sub)}
  ))
  dimnames(seed) = list(hhSize5, CarVan5, Tenure5, AccType5, hhComp6, hhComp15)

  seed_weighted = (seed /sum(seed)) * med_pop

  #IPF
  # hhSize5, CarVan5, Tenure5, AccType5, hhComp6, hhComp15
  target.list <- list(
    if(!is.null(Acc_tenure_sub)){c(4,3)},
    if(!is.null(hhComp_Tenure_sub)){c(3,6)},
    if(!is.null(Tenure_hhSize_CarVan_sub)){c(3,1,2)},

    if(!is.null(Acc_hhComp_sub)){c(4,6)},
    if(!is.null(Acc_CarVan_sub)){c(4,2)},
    if(!is.null(hhSize_hhComp_sub)){c(5,1)},

    if(!is.null(Acc_hhSize_sub)){c(1,4)},
    if(!is.null(hhComp6_Tenure_sub)){c(5,3)},
    if(!is.null(CarVan_hhComp_sub)){c(5,2)},

    if(!is.null(Acc_hhComp6_sub)){c(4,5)},
    if(!is.null(Tenure_CarVan_hhComp6_sub)){c(5,2,3)},
    if(!is.null(Tenure_hhSize_sub)){c(3,1)},

    if(!is.null(Tenure_CarVan_sub)){c(3,2)},
    if(!is.null(hhSize_CarVan_sub)){c(1,2)}
  )
  target.data <- list(
    if(!is.null(Acc_tenure_sub)){Acc_tenure_sub}, #4,3
    if(!is.null(hhComp_Tenure_sub)){hhComp_Tenure_sub}, #3,6
    if(!is.null(Tenure_hhSize_CarVan_sub)){Tenure_hhSize_CarVan_sub}, #3,1,2

    if(!is.null(Acc_hhComp_sub)){Acc_hhComp_sub}, #4,6
    if(!is.null(Acc_CarVan_sub)){Acc_CarVan_sub}, #4,2
    if(!is.null(hhSize_hhComp_sub)){hhSize_hhComp_sub}, #5,1

    if(!is.null(Acc_hhSize_sub)){Acc_hhSize_sub},#1,4
    if(!is.null(hhComp6_Tenure_sub)){hhComp6_Tenure_sub},#5,3
    if(!is.null(CarVan_hhComp_sub)){CarVan_hhComp_sub},#5,2

    if(!is.null(Acc_hhComp6_sub)){Acc_hhComp6_sub},#4,5
    if(!is.null(Tenure_CarVan_hhComp6_sub)){Tenure_CarVan_hhComp6_sub}, #5,2,3
    if(!is.null(Tenure_hhSize_sub)){Tenure_hhSize_sub}, #3,1

    if(!is.null(Tenure_CarVan_sub)){Tenure_CarVan_sub}, #3,2
    if(!is.null(hhSize_CarVan_sub)){hhSize_CarVan_sub} #1,2
  )
  target.list = target.list[!vapply(target.list,is.null,TRUE)]
  target.data = target.data[!vapply(target.data,is.null,TRUE)]

  res <- mipfp::Ipfp(seed_weighted,
                     target.list = target.list,
                     target.data = target.data,
                     iter = iter) #Increased Iterations to get convergence

  dimnames(res$x.hat) = dimnames(seed_weighted)

  res2 = int_trs(res$x.hat * med_pop)

  result_df <- as.data.frame.table(res2)
  names(result_df) = c("hhSize5", "CarVan5", "Tenure5", "AccType5", "hhComp6", "hhComp15","households")

  # Calculate the Mean Absolute Error, checks 2D inputs only
  if(!is.null(Acc_tenure_sub)){
    chk1 = vaidate_syth_pop(result_df,Acc_tenure_sub,"AccType5","Tenure5")
  } else {
    chk1 = 0
  }
  if(!is.null(hhComp_Tenure_sub)){
    chk2 = vaidate_syth_pop(result_df,hhComp_Tenure_sub,"Tenure5","hhComp15")
  } else {
    chk2 = 0
  }
  if(!is.null(Acc_hhComp_sub)){
    chk3 = vaidate_syth_pop(result_df,Acc_hhComp_sub,"AccType5","hhComp15")
  } else {
    chk3 = 0
  }
  if(!is.null(Acc_CarVan_sub)){
    chk4 = vaidate_syth_pop(result_df,Acc_CarVan_sub,"AccType5","CarVan5")
  } else {
    chk4 = 0
  }
  if(!is.null(hhSize_hhComp_sub)){
    chk5 = vaidate_syth_pop(result_df,hhSize_hhComp_sub,"hhComp6","hhSize5")
  } else {
    chk5 = 0
  }
  if(!is.null(Acc_hhSize_sub)){
    chk6 = vaidate_syth_pop(result_df,Acc_hhSize_sub,"hhSize5", "AccType5")
  } else {
    chk6 = 0
  }
  if(!is.null(hhComp6_Tenure_sub)){
    chk7 = vaidate_syth_pop(result_df,hhComp6_Tenure_sub,"hhComp6","Tenure5")
  } else {
    chk7 = 0
  }
  if(!is.null(CarVan_hhComp_sub)){
    chk8 = vaidate_syth_pop(result_df,CarVan_hhComp_sub,"hhComp6","CarVan5")
  } else {
    chk8 = 0
  }
  if(!is.null(Acc_hhComp6_sub)){
    chk9 = vaidate_syth_pop(result_df,Acc_hhComp6_sub,"AccType5","hhComp6")
  } else {
    chk9 = 0
  }
  if(!is.null(Tenure_hhSize_sub)){
    chk10 = vaidate_syth_pop(result_df,Tenure_hhSize_sub,"Tenure5","hhSize5")
  } else {
    chk10 = 0
  }
  if(!is.null(Tenure_CarVan_sub)){
    chk11 = vaidate_syth_pop(result_df,Tenure_CarVan_sub,"Tenure5","CarVan5")
  } else {
    chk11 = 0
  }


  if(FALSE){
    # Debug check, on household composition
    # Small errors are tolerated due to ONS privacy protection
    foo = result_df |>
      dplyr::group_by(hhComp6, hhComp15) |>
      dplyr::summarise(households = sum(households)) |>
      tidyr::pivot_wider(names_from = "hhComp6", values_from = "households")
  }

  #Collapse to Main Variables
  result_df2 = dplyr::group_by(result_df,hhSize5, CarVan5, Tenure5, AccType5, hhComp15) |>
    dplyr::summarise(households = sum(households),
                     error_margins = max(res$error.margins),
                     conv = res$conv
    )

  result_df2 = result_df2[result_df2$households > 0,]
  result_df2$MAE = max(chk1,chk2,chk3,chk4,chk5,chk6,chk7,chk8,chk9,chk10,chk11)
  result_df2$LSOA21CD = lsoacd
  result_df2

}


cenus_syth_combine_alt = function(Acc_tenure_sub,
                                  hhComp_Tenure_sub, Tenure_hhSize_sub,Tenure_CarVan_sub,#,Tenure_NSSEC_sub
                                  seed_alt
) {

  # Check LSOA match
  if(length(unique(c(Acc_tenure_sub$LSOA21CD,
                     hhComp_Tenure_sub$LSOA21CD,
                     Tenure_hhSize_sub$LSOA21CD,
                     Tenure_CarVan_sub$LSOA21CD#,Tenure_NSSEC_sub$LSOA21CD
  ))) != 1){
    stop("More than one LSOA")
  }

  AccByTenure = as.matrix(Acc_tenure_sub[,3:ncol(Acc_tenure_sub)])
  hhCompByTenure = as.matrix(hhComp_Tenure_sub[,3:ncol(hhComp_Tenure_sub)])
  TenureByhhSize = as.matrix(Tenure_hhSize_sub[,3:ncol(Tenure_hhSize_sub)])
  TenureByCarVan = as.matrix(Tenure_CarVan_sub[,3:ncol(Tenure_CarVan_sub)])
  #NSSECByTenure = as.matrix(Tenure_NSSEC_sub[,3:ncol(Tenure_NSSEC_sub)])

  rownames(AccByTenure) = Acc_tenure_sub$AccType5
  rownames(hhCompByTenure) = hhComp_Tenure_sub$hhComp15
  rownames(TenureByhhSize) = Tenure_hhSize_sub$hhSize5
  rownames(TenureByCarVan) = Tenure_CarVan_sub$CarVan5
  #rownames(NSSECByTenure) = Tenure_NSSEC_sub$NSSEC10

  # Harmonise by household size
  # The number of 1 person and more than one person households should match
  hhCompByTenure = hhCompByTenure[c("OnePersonOther","OnePersonOver66", # Only 1
                                    "CoupleNoChildren", # Only 2
                                    "CoupleChildren","CoupleNonDepChildren","FamilyOver66","LoneParent", # At least 2
                                    "LoneParentNonDepChildren","OtherChildren","OtherIncStudentOrOver66","OtherNoChildren"),]


  TenureByhhSize <- lapply(list(1,2:4), function(rows) { TenureByhhSize[rows, , drop = FALSE]})
  hhCompByTenure <- lapply(list(1:2,3:11), function(rows) { hhCompByTenure[rows, , drop = FALSE]})

  hhCompByTenure[[1]] = match_matrix_csums(TenureByhhSize[[1]], hhCompByTenure[[1]])
  hhCompByTenure[[2]] = match_matrix_csums(TenureByhhSize[[2]], hhCompByTenure[[2]])

  hhCompByTenure = rbind(hhCompByTenure[[1]], hhCompByTenure[[2]])
  TenureByhhSize = rbind(TenureByhhSize[[1]], TenureByhhSize[[2]])

  AccByTenure = match_matrix_csums(TenureByhhSize, AccByTenure)
  #NSSECByTenure = match_matrix_csums(TenureByhhSize, NSSECByTenure)
  TenureByCarVan = match_matrix_csums(TenureByhhSize, TenureByCarVan)

  #seed = array(seed_alt$seed, dim = c(5,4,11,16))



  # Humanleauge 3 tables
  # dim = c(#nrow(AccByTenure),
  #   nrow(hhCompByTenure),ncol(hhCompByTenure),nrow(TenureByhhSize),nrow(TenureByCarVan), nrow(NSSECByTenure))
  # seed = array(rep(1,prod(dim)), dim = dim)

  # Set impossible combinations to zero
  # Order by size

  # For hhSizeCarVanByTenure rows 1,2,3,4 (households with one person)
  # and hhCompByTenure rows 1,2 (households with one person)

  # For hhSizeCarVanByTenure rows 5,6,7,8 (households with two person)
  # and hhCompByTenure rows 3 (couple no children)

  # For hhSizeCarVanByTenure rows 5-16 (households with more than one person)
  # and hhCompByTenure rows 4-11 (housholds with 2-many people)

  #for (i in 1:dim[1]) {
  # for (j in 1:dim[1]) {
  #   for (k in 1:dim[2]) {
  #     for (l in 1:dim[3]) {
  #       for (m in 1:dim[4]) {
  #         for(n in 1:dim[5]){
  #           if ((j %in% c(1:2)) & (!l == 1)) {
  #             # If one person household and number of household is not 1 set to 0
  #             seed[j, k, l, m, n] = 1e-15
  #           }
  #
  #           if ((j == 3) & (l != 2)) {
  #             # If two person household and number of household is not 2 set to 0
  #             seed[j, k, l, m, n] = 1e-15
  #           }
  #
  #           if ((j %in% 4:5) & (l %in% c(1:2))) {
  #             # If at least three person household and number of household is 1 or 2 set to 0
  #             seed[j, k, l, m, n] = 1e-15
  #           }
  #
  #           if ((j %in% 6:11) & (l == 1)) {
  #             # If at least two person household and number of household is 1 set to 0
  #             seed[j, k, l, m, n] = 1e-15
  #           }
  #         }
  #       }
  #     }
  #   }
  # }
  #}



  result = try(humanleague::qisi(seed_alt,
                                 indices = list(c(1,2),c(3,2),c(4,2), c(5,2)), #list(c(1,2), c(3,2),c(4,2),c(5,2), c(6,2)),
                                 marginals = list(AccByTenure,
                                                  hhCompByTenure,
                                                  TenureByhhSize,
                                                  TenureByCarVan#, NSSECByTenure
                                 )),
               silent = TRUE)

  if(inherits(result,"try-error")){
    message("QISI failed for ",hhComp_Tenure_sub$LSOA21CD[1])
    return(NULL)
  }

  result_df = expand.grid(rownames(AccByTenure), colnames(AccByTenure),
                          rownames(hhCompByTenure), #colnames(hhCompByTenure),
                          rownames(TenureByhhSize), rownames(TenureByCarVan)#, rownames(NSSECByTenure)
  )
  names(result_df) = c("Acc5","Tenure5",
                       "hhComp15","hhSize5","CarVan5"#,"NSSEC10"
  )
  result_df$households = as.numeric(result$result)
  result_df = result_df[result_df$households > 0,]
  result_df$conv = result$conv
  result_df$pValue = result$pValue


  # Integrity checks
  if(sum(result_df$households[result_df$Tenure5 == "Mortgage" & result_df$Acc5 == "Flat"]) !=
     AccByTenure["Flat","Mortgage"]){
    warning("check 1 failed for:",Acc_tenure_sub$LSOA21CD[1])
  }
  if(sum(result_df$households[result_df$Tenure5 == "Outright" & result_df$hhComp15 == "OnePersonOver66"]) !=
     hhCompByTenure["OnePersonOver66","Outright"]){
    warning("check 2 failed for:",hhComp_Tenure_sub$LSOA21CD[1])
  }
  if(sum(result_df$households[result_df$Tenure5 == "Private_rented" & result_df$hhSize5 == "p2"]) !=
     TenureByhhSize["p2","Private_rented"]){
    warning("check 3 failed for:" ,hhComp_Tenure_sub$LSOA21CD[1])
  }
  if(sum(result_df$households[result_df$Tenure5 == "Social_rented" & result_df$CarVan5 == "car1"]) !=
     TenureByCarVan["car1","Private_rented"]){
    warning("check 3 failed for:" ,hhComp_Tenure_sub$LSOA21CD[1])
  }


  result_df$LSOA = hhComp_Tenure_sub$LSOA21CD[1]

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


simplify_Tenure5 = function(x){
  x[x=="Owned: Owns outright"] = "outright"
  x[x=="Owned: Owns with a mortgage or loan or shared ownership"] = "mortgage"
  x[x=="Rented: Social rented"] = "socialrented"
  x[x=="Private rented or lives rent free"] = "privaterented"
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
  x[x=="Whole house or bungalow: Detached"] = "detached"
  x[x=="Whole house or bungalow: Semi-detached"] = "semidetached"
  x[x=="Whole house or bungalow: Terraced"] = "terraced"
  x[x=="Flat, maisonette or apartment"] = "flat"
  x[x=="A caravan or other mobile or temporary structure"] = "caravan"
  x
}
