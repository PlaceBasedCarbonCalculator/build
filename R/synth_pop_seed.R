read_hhComp15_Tenure5_Size5_CarVan5_EW = function(path = file.path(path_data,"census2021EW_Households_HouseholdComposition15_Tenure5_Size5_CarVar_5_EW.csv")){

  raw = readr::read_csv(path, show_col_types = FALSE)

  names(raw) = c("England and Wales Code","England and Wales",
                 "Household size (5 categories) Code","hhSize5",
                 "Car or van availability (5 categories) Code", "CarVan5",
                 "Tenure of household (5 categories) Code","Tenure5",
                 "Household composition (15 categories) Code","hhComp15",
                 "households")
  raw = raw[,c("hhSize5","CarVan5","Tenure5","hhComp15","households")]

  raw = raw[raw$Tenure5 != "Does not apply",]
  raw = raw[raw$CarVan5 != "Does not apply",]
  raw = raw[raw$hhSize5 != "0 people in household",]
  raw = raw[raw$hhComp15 != "Does not apply",]

  raw$hhSize5 = simplify_hhSize5(raw$hhSize5)
  raw$CarVan5 = simplify_CarVan5(raw$CarVan5)
  raw$Tenure5 = simplify_Tenure5(raw$Tenure5)
  raw$hhComp15 = simplify_household15(raw$hhComp15)


  raw$hhComp15 = gsub("Cohabit|Married","Couple",raw$hhComp15)
  raw = dplyr::group_by(raw, hhSize5,CarVan5,Tenure5,hhComp15)
  raw = dplyr::summarise(raw, households = sum(households))
  raw = dplyr::ungroup(raw)


  raw

}

read_hhComp15_Tenure5_Size5_Acc5_EW = function(path = file.path(path_data,"census2021EW_Households_HouseholdComposition15_Tenure5_Size5_Acc5_EW.csv")){

  raw = readr::read_csv(path, show_col_types = FALSE)

  names(raw) = c("England and Wales Code","England and Wales",
                 "Household size (5 categories) Code","hhSize5",
                 "Tenure of household (5 categories) Code","Tenure5",
                 "Household composition (15 categories) Code","hhComp15",
                 "Accommodation type (5 categories) Code","AccType5",
                 "households")
  raw = raw[,c("hhSize5","AccType5","Tenure5","hhComp15","households")]

  raw = raw[raw$Tenure5 != "Does not apply",]
  raw = raw[raw$AccType5 != "Does not apply",]
  raw = raw[raw$hhSize5 != "0 people in household",]
  raw = raw[raw$hhComp15 != "Does not apply",]

  raw$hhSize5 = simplify_hhSize5(raw$hhSize5)
  raw$Tenure5 = simplify_Tenure5(raw$Tenure5)
  raw$AccType5 = simplify_AccType5(raw$AccType5)
  raw$hhComp15 = simplify_household15(raw$hhComp15)

  raw$hhComp15 = gsub("Cohabit|Married","Couple",raw$hhComp15)
  raw = dplyr::group_by(raw, hhSize5,Tenure5,AccType5,hhComp15)
  raw = dplyr::summarise(raw, households = sum(households))
  raw = dplyr::ungroup(raw)


  raw

}

read_hhComp6_Tenure5_Size5_Acc5_EW = function(path = file.path(path_data,"census2021EW_Households_HouseholdComposition6_Tenure5_Size5_Acc5_EW.csv")){

  raw = readr::read_csv(path, show_col_types = FALSE)

  names(raw) = c("England and Wales Code","England and Wales",
                 "Household composition (6 categories) Code","hhComp6",
                 "Accommodation type (5 categories) Code","AccType5",
                 "Tenure of household (5 categories) Code","Tenure5",
                 "Household size (5 categories) Code","hhSize5",
                 "households")
  raw = raw[,c("hhSize5","AccType5","Tenure5","hhComp6","households")]

  raw = raw[raw$Tenure5 != "Does not apply",]
  raw = raw[raw$AccType5 != "Does not apply",]
  raw = raw[raw$hhSize5 != "0 people in household",]
  raw = raw[raw$hhComp6 != "Does not apply",]

  raw$hhSize5 = simplify_hhSize5(raw$hhSize5)
  raw$AccType5 = simplify_AccType5(raw$AccType5)
  raw$Tenure5 = simplify_Tenure5(raw$Tenure5)
  raw$hhComp6 = simplify_household6(raw$hhComp6)

  raw

}

read_hhComp6_Tenure5_Size5_CarVan5_EW = function(path = file.path(path_data,"census2021EW_Households_HouseholdComposition6_Tenure5_Size5_CarVan5_EW.csv")){

  raw = readr::read_csv(path, show_col_types = FALSE)

  names(raw) = c("England and Wales Code","England and Wales",
                 "Tenure of household (5 categories) Code","Tenure5",
                 "Household size (5 categories) Code","hhSize5",
                 "Car or van availability (5 categories) Code","CarVan5",
                 "Household composition (6 categories) Code","hhComp6",
                 "households")
  raw = raw[,c("hhSize5","CarVan5","Tenure5","hhComp6","households")]

  raw = raw[raw$Tenure5 != "Does not apply",]
  raw = raw[raw$CarVan5 != "Does not apply",]
  raw = raw[raw$hhSize5 != "0 people in household",]
  raw = raw[raw$hhComp6 != "Does not apply",]

  raw$hhSize5 = simplify_hhSize5(raw$hhSize5)
  raw$CarVan5 = simplify_CarVan5(raw$CarVan5)
  raw$Tenure5 = simplify_Tenure5(raw$Tenure5)
  raw$hhComp6 = simplify_household6(raw$hhComp6)

  raw

}

read_CarVan5_Tenure5_Size5_Acc5_EW = function(path = file.path(path_data,"census2021EW_Households_CarVan5_Tenure5_Size5_Acc5_EW.csv")){

  raw = readr::read_csv(path, show_col_types = FALSE)

  names(raw) = c("England and Wales Code","England and Wales",
                 "Accommodation type (5 categories) Code","AccType5",
                 "Tenure of household (5 categories) Code","Tenure5",
                 "Household size (5 categories) Code","hhSize5",
                 "Car or van availability (5 categories) Code","CarVan5",
                 "households")
  raw = raw[,c("hhSize5","AccType5","Tenure5","CarVan5","households")]

  raw = raw[raw$Tenure5 != "Does not apply",]
  raw = raw[raw$AccType5 != "Does not apply",]
  raw = raw[raw$hhSize5 != "0 people in household",]
  raw = raw[raw$CarVan5 != "Does not apply",]

  raw$hhSize5 = simplify_hhSize5(raw$hhSize5)
  raw$CarVan5 = simplify_CarVan5(raw$CarVan5)
  raw$AccType5 = simplify_AccType5(raw$AccType5)
  raw$Tenure5 = simplify_Tenure5(raw$Tenure5)

  raw

}

build_synth_pop_seed = function(path_data = file.path(parameters$path_data,"population")){

  sizeCarTenureComp15 = read_hhComp15_Tenure5_Size5_CarVan5_EW(file.path(path_data,"census2021EW_Households_HouseholdComposition15_Tenure5_Size5_CarVar_5_EW.csv"))
  AccsizeTenureComp15 = read_hhComp15_Tenure5_Size5_Acc5_EW(file.path(path_data,"census2021EW_Households_HouseholdComposition15_Tenure5_Size5_Acc5_EW.csv"))

  sizeCarTenureComp6 = read_hhComp6_Tenure5_Size5_CarVan5_EW(file.path(path_data,"census2021EW_Households_HouseholdComposition6_Tenure5_Size5_CarVan5_EW.csv"))
  AccsizeTenureComp6 = read_hhComp6_Tenure5_Size5_Acc5_EW(file.path(path_data,"census2021EW_Households_HouseholdComposition6_Tenure5_Size5_Acc5_EW.csv"))

  sizeCarTenureAcc = read_CarVan5_Tenure5_Size5_Acc5_EW(file.path(path_data,"census2021EW_Households_CarVan5_Tenure5_Size5_Acc5_EW.csv"))

  hhComp15 = c("OnePersonOver66","OnePersonOther","FamilyOver66","CoupleNoChildren","CoupleChildren","CoupleNonDepChildren","LoneParent","LoneParentNonDepChildren","OtherChildren","OtherNoChildren","OtherIncStudentOrOver66")
  hhComp6 = c("OnePerson","FamilyOver66","CoupleFamily","LoneParent","Other6")
  hhSize5 = c("p1","p2","p3","p4+")
  Tenure5 = c("outright","mortgage","socialrented","privaterented")
  CarVan5 = c("car0","car1","car2","car3+")
  AccType5 = c("detached","semidetached","terraced","flat","caravan")

  # Make Arrays
  MsizeCarTenureComp15 = array_maker(sizeCarTenureComp15, hhSize5, CarVan5, Tenure5,  hhComp15)
  MAccsizeTenureComp15 = array_maker(AccsizeTenureComp15, hhSize5, Tenure5,  AccType5, hhComp15)
  MsizeCarTenureComp6 = array_maker(sizeCarTenureComp6, hhSize5, CarVan5, Tenure5,  hhComp6)
  MAccsizeTenureComp6 = array_maker(AccsizeTenureComp6, hhSize5, AccType5, Tenure5,      hhComp6)
  MsizeCarTenureAcc = array_maker(sizeCarTenureAcc, hhSize5, AccType5, Tenure5,  CarVan5)

  # Make Seed
  # hhSize5, CarVan5, Tenure5, AccType5, hhComp6, hhComp15

  dim = c(length(hhSize5), length(CarVan5), length(Tenure5), length(AccType5), length(hhComp6), length(hhComp15))
  seed = array(rep(1,prod(dim)), dim = dim)

  med_pop = median(c(sum(MsizeCarTenureComp15),
                     sum(MAccsizeTenureComp15),
                     sum(MsizeCarTenureComp6),
                     sum(MAccsizeTenureComp6),
                     sum(MsizeCarTenureAcc)
  ))
  dimnames(seed) = list(hhSize5, CarVan5, Tenure5, AccType5, hhComp6, hhComp15)

  # Force the hhComp relationships
  rules <- list(
    OnePerson = c("OnePersonOver66", "OnePersonOther"),
    FamilyOver66 = c("FamilyOver66"),
    CoupleFamily = c("CoupleNoChildren", "CoupleChildren", "CoupleNonDepChildren"),
    LoneParent = c("LoneParent", "LoneParentNonDepChildren"),
    Other6 = c("OtherChildren", "OtherNoChildren", "OtherIncStudentOrOver66")
  )

  # Apply rules elegantly
  for (hh6 in names(rules)) {
    hh6_idx <- which(hhComp6 == hh6)
    valid_hh15 <- rules[[hh6]]
    invalid_hh15_idx <- which(!(hhComp15 %in% valid_hh15))

    # Zero out seed for invalid hhComp15 combinations
    seed[, , , , hh6_idx, invalid_hh15_idx] <- 0
  }

  seed = (seed /sum(seed)) * med_pop

  res <- mipfp::Ipfp(seed,
                     list(c(1,2,3,6),c(1,3,4,6),c(1,2,3,5),c(1,4,3,5),c(1,4,3,2)
                     ),
                     list(
                       MsizeCarTenureComp15, #1,2,3,6
                       MAccsizeTenureComp15, #1,3,4,6
                       MsizeCarTenureComp6, #1,2,3,5
                       MAccsizeTenureComp6, #1,4,3,5
                       MsizeCarTenureAcc #1,4,3,2
                     ))

  dimnames(res$x.hat) = dimnames(seed)

  res2 = int_trs(res$x.hat * med_pop)

  result_df <- as.data.frame.table(res2)
  names(result_df) = c("hhSize5", "CarVan5", "Tenure5", "AccType5", "hhComp6", "hhComp15","households")

  if(FALSE){
    # Check hhcomp matrix
    chk_hhcomp = result_df |>
      dplyr::group_by(hhComp6, hhComp15) |>
      dplyr::summarise(households = sum(households)) |>
      tidyr::pivot_wider(names_from = "hhComp6", values_from = "households")

    chk_hhSize5 = result_df |>
      dplyr::group_by(hhSize5, hhComp15) |>
      dplyr::summarise(households = sum(households)) |>
      tidyr::pivot_wider(names_from = "hhSize5", values_from = "households")

    chk_Tenure5 = result_df |>
      dplyr::group_by(Tenure5, AccType5) |>
      dplyr::summarise(households = sum(households)) |>
      tidyr::pivot_wider(names_from = "Tenure5", values_from = "households")

    chk_Tenure5b = AccsizeTenureComp15 |>
      dplyr::group_by(Tenure5, AccType5) |>
      dplyr::summarise(households = sum(households)) |>
      tidyr::pivot_wider(names_from = "Tenure5", values_from = "households")

    chk_Tenure5b = chk_Tenure5b[match(chk_Tenure5$AccType5,chk_Tenure5b$AccType5 ),names(chk_Tenure5)]
    chk_Tenure_diff = as.matrix(chk_Tenure5b[,2:5]) - as.matrix(chk_Tenure5[,2:5])

    chk_hhComp15 = result_df |>
      dplyr::group_by(hhComp15, CarVan5) |>
      dplyr::summarise(households = sum(households)) |>
      tidyr::pivot_wider(names_from = "CarVan5", values_from = "households")

    chk_hhComp15b = sizeCarTenureComp15 |>
      dplyr::group_by(hhComp15, CarVan5) |>
      dplyr::summarise(households = sum(households)) |>
      tidyr::pivot_wider(names_from = "CarVan5", values_from = "households")

    chk_hhComp15b = chk_hhComp15b[match(chk_hhComp15$hhComp15,chk_hhComp15b$hhComp15 ),names(chk_hhComp15)]
    chk_hhComp15b_diff = as.matrix(chk_hhComp15b[,2:5]) - as.matrix(chk_hhComp15[,2:5])

  }

  result_df$seed = result_df$households / 35000
  result_df$seed = ifelse(result_df$seed > 1, 1,  result_df$seed)
  result_df$seed = ifelse(result_df$seed < 1 &    result_df$seed > 0.03, 0.75 , result_df$seed)
  result_df$seed = ifelse(result_df$seed < 0.03 & result_df$seed > 0.01, 0.5  , result_df$seed)
  result_df$seed = ifelse(result_df$seed < 0.01 & result_df$seed > 0   , 0.001, result_df$seed)

  result_df$seed[result_df$seed == 0] = 1e-15

  result_df


}




