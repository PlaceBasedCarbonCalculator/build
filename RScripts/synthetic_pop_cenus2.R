read_Acc_hhComp = function(path = "../inputdata/population/census2021EW_Households_AccomodationType5_HouseholdComposition6_LSOA_partial.csv"){

  raw = readr::read_csv(path, show_col_types = FALSE)

  names(raw) = c("LSOA21CD","LSOA21NM","hhComp15CD","hhComp15","AccType5CD","AccType5","households")
  raw = raw[,c("LSOA21CD","AccType5","hhComp15","households")]

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

read_Tenure_CarVan = function(path = "../inputdata/population/census2021EW_Households_Tenure5_CarVan5_LSOA.csv"){

  raw = readr::read_csv(path, show_col_types = FALSE)

  names(raw) = c("LSOA21CD","LSOA21NM","CarVan5CD","CarVan5","Tenure5CD","Tenure5","households")
  raw = raw[,c("LSOA21CD","Tenure5","CarVan5","households")]

  raw

}

read_Tenure_hhSize_CarVan = function(path = "../inputdata/population/census2021EW_Households_Tenure5_HouseholdSize5_CarVan5_LSOA_partial.csv"){

  raw = readr::read_csv(path, show_col_types = FALSE)

  names(raw) = c("LSOA21CD","LSOA21NM","CarVan5CD","CarVan5","Tenure5CD","Tenure5","hhSize5CD","hhSize5","households")
  raw = raw[,c("LSOA21CD","Tenure5","hhSize5","CarVan5","households")]

  raw

}

read_Tenure_NSSEC = function(path = "../inputdata/population/census2021EW_RefPerson_NSSEC10_Tenure5_LSOA.csv"){

  raw = readr::read_csv(path, show_col_types = FALSE)

  names(raw) = c("LSOA21CD","LSOA21NM","Tenure5CD","Tenure5","NSSEC10CD","NSSEC10","households")
  raw = raw[,c("LSOA21CD","Tenure5","NSSEC10","households")]

  raw

}

read_hhComp_NSSEC = function(path = "../inputdata/population/census2021EW_RefPerson_NSSEC10_Houshold15_LSOA_partial.csv"){

  raw = readr::read_csv(path, show_col_types = FALSE)

  names(raw) = c("LSOA21CD","LSOA21NM","hhComp15CD","hhComp15","NSSEC10CD","NSSEC10","households")
  raw = raw[,c("LSOA21CD","hhComp15","NSSEC10","households")]

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

sythetic_census = function(path_data = file.path(parameters$path_data,"population"), synth_pop_seed){

  #Acc_hhComp = read_Acc_hhComp(file.path(path_data,"census2021EW_Households_HouseholdComposition15_AccomodationType5_LSOA_partial.csv"))  # Partial
  #Acc_CarVan = read_Acc_CarVan(file.path(path_data,"census2021EW_Households_AccomodationType5_CarVan5_LSOA_partial.csv"))  # Partial
  #hhSize_hhComp = read_hhSize_hhComp(file.path(path_data,"census2021EW_Households_HouseholdComposition6_HouseholdSize5_LSOA_partial.csv"))  # Partial
  Acc_tenure = read_Acc_tenure(file.path(path_data,"census2021EW_Households_AccomodationType5_Tenure5_LSOA.csv"))
  hhComp_Tenure = read_hhComp_Tenure(file.path(path_data,"census2021EW_Households_HouseholdComposition15_Tenure5_LSOA.csv"))
  Tenure_hhSize = read_Tenure_hhSize(file.path(path_data,"census2021EW_Households_Tenure5_HouseholdSize5_LSOA.csv"))
  #CarVan_hhComp = read_CarVan_hhComp(file.path(path_data,"census2021EW_Households_HouseholdComposition6_CarVan5_LSOA.csv"))
  Tenure_CarVan = read_Tenure_CarVan(file.path(path_data,"census2021EW_Households_Tenure5_CarVan5_LSOA.csv"))
  Tenure_hhSize_CarVan = read_Tenure_hhSize_CarVan(file.path(path_data,"census2021EW_Households_Tenure5_HouseholdSize5_CarVan5_LSOA_partial.csv")) # Partial
  #Tenure_NSSEC = read_Tenure_NSSEC(file.path(path_data,"census2021EW_RefPerson_NSSEC10_Tenure5_LSOA.csv"))
  #hhComp_NSSEC = read_hhComp_NSSEC(file.path(path_data,"census2021EW_RefPerson_NSSEC10_Houshold15_LSOA_partial.csv"))

  # Remove DNA if all zeros
  Acc_tenure = Acc_tenure[Acc_tenure$Tenure5 != "Does not apply",]
  #Tenure_NSSEC = Tenure_NSSEC[Tenure_NSSEC$Tenure5 != "Does not apply",]
  #hhComp_NSSEC = hhComp_NSSEC[hhComp_NSSEC$hhComp15 != "Does not apply",]
  hhComp_Tenure = hhComp_Tenure[hhComp_Tenure$Tenure5 != "Does not apply",]
  hhComp_Tenure = hhComp_Tenure[hhComp_Tenure$hhComp15 != "Does not apply",]
  Tenure_hhSize = Tenure_hhSize[Tenure_hhSize$Tenure5 != "Does not apply",]
  Tenure_hhSize = Tenure_hhSize[Tenure_hhSize$hhSize5 != "0 people in household",]
  Tenure_CarVan = Tenure_CarVan[Tenure_CarVan$CarVan5 != "Does not apply",]
  Tenure_CarVan = Tenure_CarVan[Tenure_CarVan$Tenure5 != "Does not apply",]
  Tenure_hhSize_CarVan = Tenure_hhSize_CarVan[Tenure_hhSize_CarVan$Tenure5 != "Does not apply",]
  Tenure_hhSize_CarVan = Tenure_hhSize_CarVan[Tenure_hhSize_CarVan$hhSize5 != "0 people in household",]
  Tenure_hhSize_CarVan = Tenure_hhSize_CarVan[Tenure_hhSize_CarVan$CarVan5 != "Does not apply",]

  #Acc_CarVan = Acc_CarVan[Acc_CarVan$CarVan5 != "Does not apply",]

  # Simplify Values
  Tenure_hhSize_CarVan$Tenure5 = simplify_Tenure5(Tenure_hhSize_CarVan$Tenure5)
  Tenure_hhSize_CarVan$hhSize5 = simplify_hhSize5(Tenure_hhSize_CarVan$hhSize5)
  Tenure_hhSize_CarVan$CarVan5 = simplify_CarVan5(Tenure_hhSize_CarVan$CarVan5)
  Acc_tenure$Tenure5 = simplify_Tenure5(Acc_tenure$Tenure5)
  Acc_tenure$AccType5 = simplify_AccType5(Acc_tenure$AccType5)
  Tenure_CarVan$Tenure5 = simplify_Tenure5(Tenure_CarVan$Tenure5)
  Tenure_CarVan$CarVan5 = simplify_CarVan5(Tenure_CarVan$CarVan5)
  hhComp_Tenure$Tenure5 = simplify_Tenure5(hhComp_Tenure$Tenure5)
  #hhComp_NSSEC$NSSEC10 =  simplify_nssec(hhComp_NSSEC$NSSEC10)
  #Tenure_NSSEC$Tenure5 = simplify_Tenure5(Tenure_NSSEC$Tenure5)
  #Tenure_NSSEC$NSSEC10 =  simplify_nssec(Tenure_NSSEC$NSSEC10)
  Tenure_hhSize$Tenure5 = simplify_Tenure5(Tenure_hhSize$Tenure5)
  Tenure_hhSize$hhSize5  = simplify_hhSize5(Tenure_hhSize$hhSize5)

  #Acc_CarVan$CarVan5 = simplify_CarVan5(Acc_CarVan$CarVan5)
  #Acc_CarVan$AccType5 = simplify_AccType5(Acc_CarVan$AccType5)

  # Simplify hhComp15 as don't know married etc
  hhComp_Tenure$hhComp15 = simplify_household15(hhComp_Tenure$hhComp15)
  hhComp_Tenure$hhComp15 = gsub("Cohabit|Married","Couple",hhComp_Tenure$hhComp15)
  hhComp_Tenure = dplyr::group_by(hhComp_Tenure, LSOA21CD, Tenure5, hhComp15)
  hhComp_Tenure = dplyr::summarise(hhComp_Tenure, households = sum(households))
  hhComp_Tenure = dplyr::ungroup(hhComp_Tenure)

  #hhComp_NSSEC$hhComp15 = simplify_household15(hhComp_NSSEC$hhComp15)
  #hhComp_NSSEC$hhComp15 = gsub("Cohabit|Married","Couple",hhComp_NSSEC$hhComp15)
  #hhComp_NSSEC = dplyr::group_by(hhComp_NSSEC, LSOA21CD, NSSEC10, hhComp15)
  #hhComp_NSSEC = dplyr::summarise(hhComp_NSSEC, households = sum(households))
  #hhComp_NSSEC = dplyr::ungroup(hhComp_NSSEC)

  # Find common core
  lsoa_common = Reduce(intersect,
                       list(
                         unique(Acc_tenure$LSOA21CD),
                         unique(hhComp_Tenure$LSOA21CD),
                         unique(Tenure_hhSize_CarVan$LSOA21CD)#,
                          #unique(Tenure_NSSEC$LSOA21CD)
                       )) #35148 98.5%

  Acc_tenure_com = Acc_tenure[Acc_tenure$LSOA21CD %in% lsoa_common,]
  hhComp_Tenure_com = hhComp_Tenure[hhComp_Tenure$LSOA21CD %in% lsoa_common,]
  Tenure_hhSize_CarVan_com = Tenure_hhSize_CarVan[Tenure_hhSize_CarVan$LSOA21CD %in% lsoa_common,]
  #Tenure_NSSEC_com = Tenure_NSSEC[Tenure_NSSEC$LSOA21CD %in% lsoa_common,]

  #Acc_CarVan_com = Acc_CarVan[Acc_CarVan$LSOA21CD %in% lsoa_common,]

  # Reduce to 2 variables
  Tenure_hhSize_CarVan_com = Tenure_hhSize_CarVan_com[order(Tenure_hhSize_CarVan_com$hhSize5),]
  Tenure_hhSize_CarVan_com$hhSize5_CarVan5 = paste0(Tenure_hhSize_CarVan_com$hhSize5,"_",Tenure_hhSize_CarVan_com$CarVan5)
  Tenure_hhSize_CarVan_com = Tenure_hhSize_CarVan_com[,c("LSOA21CD","Tenure5","hhSize5_CarVan5","households")]

  # Pivot
  Acc_tenure_com = tidyr::pivot_wider(Acc_tenure_com, names_from = "Tenure5", values_from = "households")
  hhComp_Tenure_com = tidyr::pivot_wider(hhComp_Tenure_com, names_from = "Tenure5", values_from = "households")
  Tenure_hhSize_CarVan_com = tidyr::pivot_wider(Tenure_hhSize_CarVan_com, names_from = "Tenure5", values_from = "households")
  #Tenure_NSSEC_com = tidyr::pivot_wider(Tenure_NSSEC_com, names_from = "Tenure5", values_from = "households")

  #Acc_CarVan_com = tidyr::pivot_wider(Acc_CarVan_com, names_from = "CarVan5", values_from = "households")

  # Match Column orders
  Acc_tenure_com = Acc_tenure_com[,c("LSOA21CD","AccType5","Outright","Mortgage","Social_rented","Private_rented")]
  hhComp_Tenure_com = hhComp_Tenure_com[,c("LSOA21CD","hhComp15","Outright","Mortgage","Social_rented","Private_rented")]
  Tenure_hhSize_CarVan_com = Tenure_hhSize_CarVan_com[,c("LSOA21CD","hhSize5_CarVan5","Outright","Mortgage","Social_rented","Private_rented")]
  #Tenure_NSSEC_com = Tenure_NSSEC_com[,c("LSOA21CD","NSSEC10","Outright","Mortgage","Social_rented","Private_rented")]

  # Order
  Acc_tenure_com    = Acc_tenure_com[order(Acc_tenure_com$LSOA21CD),]
  hhComp_Tenure_com    = hhComp_Tenure_com[order(hhComp_Tenure_com$LSOA21CD),]
  Tenure_hhSize_CarVan_com    = Tenure_hhSize_CarVan_com[order(Tenure_hhSize_CarVan_com$LSOA21CD),]
  #Tenure_NSSEC_com    = Tenure_NSSEC_com[order(Tenure_NSSEC_com$LSOA21CD),]

  #Acc_CarVan_com    = Acc_CarVan_com[order(Acc_CarVan_com$LSOA21CD),]

  # Split
  Acc_tenure_com = dplyr::group_split(dplyr::ungroup(Acc_tenure_com), LSOA21CD)
  hhComp_Tenure_com = dplyr::group_split(hhComp_Tenure_com, LSOA21CD)
  Tenure_hhSize_CarVan_com = dplyr::group_split(Tenure_hhSize_CarVan_com, LSOA21CD)
  #Tenure_NSSEC_com = dplyr::group_split(Tenure_NSSEC_com, LSOA21CD)

  #Acc_CarVan_com = dplyr::group_split(Acc_CarVan_com, LSOA21CD)

  # Build Seed
  seed_df = expand.grid(c("Detached","Semi","Terraced","Flat","caravan"),
    c("OnePersonOther","OnePersonOver66",
      "CoupleNoChildren",
      "CoupleChildren","CoupleNonDepChildren","FamilyOver66","LoneParent",
      "LoneParentNonDepChildren","OtherChildren","OtherIncStudentOrOver66","OtherNoChildren"),
                          c("Outright","Mortgage","Social_rented","Private_rented"),
                          c("p1_car0","p1_car1","p1_car2","p1_car3+","p2_car0","p2_car1","p2_car2","p2_car3+","p3_car0",
                            "p3_car1","p3_car2","p3_car3+","p4+_car0","p4+_car1","p4+_car2","p4+_car3+"))#,
                          #c("DNA","L1L2L3","L4L5L6","L7","L8L9","L10L11","L12","L13","L14","L15"))
  names(seed_df) = c("Acc5",
    "hhComp15","Tenure5","hhSizeCarVan")#,"NSSEC10")

  seed_df[] <- lapply(seed_df[], as.character)

  # Match Seed to Inputs
  synth_pop_seed = synth_pop_seed[,c("Acc5",
    "hhComp15","Tenure5","hhSizeCarVan",#"NSSEC10",
    "households","seed")]
  synth_pop_seed[1:4] <- lapply(synth_pop_seed[1:4], as.character)

  synth_pop_seed$Tenure5 = gsub("Privaterented","Private_rented",synth_pop_seed$Tenure5)
  synth_pop_seed$Tenure5 = gsub("Socialrented","Social_rented",synth_pop_seed$Tenure5)

  # Create a key column by concatenating columns' values
  seed_df$key <- apply(seed_df, 1, paste, collapse = "_")
  synth_pop_seed$key <- apply(synth_pop_seed[,c("Acc5","hhComp15","Tenure5","hhSizeCarVan"#"NSSEC10"
                                                )], 1, paste, collapse = "_")

  # Match rows of A to B using the key column
  synth_pop_seed <- synth_pop_seed[match(seed_df$key, synth_pop_seed$key), ]

  # Remove the key column
  synth_pop_seed$key <- NULL
  seed_df$key <- NULL

  seed = array(synth_pop_seed$seed, dim = c(5,4,11,16))
  #seed = array(synth_pop_seed$seed, dim = c(11,4,16,10))

  # Combine
  future::plan("multisession")
  res_com = furrr::future_pmap(.l = list(Acc_tenure_com,
    hhComp_Tenure_com, Tenure_hhSize_CarVan_com),# Tenure_NSSEC_com),
                        .f = cenus_syth_combine_v3, seed = seed,
    .progress = TRUE, .options = furrr::furrr_options(seed = TRUE))
  future::plan("sequential")
  res_com = dplyr::bind_rows(res_com)

  res_com$hhSize5 = sapply(strsplit(as.character(res_com$hhSizeCarVan),"_"),`[[`,1)
  res_com$CarVan5 = sapply(strsplit(as.character(res_com$hhSizeCarVan),"_"),`[[`,2)
  res_com$hhSizeCarVan = NULL

  if(FALSE){
    #DEBUG
    res_com = purrr::pmap(.l = list(Acc_tenure_com[1:10],
                                           hhComp_Tenure_com[1:10], Tenure_hhSize_CarVan_com[1:10]),# Tenure_NSSEC_com),
                                 .f = cenus_syth_combine_v3, seed = seed,
                                 .progress = TRUE)

    res_comb = cenus_syth_combine_v3(
      Acc_tenure_sub = Acc_tenure_com[[1]],
      hhComp_Tenure_sub = hhComp_Tenure_com[[1]],
      Tenure_hhSize_CarVan_sub = Tenure_hhSize_CarVan_com[[1]],
      seed)
  }

  if(FALSE){
    res_com = dplyr::bind_rows(res_com)

    #Do some cross-validation
    Acc_CarVan = read_Acc_CarVan(file.path(path_data,"census2021EW_Households_AccomodationType5_CarVan5_LSOA_partial.csv"))
    Acc_CarVan = Acc_CarVan[Acc_CarVan$LSOA21CD %in% res_com$LSOA,]
    Acc_CarVan$AccType5 = simplify_AccType5(Acc_CarVan$AccType5)
    Acc_CarVan$CarVan5 = simplify_CarVan5(Acc_CarVan$CarVan5)
    Acc_CarVan = Acc_CarVan[Acc_CarVan$CarVan5 != "Does not apply",]

    res_AccCarVan = res_com[,c("LSOA","Acc5","hhSizeCarVan","households")]
    res_AccCarVan$CarVan = sapply(strsplit(as.character(res_AccCarVan$hhSizeCarVan),"_"),`[[`, 2)
    res_AccCarVan = res_AccCarVan[res_AccCarVan$LSOA %in% Acc_CarVan$LSOA21CD,]
    res_AccCarVan = dplyr::group_by(res_AccCarVan, LSOA, Acc5, CarVan)
    res_AccCarVan = dplyr::summarise(res_AccCarVan, households = sum(households))

    foo = dplyr::full_join(Acc_CarVan, res_AccCarVan, by = c("LSOA21CD" ="LSOA", "AccType5" = "Acc5", "CarVan5" = "CarVan"))
    foo$households.x[is.na(foo$households.x)] = 0
    foo$households.y[is.na(foo$households.y)] = 0

    ggplot(foo, aes(x = households.x, y = households.y, colour = AccType5, shape = CarVan5)) +
    geom_point() +
      xlab("Number of Households in Census 2021") +
         ylab("Number Households in Synthetic Population") +
      ggtitle("Accommodation Type and Car/Van Ownership") +
      geom_abline(, color = "black") +
      labs(color='Accommodation Type') +
      labs(shape='Car/Van Ownership')
      theme(legend.position = "none")

    cor(foo$households.x, foo$households.y) #0.9874037 very good
    summary(lm(foo$households.x ~ foo$households.y))
  }



  #Alt mesaure for unusual LSOA
  lsoa_alt = unique(hhComp_Tenure$LSOA21CD)
  lsoa_alt = lsoa_alt[!lsoa_alt %in% lsoa_common]

  Acc_tenure_alt = Acc_tenure[Acc_tenure$LSOA21CD %in% lsoa_alt,]
  hhComp_Tenure_alt = hhComp_Tenure[hhComp_Tenure$LSOA21CD %in% lsoa_alt,]
  Tenure_hhSize_alt = Tenure_hhSize[Tenure_hhSize$LSOA21CD %in% lsoa_alt,]
  Tenure_CarVan_alt = Tenure_CarVan[Tenure_CarVan$LSOA21CD %in% lsoa_alt,]
  #Tenure_NSSEC_alt = Tenure_NSSEC[Tenure_NSSEC$LSOA21CD %in% lsoa_alt,]

  Acc_tenure_alt = tidyr::pivot_wider(Acc_tenure_alt, names_from = "Tenure5", values_from = "households")
  hhComp_Tenure_alt = tidyr::pivot_wider(hhComp_Tenure_alt, names_from = "Tenure5", values_from = "households")
  Tenure_hhSize_alt = tidyr::pivot_wider(Tenure_hhSize_alt, names_from = "Tenure5", values_from = "households")
  Tenure_CarVan_alt = tidyr::pivot_wider(Tenure_CarVan_alt, names_from = "Tenure5", values_from = "households")
  #Tenure_NSSEC_alt = tidyr::pivot_wider(Tenure_NSSEC_alt, names_from = "Tenure5", values_from = "households")

  Acc_tenure_alt = Acc_tenure_alt[,c("LSOA21CD","AccType5","Outright","Mortgage","Social_rented","Private_rented")]
  hhComp_Tenure_alt = hhComp_Tenure_alt[,c("LSOA21CD","hhComp15","Outright","Mortgage","Social_rented","Private_rented")]
  Tenure_hhSize_alt = Tenure_hhSize_alt[,c("LSOA21CD","hhSize5","Outright","Mortgage","Social_rented","Private_rented")]
  Tenure_CarVan_alt = Tenure_CarVan_alt[,c("LSOA21CD","CarVan5","Outright","Mortgage","Social_rented","Private_rented")]
  #Tenure_NSSEC_alt = Tenure_NSSEC_alt[,c("LSOA21CD","NSSEC10","Outright","Mortgage","Social_rented","Private_rented")]

  Acc_tenure_alt    = Acc_tenure_alt[order(Acc_tenure_alt$LSOA21CD),]
  hhComp_Tenure_alt = hhComp_Tenure_alt[order(hhComp_Tenure_alt$LSOA21CD),]
  Tenure_hhSize_alt = Tenure_hhSize_alt[order(Tenure_hhSize_alt$LSOA21CD),]
  Tenure_CarVan_alt = Tenure_CarVan_alt[order(Tenure_CarVan_alt$LSOA21CD),]
  #Tenure_NSSEC_alt = Tenure_NSSEC_alt[order(Tenure_NSSEC_alt$LSOA21CD),]

  Acc_tenure_alt = dplyr::group_split(Acc_tenure_alt, LSOA21CD)
  hhComp_Tenure_alt = dplyr::group_split(hhComp_Tenure_alt, LSOA21CD)
  Tenure_hhSize_alt = dplyr::group_split(Tenure_hhSize_alt, LSOA21CD)
  Tenure_CarVan_alt = dplyr::group_split(Tenure_CarVan_alt, LSOA21CD)
  #Tenure_NSSEC_alt = dplyr::group_split(Tenure_NSSEC_alt, LSOA21CD)

  seed_alt = synth_pop_seed
  hhSizeCarVansplt = strsplit(seed_alt$hhSizeCarVan,"_")
  seed_alt$hhSize = sapply(hhSizeCarVansplt,`[[`,1)
  seed_alt$CarVan = sapply(hhSizeCarVansplt,`[[`,2)

  seed_alt = array(seed_alt$seed, dim = c(5,4,11,4,4))

  # Combine
  future::plan("multisession")
  res_alt = furrr::future_pmap(.l = list(Acc_tenure_alt,
    hhComp_Tenure_alt, Tenure_hhSize_alt,Tenure_CarVan_alt),#Tenure_NSSEC_alt),
  .f = cenus_syth_combine_alt,
  seed_alt = seed_alt,
  .progress = TRUE,  .options = furrr::furrr_options(seed = TRUE))
  future::plan("sequential")
  res_alt = dplyr::bind_rows(res_alt)

  if(FALSE){
    # DEBUG
    res_alt = purrr::pmap(.l = list(Acc_tenure_alt[1:3],
                                           hhComp_Tenure_alt[1:3], Tenure_hhSize_alt[1:3],Tenure_CarVan_alt[1:3]),#Tenure_NSSEC_alt),
                                 .f = cenus_syth_combine_alt, seed_alt = seed_alt, .progress = TRUE)

    # Acc_tenure_sub = Acc_tenure_com[[1]]
    # hhComp_Tenure_sub = hhComp_Tenure_com[[1]]
    # Tenure_hhSize_CarVan_sub = Tenure_hhSize_CarVan_com[[1]]
    # Tenure_NSSEC_sub = Tenure_NSSEC_com[[1]]
    #Acc_CarVan_sub = Acc_CarVan_com[[1]]

    # # Acc_tenure_sub = Acc_tenure_com[Acc_tenure_com$LSOA21CD == lsoaid,]
    # # Acc_CarVan_sub = Acc_CarVan_com[Acc_CarVan_com$LSOA21CD == lsoaid,]
    # # Acc_hhComp_sub = Acc_hhComp_com[Acc_hhComp_com$LSOA21CD == lsoaid,]
    # # hhSize_hhComp_sub = hhSize_hhComp_com[hhSize_hhComp_com$LSOA21CD == lsoaid,]
    #
    # hhComp_Tenure_sub = hhComp_Tenure_alt[[1]]
    # Tenure_hhSize_sub = Tenure_hhSize_alt[[1]]
    # Tenure_CarVan_sub = Tenure_CarVan_alt[[1]]
    # Acc_tenure_sub = Acc_tenure_alt[[1]]
    #Tenure_NSSEC_sub = Tenure_NSSEC_alt[[1]]

  }

  res_final = rbind(res_com, res_alt)

  res_final
}

simplify_Tenure5 = function(x){
  x[x=="Owned: Owns outright"] = "outright"
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
  x[x=="Whole house or bungalow: Detached"] = "detached"
  x[x=="Whole house or bungalow: Semi-detached"] = "semidetached"
  x[x=="Whole house or bungalow: Terraced"] = "terraced"
  x[x=="Flat, maisonette or apartment"] = "flat"
  x[x=="A caravan or other mobile or temporary structure"] = "caravan"
  x
}

cenus_syth_combine_v3 = function(Acc_tenure_sub,
  hhComp_Tenure_sub, Tenure_hhSize_CarVan_sub, #Tenure_NSSEC_sub,
  seed) {

  # Check LSOA match
  if(length(unique(c(Acc_tenure_sub$LSOA21CD,
                     hhComp_Tenure_sub$LSOA21CD,
                     Tenure_hhSize_CarVan_sub$LSOA21CD#,
                     #Tenure_NSSEC_sub$LSOA21CD
                     ))) != 1){
    stop("More than one LSOA")
  }

  AccByTenure = as.matrix(Acc_tenure_sub[,3:ncol(Acc_tenure_sub)])
  hhCompByTenure = as.matrix(hhComp_Tenure_sub[,3:ncol(hhComp_Tenure_sub)])
  #TenureByhhSize = as.matrix(Tenure_hhSize_sub[,3:ncol(Tenure_hhSize_sub)])
  #CarVanByhhComp = as.matrix(CarVan_hhComp_sub[,3:ncol(CarVan_hhComp_sub)])
  hhSizeCarVanByTenure = as.matrix(Tenure_hhSize_CarVan_sub[,3:ncol(Tenure_hhSize_CarVan_sub)])
  #NSSECByTenure = as.matrix(Tenure_NSSEC_sub[,3:ncol(Tenure_NSSEC_sub)])

  #AccByCarVan = as.matrix(Acc_CarVan_sub[,3:ncol(Acc_CarVan_sub)])

  #nms_car = c("car0","car1","car2","car3+")
  #nms_tenure = c("Outright","Mortgage","Social_rented","Private_rented")
  #nms_acc = c("Detached","Semi","Terrace","Flat", "Caravan")
  #nms_comp = c("Oneperson", "family66","Couple","Loneparent","Other")
  #nms_size = c("p1", "p2","p3","p4+")

  rownames(AccByTenure) = Acc_tenure_sub$AccType5
  rownames(hhCompByTenure) = hhComp_Tenure_sub$hhComp15
  rownames(hhSizeCarVanByTenure) = Tenure_hhSize_CarVan_sub$hhSize5_CarVan5
  #rownames(NSSECByTenure) = Tenure_NSSEC_sub$NSSEC10

  #rownames(AccByCarVan) = Acc_CarVan_sub$AccType5

  # Harmonise by household size
  # The number of 1 person and more than one person households should match
  hhCompByTenure = hhCompByTenure[c("OnePersonOther","OnePersonOver66", # Only 1
                                    "CoupleNoChildren", # Only 2
                                    "CoupleChildren","CoupleNonDepChildren","FamilyOver66","LoneParent", # At least 2
                                    "LoneParentNonDepChildren","OtherChildren","OtherIncStudentOrOver66","OtherNoChildren"),]

  AccByTenure = AccByTenure[c("Detached","Semi","Terraced","Flat","caravan"),]
  hhSizeCarVanByTenure = hhSizeCarVanByTenure[c("p1_car0","p1_car1","p1_car2","p1_car3+","p2_car0","p2_car1","p2_car2","p2_car3+","p3_car0",
                                                "p3_car1","p3_car2","p3_car3+","p4+_car0","p4+_car1","p4+_car2","p4+_car3+"),]
  #NSSECByTenure = NSSECByTenure[c("DNA","L1L2L3","L4L5L6","L7","L8L9","L10L11","L12","L13","L14","L15"),]

  hhCompByTenure = hhCompByTenure[,c("Outright","Mortgage","Social_rented","Private_rented")]
  AccByTenure = AccByTenure[,c("Outright","Mortgage","Social_rented","Private_rented")]
  hhSizeCarVanByTenure = hhSizeCarVanByTenure[,c("Outright","Mortgage","Social_rented","Private_rented")]
  #NSSECByTenure = NSSECByTenure[,c("Outright","Mortgage","Social_rented","Private_rented")]


  hhSizeCarVanByTenure2 <- lapply(list(1:4,5:16), function(rows) { hhSizeCarVanByTenure[rows, , drop = FALSE]})
  hhCompByTenure2 <- lapply(list(1:2,3:11), function(rows) { hhCompByTenure[rows, , drop = FALSE]})

  hhCompByTenure2[[1]] = match_matrix_csums(hhSizeCarVanByTenure2[[1]], hhCompByTenure2[[1]])
  hhCompByTenure2[[2]] = match_matrix_csums(hhSizeCarVanByTenure2[[2]], hhCompByTenure2[[2]])

  hhCompByTenure2 = rbind(hhCompByTenure2[[1]], hhCompByTenure2[[2]])
  hhSizeCarVanByTenure2 = rbind(hhSizeCarVanByTenure2[[1]], hhSizeCarVanByTenure2[[2]])

  AccByTenure = match_matrix_csums(hhSizeCarVanByTenure2, AccByTenure)
  #NSSECByTenure = match_matrix_csums(hhSizeCarVanByTenure2, NSSECByTenure)

  # AccByCarVan = match_matrix_rsums(AccByTenure, AccByCarVan)
  # AccByCarVan = match_matrix_csums(AccByTenure, AccByCarVan)

  # # Hack oder of balancing matters don't add single person housholds
  # hhSizeCarVanByTenure = hhSizeCarVanByTenure[nrow(hhSizeCarVanByTenure):1,]
  #
  # # Harmonise Sums
  # hhCompByTenure = match_matrix_csums(AccByTenure, hhCompByTenure)
  # hhSizeCarVanByTenure = match_matrix_csums(AccByTenure, hhSizeCarVanByTenure)

  #hhSizeCarVanByTenure = hhSizeCarVanByTenure[nrow(hhSizeCarVanByTenure):1,]

  # Humanleauge 3 tables
  # dim = c(nrow(AccByTenure),ncol(AccByTenure),nrow(hhCompByTenure2),nrow(hhSizeCarVanByTenure2), nrow(NSSECByTenure))
  # seed = array(rep(1,prod(dim)), dim = dim)
  #
  # # Set impossible combinations to zero
  # # Order by size
  #
  #
  # # For hhSizeCarVanByTenure rows 1,2,3,4 (households with one person)
  # # and hhCompByTenure rows 1,2 (households with one person)
  #
  # # For hhSizeCarVanByTenure rows 5,6,7,8 (households with two person)
  # # and hhCompByTenure rows 3 (couple no children)
  #
  # # For hhSizeCarVanByTenure rows 5-16 (households with more than one person)
  # # and hhCompByTenure rows 4-11 (housholds with 2-many people)
  #
  # for (i in 1:dim[1]) {
  #   for (j in 1:dim[2]) {
  #     for (k in 1:dim[3]) {
  #       for (l in 1:dim[4]) {
  #         for (m in 1:dim[5]) {
  #           if ((k %in% c(1:2)) & (!l %in% c(1:4))) {
  #             # If one person household and number of household is not 1 set to 0
  #             seed[i, j, k, l, m] = 1e-15
  #           }
  #
  #           if ((k == 3) & (!l %in% c(5:8))) {
  #             # If two person household and number of household is not 2 set to 0
  #             seed[i, j, k, l, m] = 1e-15
  #           }
  #
  #           if ((k %in% 4:5) & (l %in% c(1:8))) {
  #             # If at least three person household and number of household is 1 or 2 set to 0
  #             seed[i, j, k, l, m] = 1e-15
  #           }
  #
  #           if ((k %in% 6:11) & (l %in% c(1:4))) {
  #             # If at least two person household and number of household is 1 set to 0
  #             seed[i, j, k, l, m] = 1e-15
  #           }
  #         }
  #       }
  #     }
  #   }
  # }

  # Poth Synth
  result = try(humanleague::qisi(seed,
                                 indices = list(c(1,2),c(3,2),c(4,2)), #list(c(1,2), c(3,2),c(4,2),c(5,2)),
                                 marginals = list(AccByTenure,
                                                  hhCompByTenure2,
                                                  hhSizeCarVanByTenure2#,NSSECByTenure
                                                  )),
               silent = TRUE)

  if(inherits(result,"try-error")){
    message("QISI failed for ",hhComp_Tenure_sub$LSOA21CD[1]," ",result[1])
    return(NULL)
  }

  result_df = expand.grid(
    rownames(AccByTenure),
    colnames(AccByTenure),
    rownames(hhCompByTenure2),
    #colnames(hhCompByTenure2),
    rownames(hhSizeCarVanByTenure2)#,rownames(NSSECByTenure)
    )
  names(result_df) = c("Acc5","Tenure5",
    "hhComp15","hhSizeCarVan"#,"NSSEC10"
    )


  result_df$households = as.numeric(result$result)
  result_df = result_df[result_df$households > 0,]
  result_df$conv = result$conv
  result_df$pValue = result$pValue



  # # Humanleauge 4 tables
  # seed = array(rep(1,sum(AccByTenure)), dim=c(5,4,5,4,4))
  # result = humanleague::qisi(seed,
  #                            indices = list(c(1,2), c(2,3),c(2,4),c(5,3)),
  #                            marginals = list(AccByTenure, TenureByhhComp,TenureByhhSize,CarVanByhhComp))
  #
  # result_df = expand.grid(nms_acc, nms_tenure,nms_comp,nms_size,nms_car)
  # names(result_df) = c("Acc","Tenure","hhComp","hhSize","Car")
  # result_df$households = as.numeric(result$result)
  # result_df = result_df[result_df$households > 0,]
  # result_df$conv = result$conv
  # result_df$pValue = result$pValue


  # Integrity checks
  if(sum(result_df$households[result_df$Tenure5 == "Mortgage" & result_df$Acc5 == "Flat"]) !=
     AccByTenure["Flat","Mortgage"]){
    warning("check 1 failed for:",Acc_tenure_sub$LSOA21CD[1])
  }
  if(sum(result_df$households[result_df$Tenure5 == "Outright" & result_df$hhComp15 == "OnePersonOver66"]) !=
     hhCompByTenure2["OnePersonOver66","Outright"]){
    warning("check 2 failed for:",hhComp_Tenure_sub$LSOA21CD[1])
  }
  if(sum(result_df$households[result_df$Tenure5 == "Private_rented" & result_df$hhSizeCarVan == "p2_car1"]) !=
     hhSizeCarVanByTenure2["p2_car1","Private_rented"]){
    warning("check 3 failed for:",hhComp_Tenure_sub$LSOA21CD[1])
  }


  result_df$LSOA = hhComp_Tenure_sub$LSOA21CD[1]

  result_df

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


# cenus_syth_combine_v2 = function(Acc_tenure_sub, hhComp_Tenure_sub, Tenure_hhSize_CarVan_sub
#                                   #Tenure_hhSize_sub, CarVan_hhComp_sub
#                                   ) {
#
#   # Check LSOA match
#   # if(length(unique(c(Acc_tenure_sub$LSOA21CD,
#   #                    hhComp_Tenure_sub$LSOA21CD,
#   #                    Tenure_hhSize_sub$LSOA21CD,
#   #                    CarVan_hhComp_sub$LSOA21CD))) != 1){
#   #   stop("More than one LSOA")
#   # }
#
#   # Check LSOA match
#   if(length(unique(c(Acc_tenure_sub$LSOA21CD,
#                      hhComp_Tenure_sub$LSOA21CD,
#                      Tenure_hhSize_CarVan_sub$LSOA21CD))) != 1){
#     stop("More than one LSOA")
#   }
#
#   AccByTenure = as.matrix(Acc_tenure_sub[,3:ncol(Acc_tenure_sub)])
#   hhCompByTenure = as.matrix(hhComp_Tenure_sub[,3:ncol(hhComp_Tenure_sub)])
#   #TenureByhhSize = as.matrix(Tenure_hhSize_sub[,3:ncol(Tenure_hhSize_sub)])
#   #CarVanByhhComp = as.matrix(CarVan_hhComp_sub[,3:ncol(CarVan_hhComp_sub)])
#   hhSizeCarVanByTenure = as.matrix(Tenure_hhSize_CarVan_sub[,3:ncol(Tenure_hhSize_CarVan_sub)])
#
#   #nms_car = c("car0","car1","car2","car3+")
#   #nms_tenure = c("Outright","Mortgage","Social_rented","Private_rented")
#   #nms_acc = c("Detached","Semi","Terrace","Flat", "Caravan")
#   #nms_comp = c("Oneperson", "family66","Couple","Loneparent","Other")
#   #nms_size = c("p1", "p2","p3","p4+")
#
#   rownames(AccByTenure) = Acc_tenure_sub$AccType5
#   rownames(hhCompByTenure) = hhComp_Tenure_sub$hhComp15b
#   rownames(hhSizeCarVanByTenure) = Tenure_hhSize_CarVan_sub$hhSize5_CarVan5
#
#   # Harmonise by household size
#   # The number of 1 person and more than one person households should match
#   hhCompByTenure = hhCompByTenure[c("OnePersonOther","OnePersonOver66", # Only 1
#                                     "CoupleNoChildren", # Only 2
#                                     "CoupleChildren","CoupleNonDepChildren","FamilyOver66","LoneParent", # At least 2
#                                     "LoneParentNonDepChildren","OtherChildren","OtherIncStudentOrOver66","OtherNoChildren"),]
#
#
#   hhSizeCarVanByTenure2 <- lapply(list(1:4,5:16), function(rows) { hhSizeCarVanByTenure[rows, , drop = FALSE]})
#   hhCompByTenure2 <- lapply(list(1:2,3:11), function(rows) { hhCompByTenure[rows, , drop = FALSE]})
#
#   hhCompByTenure2[[1]] = match_matrix_csums(hhSizeCarVanByTenure2[[1]], hhCompByTenure2[[1]])
#   hhCompByTenure2[[2]] = match_matrix_csums(hhSizeCarVanByTenure2[[2]], hhCompByTenure2[[2]])
#
#   hhCompByTenure2 = rbind(hhCompByTenure2[[1]], hhCompByTenure2[[2]])
#   hhSizeCarVanByTenure2 = rbind(hhSizeCarVanByTenure2[[1]], hhSizeCarVanByTenure2[[2]])
#
#   AccByTenure = match_matrix_csums(hhSizeCarVanByTenure2, AccByTenure)
#
#   # # Hack oder of balancing matters don't add single person housholds
#   # hhSizeCarVanByTenure = hhSizeCarVanByTenure[nrow(hhSizeCarVanByTenure):1,]
#   #
#   # # Harmonise Sums
#   # hhCompByTenure = match_matrix_csums(AccByTenure, hhCompByTenure)
#   # hhSizeCarVanByTenure = match_matrix_csums(AccByTenure, hhSizeCarVanByTenure)
#
#   #hhSizeCarVanByTenure = hhSizeCarVanByTenure[nrow(hhSizeCarVanByTenure):1,]
#
#   # Humanleauge 3 tables
#   dim = c(nrow(AccByTenure),ncol(AccByTenure),nrow(hhCompByTenure2),nrow(hhSizeCarVanByTenure2))
#   seed = array(rep(1,prod(dim)), dim = dim)
#
#   # Set impossible combinations to zero
#   # Order by size
#
#
#   # For hhSizeCarVanByTenure rows 1,2,3,4 (households with one person)
#   # and hhCompByTenure rows 1,2 (households with one person)
#
#   # For hhSizeCarVanByTenure rows 5,6,7,8 (households with two person)
#   # and hhCompByTenure rows 3 (couple no children)
#
#   # For hhSizeCarVanByTenure rows 5-16 (households with more than one person)
#   # and hhCompByTenure rows 4-11 (housholds with 2-many people)
#
#   for (i in 1:dim[1]) {
#     for (j in 1:dim[2]) {
#       for (k in 1:dim[3]) {
#         for (l in 1:dim[4]) {
#
#           if ((k %in% c(1:2)) & (!l %in% c(1:4))) {
#             # If one person household and number of household is not 1 set to 0
#             seed[i, j, k, l] = 1e-15
#           }
#
#           if ((k == 3) & (!l %in% c(5:8))) {
#             # If two person household and number of household is not 2 set to 0
#             seed[i, j, k, l] = 1e-15
#           }
#
#           if ((k %in% 4:5) & (l %in% c(1:8))) {
#             # If at least three person household and number of household is 1 or 2 set to 0
#             seed[i, j, k, l] = 1e-15
#           }
#
#           if ((k %in% 6:11) & (l %in% c(1:4))) {
#             # If at least two person household and number of household is 1 set to 0
#             seed[i, j, k, l] = 1e-15
#           }
#
#
#
#         }
#       }
#     }
#   }
#
#
#
#   result = try(humanleague::qisi(seed,
#                              indices = list(c(1,2), c(3,2),c(4,2)),
#                              marginals = list(AccByTenure, hhCompByTenure2,hhSizeCarVanByTenure2)), silent = TRUE)
#
#   if(inherits(result,"try-error")){
#     message("QISI failed for ",Acc_tenure_sub$LSOA21CD[1])
#     return(NULL)
#   }
#
#   result_df = expand.grid(rownames(AccByTenure), colnames(AccByTenure), rownames(hhCompByTenure2), rownames(hhSizeCarVanByTenure2))
#   names(result_df) = c("Acc","Tenure","hhComp","hhSizeCarVan")
#   result_df$households = as.numeric(result$result)
#   result_df = result_df[result_df$households > 0,]
#   result_df$conv = result$conv
#   result_df$pValue = result$pValue
#
#
#
#   # # Humanleauge 4 tables
#   # seed = array(rep(1,sum(AccByTenure)), dim=c(5,4,5,4,4))
#   # result = humanleague::qisi(seed,
#   #                            indices = list(c(1,2), c(2,3),c(2,4),c(5,3)),
#   #                            marginals = list(AccByTenure, TenureByhhComp,TenureByhhSize,CarVanByhhComp))
#   #
#   # result_df = expand.grid(nms_acc, nms_tenure,nms_comp,nms_size,nms_car)
#   # names(result_df) = c("Acc","Tenure","hhComp","hhSize","Car")
#   # result_df$households = as.numeric(result$result)
#   # result_df = result_df[result_df$households > 0,]
#   # result_df$conv = result$conv
#   # result_df$pValue = result$pValue
#
#
#   # Integrity checks
#   if(sum(result_df$households[result_df$Tenure == "Mortgage" & result_df$Acc == "Flat"]) !=
#      AccByTenure["Flat","Mortgage"]){
#     warning("check 1 failed for:",Acc_tenure_sub$LSOA21CD[1])
#   }
#   if(sum(result_df$households[result_df$Tenure == "Outright" & result_df$hhComp == "OnePersonOver66"]) !=
#      hhCompByTenure2["OnePersonOver66","Outright"]){
#     warning("check 2 failed for:",Acc_tenure_sub$LSOA21CD[1])
#   }
#   if(sum(result_df$households[result_df$Tenure == "Private_rented" & result_df$hhSizeCarVan == "p2_car1"]) !=
#      hhSizeCarVanByTenure2["p2_car1","Private_rented"]){
#     warning("check 3 failed for:" )
#   }
#
#
#   result_df$LSOA = Acc_tenure_sub$LSOA21CD[1]
#
#   result_df
#
# }
#
# cenus_syth_combine = function(Acc_tenure_sub, Acc_CarVan_sub, Acc_hhComp_sub, hhSize_hhComp_sub#,
#                               #hhComp_Tenure_sub, Tenure_hhSize_sub, CarVan_hhComp_sub, Acc_hhSize_sub
#                               ) {
#
#   # Check LSOA match
#   if(length(unique(c(Acc_tenure_sub$LSOA21CD,
#                      Acc_CarVan_sub$LSOA21CD,
#                      Acc_hhComp_sub$LSOA21CD,
#                      hhSize_hhComp_sub$LSOA21CD))) != 1){
#     stop("More than one LSOA")
#   }
#
#   AccByTenure = as.matrix(Acc_tenure_sub[,3:ncol(Acc_tenure_sub)])
#   AccByCarVan = as.matrix(Acc_CarVan_sub[,3:ncol(Acc_CarVan_sub)])
#   AccByhhComp = as.matrix(Acc_hhComp_sub[,3:ncol(Acc_hhComp_sub)])
#   hhSizeByhhComp = as.matrix(hhSize_hhComp_sub[,3:ncol(hhSize_hhComp_sub)])
#
#   nms_car = c("car0","car1","car2","car3+")
#   nms_tenure = c("Outright","Mortgage","Social_rented","Private_rented")
#   nms_acc = c("Detached","Semi","Terrace","Flat", "Caravan")
#   nms_comp = c("Oneperson", "family66","Couple","Loneparent","Other")
#   nms_size = c("p1", "p2","p3","p4+")
#
#   rownames(AccByTenure) = c(nms_acc)
#   colnames(AccByTenure) = c(nms_tenure)
#
#   rownames(AccByCarVan) = c(nms_acc)
#   colnames(AccByCarVan) = c(nms_car)
#
#   rownames(AccByhhComp) = c(nms_acc)
#   colnames(AccByhhComp) = c(nms_comp)
#
#   rownames(hhSizeByhhComp) = c(nms_size)
#   colnames(hhSizeByhhComp) = c(nms_comp)
#
#
#   # Harmonise Sums
#   AccByhhComp = match_matrix_csums(hhSizeByhhComp, AccByhhComp)
#   AccByTenure = match_matrix_rsums(AccByhhComp, AccByTenure)
#   AccByCarVan = match_matrix_rsums(AccByTenure, AccByCarVan)
#
#   # Humanleauge 4 tables
#   seed = array(rep(1,sum(AccByTenure)), dim=c(5,4,4,5,4))
#   result = humanleague::qisi(seed,
#                 indices = list(c(1,2), c(1,3),c(1,4),c(5,4)),
#                 marginals = list(AccByTenure, AccByCarVan,AccByhhComp,hhSizeByhhComp))
#
#   result_df = expand.grid(nms_acc, nms_tenure,nms_car,nms_comp,nms_size)
#   names(result_df) = c("Acc","Tenure","Car","hhComp","hhSize")
#   result_df$households = as.numeric(result$result)
#   result_df = result_df[result_df$households > 0,]
#   result_df$conv = result$conv
#   result_df$pValue = result$pValue
#
#
#
#   # Integrity checks
#   if(sum(result_df$households[result_df$hhComp == "Oneperson" & result_df$hhSize == "p1"]) !=
#      hhSizeByhhComp["p1","Oneperson"]){
#     stop("check 1 failed")
#   }
#   if(sum(result_df$households[result_df$hhComp == "Couple" & result_df$Acc == "Terrace"]) !=
#      AccByhhComp["Terrace","Couple"]){
#     stop("check 2 failed")
#   }
#   if(sum(result_df$households[result_df$Car == "car2" & result_df$Acc == "Detached"]) !=
#      AccByCarVan["Detached","car2"]){
#     stop("check 3 failed")
#   }
#   if(sum(result_df$households[result_df$Tenure == "Social_rented" & result_df$Acc == "Flat"]) !=
#      AccByTenure["Flat","Social_rented"]){
#     stop("check 4 failed")
#   }
#
#   result_df$LSOA = Acc_tenure_sub$LSOA21CD[1]
#
#   result_df
#
# }
#
#
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
