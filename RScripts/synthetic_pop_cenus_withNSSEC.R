sythetic_census = function(path_data = file.path(parameters$path_data,"population"), synth_pop_seed){

  #Acc_hhComp = read_Acc_hhComp(file.path(path_data,"census2021EW_Households_HouseholdComposition15_AccomodationType5_LSOA_partial.csv"))  # Partial
  #Acc_CarVan = read_Acc_CarVan(file.path(path_data,"census2021EW_Households_AccomodationType5_CarVan5_LSOA_partial.csv"))  # Partial
  #hhSize_hhComp = read_hhSize_hhComp(file.path(path_data,"census2021EW_Households_HouseholdComposition6_HouseholdSize5_LSOA_partial.csv"))  # Partial
  #Acc_tenure = read_Acc_tenure(file.path(path_data,"census2021EW_Households_AccomodationType5_Tenure5_LSOA.csv"))
  hhComp_Tenure = read_hhComp_Tenure(file.path(path_data,"census2021EW_Households_HouseholdComposition15_Tenure5_LSOA.csv"))
  Tenure_hhSize = read_Tenure_hhSize(file.path(path_data,"census2021EW_Households_Tenure5_HouseholdSize5_LSOA.csv"))
  #CarVan_hhComp = read_CarVan_hhComp(file.path(path_data,"census2021EW_Households_HouseholdComposition6_CarVan5_LSOA.csv"))
  Tenure_CarVan = read_Tenure_CarVan(file.path(path_data,"census2021EW_Households_Tenure5_CarVan5_LSOA.csv"))
  Tenure_hhSize_CarVan = read_Tenure_hhSize_CarVan(file.path(path_data,"census2021EW_Households_Tenure5_HouseholdSize5_CarVan5_LSOA_partial.csv")) # Partial
  Tenure_NSSEC = read_Tenure_NSSEC(file.path(path_data,"census2021EW_RefPerson_NSSEC10_Tenure5_LSOA.csv"))
  hhComp_NSSEC = read_hhComp_NSSEC(file.path(path_data,"census2021EW_RefPerson_NSSEC10_Houshold15_LSOA_partial.csv"))

  # Remove DNA if all zeros
  #Acc_tenure = Acc_tenure[Acc_tenure$Tenure5 != "Does not apply",]
  Tenure_NSSEC = Tenure_NSSEC[Tenure_NSSEC$Tenure5 != "Does not apply",]
  hhComp_NSSEC = hhComp_NSSEC[hhComp_NSSEC$hhComp15 != "Does not apply",]
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
  #Acc_tenure$Tenure5 = simplify_Tenure5(Acc_tenure$Tenure5)
  #Acc_tenure$AccType5 = simplify_AccType5(Acc_tenure$AccType5)
  Tenure_CarVan$Tenure5 = simplify_Tenure5(Tenure_CarVan$Tenure5)
  Tenure_CarVan$CarVan5 = simplify_CarVan5(Tenure_CarVan$CarVan5)
  hhComp_Tenure$Tenure5 = simplify_Tenure5(hhComp_Tenure$Tenure5)
  hhComp_NSSEC$NSSEC10 =  simplify_nssec(hhComp_NSSEC$NSSEC10)
  Tenure_NSSEC$Tenure5 = simplify_Tenure5(Tenure_NSSEC$Tenure5)
  Tenure_NSSEC$NSSEC10 =  simplify_nssec(Tenure_NSSEC$NSSEC10)
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

  hhComp_NSSEC$hhComp15 = simplify_household15(hhComp_NSSEC$hhComp15)
  hhComp_NSSEC$hhComp15 = gsub("Cohabit|Married","Couple",hhComp_NSSEC$hhComp15)
  hhComp_NSSEC = dplyr::group_by(hhComp_NSSEC, LSOA21CD, NSSEC10, hhComp15)
  hhComp_NSSEC = dplyr::summarise(hhComp_NSSEC, households = sum(households))
  hhComp_NSSEC = dplyr::ungroup(hhComp_NSSEC)

  # Find common core
  lsoa_common = Reduce(intersect,
                       list(
                         #unique(Acc_tenure$LSOA21CD),
                         unique(hhComp_Tenure$LSOA21CD),
                         unique(Tenure_hhSize_CarVan$LSOA21CD),
                         unique(Tenure_NSSEC$LSOA21CD)
                       )) #35148 98.5%

  #Acc_tenure_com = Acc_tenure[Acc_tenure$LSOA21CD %in% lsoa_common,]
  hhComp_Tenure_com = hhComp_Tenure[hhComp_Tenure$LSOA21CD %in% lsoa_common,]
  Tenure_hhSize_CarVan_com = Tenure_hhSize_CarVan[Tenure_hhSize_CarVan$LSOA21CD %in% lsoa_common,]
  Tenure_NSSEC_com = Tenure_NSSEC[Tenure_NSSEC$LSOA21CD %in% lsoa_common,]

  #Acc_CarVan_com = Acc_CarVan[Acc_CarVan$LSOA21CD %in% lsoa_common,]

  # Reduce to 2 variables
  Tenure_hhSize_CarVan_com = Tenure_hhSize_CarVan_com[order(Tenure_hhSize_CarVan_com$hhSize5),]
  Tenure_hhSize_CarVan_com$hhSize5_CarVan5 = paste0(Tenure_hhSize_CarVan_com$hhSize5,"_",Tenure_hhSize_CarVan_com$CarVan5)
  Tenure_hhSize_CarVan_com = Tenure_hhSize_CarVan_com[,c("LSOA21CD","Tenure5","hhSize5_CarVan5","households")]

  # Pivot
  #Acc_tenure_com = tidyr::pivot_wider(Acc_tenure_com, names_from = "Tenure5", values_from = "households")
  hhComp_Tenure_com = tidyr::pivot_wider(hhComp_Tenure_com, names_from = "Tenure5", values_from = "households")
  Tenure_hhSize_CarVan_com = tidyr::pivot_wider(Tenure_hhSize_CarVan_com, names_from = "Tenure5", values_from = "households")
  Tenure_NSSEC_com = tidyr::pivot_wider(Tenure_NSSEC_com, names_from = "Tenure5", values_from = "households")

  #Acc_CarVan_com = tidyr::pivot_wider(Acc_CarVan_com, names_from = "CarVan5", values_from = "households")

  # Match Column orders
  #Acc_tenure_com = Acc_tenure_com[,c("LSOA21CD","AccType5","Outright","Mortgage","Social_rented","Private_rented")]
  hhComp_Tenure_com = hhComp_Tenure_com[,c("LSOA21CD","hhComp15","Outright","Mortgage","Social_rented","Private_rented")]
  Tenure_hhSize_CarVan_com = Tenure_hhSize_CarVan_com[,c("LSOA21CD","hhSize5_CarVan5","Outright","Mortgage","Social_rented","Private_rented")]
  Tenure_NSSEC_com = Tenure_NSSEC_com[,c("LSOA21CD","NSSEC10","Outright","Mortgage","Social_rented","Private_rented")]

  # Order
  #Acc_tenure_com    = Acc_tenure_com[order(Acc_tenure_com$LSOA21CD),]
  hhComp_Tenure_com    = hhComp_Tenure_com[order(hhComp_Tenure_com$LSOA21CD),]
  Tenure_hhSize_CarVan_com    = Tenure_hhSize_CarVan_com[order(Tenure_hhSize_CarVan_com$LSOA21CD),]
  Tenure_NSSEC_com    = Tenure_NSSEC_com[order(Tenure_NSSEC_com$LSOA21CD),]

  #Acc_CarVan_com    = Acc_CarVan_com[order(Acc_CarVan_com$LSOA21CD),]

  # Split
  #Acc_tenure_com = dplyr::group_split(dplyr::ungroup(Acc_tenure_com), LSOA21CD)
  hhComp_Tenure_com = dplyr::group_split(hhComp_Tenure_com, LSOA21CD)
  Tenure_hhSize_CarVan_com = dplyr::group_split(Tenure_hhSize_CarVan_com, LSOA21CD)
  Tenure_NSSEC_com = dplyr::group_split(Tenure_NSSEC_com, LSOA21CD)

  #Acc_CarVan_com = dplyr::group_split(Acc_CarVan_com, LSOA21CD)

  # Build Seed
  seed_df = expand.grid(#c("Detached","Semi","Terraced","Flat","caravan"),
    c("OnePersonOther","OnePersonOver66",
      "CoupleNoChildren",
      "CoupleChildren","CoupleNonDepChildren","FamilyOver66","LoneParent",
      "LoneParentNonDepChildren","OtherChildren","OtherIncStudentOrOver66","OtherNoChildren"),
    c("Outright","Mortgage","Social_rented","Private_rented"),
    c("p1_car0","p1_car1","p1_car2","p1_car3+","p2_car0","p2_car1","p2_car2","p2_car3+","p3_car0",
      "p3_car1","p3_car2","p3_car3+","p4+_car0","p4+_car1","p4+_car2","p4+_car3+"),
    c("DNA","L1L2L3","L4L5L6","L7","L8L9","L10L11","L12","L13","L14","L15"))
  names(seed_df) = c(#"Acc5",
    "hhComp15","Tenure5","hhSizeCarVan","NSSEC10")

  seed_df[] <- lapply(seed_df[], as.character)

  # Match Seed to Inputs
  synth_pop_seed = synth_pop_seed[,c(#"Acc5",
    "hhComp15","Tenure5","hhSizeCarVan","NSSEC10","households","seed")]
  synth_pop_seed[1:4] <- lapply(synth_pop_seed[1:4], as.character)

  synth_pop_seed$Tenure5 = gsub("Privaterented","Private_rented",synth_pop_seed$Tenure5)
  synth_pop_seed$Tenure5 = gsub("Socialrented","Social_rented",synth_pop_seed$Tenure5)

  # Create a key column by concatenating columns' values
  seed_df$key <- apply(seed_df, 1, paste, collapse = "_")
  synth_pop_seed$key <- apply(synth_pop_seed[,c(#"Acc5",
    "hhComp15","Tenure5","hhSizeCarVan","NSSEC10")], 1, paste, collapse = "_")

  # Match rows of A to B using the key column
  synth_pop_seed <- synth_pop_seed[match(seed_df$key, synth_pop_seed$key), ]

  # Remove the key column
  synth_pop_seed$key <- NULL
  seed_df$key <- NULL

  #seed = array(synth_pop_seed$seed, dim = c(5,4,11,16,10))
  seed = array(synth_pop_seed$seed, dim = c(11,4,16,10))

  # Combine
  future::plan("multisession")
  res_com = furrr::future_pmap(.l = list(#Acc_tenure_com,
    hhComp_Tenure_com, Tenure_hhSize_CarVan_com, Tenure_NSSEC_com),
    .f = cenus_syth_combine_v3, seed = seed,
    .progress = TRUE, .options = furrr::furrr_options(seed = TRUE))
  future::plan("sequential")
  res_com = dplyr::bind_rows(res_com)

  res_com$hhSize5 = sapply(strsplit(as.character(res_com$hhSizeCarVan),"_"),`[[`,1)
  res_com$CarVan5 = sapply(strsplit(as.character(res_com$hhSizeCarVan),"_"),`[[`,2)
  res_com$hhSizeCarVan = NULL

  # Do some cross-validation
  # Acc_CarVan = read_Acc_CarVan(file.path(path_data,"census2021EW_Households_AccomodationType5_CarVan5_LSOA_partial.csv"))
  # Acc_CarVan = Acc_CarVan[Acc_CarVan$LSOA21CD %in% res_com$LSOA,]
  # Acc_CarVan$AccType5 = simplify_AccType5(Acc_CarVan$AccType5)
  # Acc_CarVan$CarVan5 = simplify_CarVan5(Acc_CarVan$CarVan5)
  # Acc_CarVan = Acc_CarVan[Acc_CarVan$CarVan5 != "Does not apply",]
  #
  # res_AccCarVan = res_com[,c("LSOA","Acc","hhSizeCarVan","households")]
  # res_AccCarVan$CarVan = sapply(strsplit(as.character(res_AccCarVan$hhSizeCarVan),"_"),`[[`, 2)
  # res_AccCarVan = res_AccCarVan[res_AccCarVan$LSOA %in% Acc_CarVan$LSOA21CD,]
  # res_AccCarVan = dplyr::group_by(res_AccCarVan, LSOA, Acc, CarVan)
  # res_AccCarVan = dplyr::summarise(res_AccCarVan, households = sum(households))
  #
  # foo = dplyr::full_join(Acc_CarVan, res_AccCarVan, by = c("LSOA21CD" ="LSOA", "AccType5" = "Acc", "CarVan5" = "CarVan"))
  # foo$households.x[is.na(foo$households.x)] = 0
  # foo$households.y[is.na(foo$households.y)] = 0
  #
  # ggplot(foo, aes(x = households.x, y = households.y, colour = AccType5, shape = CarVan5)) +
  # geom_point() +
  #   xlab("Number of Households in Census 2021") +
  #      ylab("Number Households in Synthetic Population") +
  #   ggtitle("Accommodation Type and Car/Van Ownership") +
  #   geom_abline(, color = "black") +
  #   labs(color='Accommodation Type') +
  #   labs(shape='Car/Van Ownership')
  #   theme(legend.position = "none")
  #
  # cor(foo$households.x, foo$households.y) #0.9874037 very good
  # summary(lm(foo$households.x ~ foo$households.y))

  #Alt mesaure for unusual LSOA
  lsoa_alt = unique(hhComp_Tenure$LSOA21CD)
  lsoa_alt = lsoa_alt[!lsoa_alt %in% lsoa_common]

  #Acc_tenure_alt = Acc_tenure[Acc_tenure$LSOA21CD %in% lsoa_alt,]
  hhComp_Tenure_alt = hhComp_Tenure[hhComp_Tenure$LSOA21CD %in% lsoa_alt,]
  Tenure_hhSize_alt = Tenure_hhSize[Tenure_hhSize$LSOA21CD %in% lsoa_alt,]
  Tenure_CarVan_alt = Tenure_CarVan[Tenure_CarVan$LSOA21CD %in% lsoa_alt,]
  Tenure_NSSEC_alt = Tenure_NSSEC[Tenure_NSSEC$LSOA21CD %in% lsoa_alt,]

  #Acc_tenure_alt = tidyr::pivot_wider(Acc_tenure_alt, names_from = "Tenure5", values_from = "households")
  hhComp_Tenure_alt = tidyr::pivot_wider(hhComp_Tenure_alt, names_from = "Tenure5", values_from = "households")
  Tenure_hhSize_alt = tidyr::pivot_wider(Tenure_hhSize_alt, names_from = "Tenure5", values_from = "households")
  Tenure_CarVan_alt = tidyr::pivot_wider(Tenure_CarVan_alt, names_from = "Tenure5", values_from = "households")
  Tenure_NSSEC_alt = tidyr::pivot_wider(Tenure_NSSEC_alt, names_from = "Tenure5", values_from = "households")

  #Acc_tenure_alt = Acc_tenure_alt[,c("LSOA21CD","AccType5","Outright","Mortgage","Social_rented","Private_rented")]
  hhComp_Tenure_alt = hhComp_Tenure_alt[,c("LSOA21CD","hhComp15","Outright","Mortgage","Social_rented","Private_rented")]
  Tenure_hhSize_alt = Tenure_hhSize_alt[,c("LSOA21CD","hhSize5","Outright","Mortgage","Social_rented","Private_rented")]
  Tenure_CarVan_alt = Tenure_CarVan_alt[,c("LSOA21CD","CarVan5","Outright","Mortgage","Social_rented","Private_rented")]
  Tenure_NSSEC_alt = Tenure_NSSEC_alt[,c("LSOA21CD","NSSEC10","Outright","Mortgage","Social_rented","Private_rented")]

  #Acc_tenure_alt    = Acc_tenure_alt[order(Acc_tenure_alt$LSOA21CD),]
  hhComp_Tenure_alt = hhComp_Tenure_alt[order(hhComp_Tenure_alt$LSOA21CD),]
  Tenure_hhSize_alt = Tenure_hhSize_alt[order(Tenure_hhSize_alt$LSOA21CD),]
  Tenure_CarVan_alt = Tenure_CarVan_alt[order(Tenure_CarVan_alt$LSOA21CD),]
  Tenure_NSSEC_alt = Tenure_NSSEC_alt[order(Tenure_NSSEC_alt$LSOA21CD),]

  #Acc_tenure_alt = dplyr::group_split(Acc_tenure_alt, LSOA21CD)
  hhComp_Tenure_alt = dplyr::group_split(hhComp_Tenure_alt, LSOA21CD)
  Tenure_hhSize_alt = dplyr::group_split(Tenure_hhSize_alt, LSOA21CD)
  Tenure_CarVan_alt = dplyr::group_split(Tenure_CarVan_alt, LSOA21CD)
  Tenure_NSSEC_alt = dplyr::group_split(Tenure_NSSEC_alt, LSOA21CD)

  # Combine
  future::plan("multisession")
  res_alt = furrr::future_pmap(.l = list(#Acc_tenure_alt,
    hhComp_Tenure_alt, Tenure_hhSize_alt,Tenure_CarVan_alt,Tenure_NSSEC_alt),
    .f = cenus_syth_combine_alt, .progress = TRUE,  .options = furrr::furrr_options(seed = TRUE))
  future::plan("sequential")
  res_alt = dplyr::bind_rows(res_alt)


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
  hhComp_Tenure_sub = hhComp_Tenure_alt[[1]]
  Tenure_hhSize_sub = Tenure_hhSize_alt[[1]]
  Tenure_CarVan_sub = Tenure_CarVan_alt[[1]]
  # Acc_tenure_sub = Acc_tenure_alt[[1]]
  Tenure_NSSEC_sub = Tenure_NSSEC_alt[[1]]



  res_final = rbind(res_com, res_alt)

  res_final
}
