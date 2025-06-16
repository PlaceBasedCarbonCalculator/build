#library(dplyr)
#library(tidyr)

build_synth_pop_seed = function(path_data = file.path(parameters$path_data,"population")){

  sizeCarTenureComp = read.csv(file.path(path_data,"census2021EW_Households_HouseholdComposition15_Tenure5_Size5_CarVar_5_EW.csv"))
  AccsizeTenureComp = read.csv(file.path(path_data,"census2021EW_Households_HouseholdComposition15_Tenure5_Size5_Acc5_EW.csv"))
  #NSSECsizeTenureComp = read.csv(file.path(path_data,"census2021EW_RefPerson_NSSEC10_Household15_Tenure5_Size5_EW.csv"))

  names(sizeCarTenureComp) = c("England.and.Wales.Code","England.and.Wales",
                               "Household.size..5.categories..Code","hhSize5",
                               "Car.or.van.availability..5.categories..Code", "CarVan5",
                               "Tenure.of.household..5.categories..Code","Tenure5",
                               "Household.composition..15.categories..Code","hhComp15",
                               "households")

  sizeCarTenureComp = sizeCarTenureComp[,c("hhSize5","CarVan5","Tenure5","hhComp15","households")]
  sizeCarTenureComp = sizeCarTenureComp[sizeCarTenureComp$hhSize5 != "0 people in household",]
  sizeCarTenureComp = sizeCarTenureComp[sizeCarTenureComp$Tenure5 != "Does not apply",]
  sizeCarTenureComp = sizeCarTenureComp[sizeCarTenureComp$hhComp15 != "Does not apply",]
  sizeCarTenureComp = sizeCarTenureComp[sizeCarTenureComp$CarVan5 != "Does not apply",]



  names(AccsizeTenureComp) = c("England.and.Wales.Code","England.and.Wales",
                               "Household.size..5.categories..Code","hhSize5",
                               "Tenure.of.household..5.categories..Code","Tenure5",
                               "Household.composition..15.categories..Code","hhComp15",
                               "Accommodation.type..5.categories..Code","Acc5",
                               "households" )
  AccsizeTenureComp = AccsizeTenureComp[,c("hhSize5","Acc5","Tenure5","hhComp15","households")]

  AccsizeTenureComp = AccsizeTenureComp[AccsizeTenureComp$hhSize5 != "0 people in household",]
  AccsizeTenureComp = AccsizeTenureComp[AccsizeTenureComp$Tenure5 != "Does not apply",]
  AccsizeTenureComp = AccsizeTenureComp[AccsizeTenureComp$hhComp15 != "Does not apply",]
  AccsizeTenureComp = AccsizeTenureComp[AccsizeTenureComp$Acc5 != "Does not apply",]

  # names(NSSECsizeTenureComp) = c("England.and.Wales.Code","England.and.Wales",
  #                                "Tenure.of.household..5.categories..Code","Tenure5",
  #                                "Household.size..5.categories..Code","hhSize5",
  #                                "Household.composition..15.categories..Code","hhComp15",
  #                                "National.Statistics.Socio.economic.Classification..NS.SeC...10.categories..Code","NSSEC10",
  #                                "households")
  #
  # NSSECsizeTenureComp = NSSECsizeTenureComp[,c("hhSize5","NSSEC10","Tenure5","hhComp15","households")]
  #
  # NSSECsizeTenureComp = NSSECsizeTenureComp[NSSECsizeTenureComp$hhSize5 != "0 people in household",]
  # NSSECsizeTenureComp = NSSECsizeTenureComp[NSSECsizeTenureComp$Tenure5 != "Does not apply",]
  # NSSECsizeTenureComp = NSSECsizeTenureComp[NSSECsizeTenureComp$hhComp15 != "Does not apply",]

  sizeCarTenureComp$hhComp15 = simplify_household15(sizeCarTenureComp$hhComp15)
  sizeCarTenureComp$hhComp15 = gsub("Cohabit|Married","Couple",sizeCarTenureComp$hhComp15)
  sizeCarTenureComp = dplyr::group_by(sizeCarTenureComp, hhSize5,CarVan5,Tenure5,hhComp15)
  sizeCarTenureComp = dplyr::summarise(sizeCarTenureComp, households = sum(households))
  sizeCarTenureComp = dplyr::ungroup(sizeCarTenureComp)

  sizeCarTenureComp$hhSize5 = simplify_hhSize5(sizeCarTenureComp$hhSize5)
  sizeCarTenureComp$CarVan5 = simplify_CarVan5(sizeCarTenureComp$CarVan5)
  sizeCarTenureComp$Tenure5 = simplify_Tenure5(sizeCarTenureComp$Tenure5)

  AccsizeTenureComp$hhComp15 = simplify_household15(AccsizeTenureComp$hhComp15)
  AccsizeTenureComp$hhComp15 = gsub("Cohabit|Married","Couple",AccsizeTenureComp$hhComp15)
  AccsizeTenureComp = dplyr::group_by(AccsizeTenureComp, hhSize5,Acc5,Tenure5,hhComp15)
  AccsizeTenureComp = dplyr::summarise(AccsizeTenureComp, households = sum(households))
  AccsizeTenureComp = dplyr::ungroup(AccsizeTenureComp)

  AccsizeTenureComp$hhSize5 = simplify_hhSize5(AccsizeTenureComp$hhSize5)
  AccsizeTenureComp$Acc5 = simplify_AccType5(AccsizeTenureComp$Acc5)
  AccsizeTenureComp$Tenure5 = simplify_Tenure5(AccsizeTenureComp$Tenure5)

  # NSSECsizeTenureComp$hhComp15 = simplify_household15(NSSECsizeTenureComp$hhComp15)
  # NSSECsizeTenureComp$hhComp15 = gsub("Cohabit|Married","Couple",NSSECsizeTenureComp$hhComp15)
  # NSSECsizeTenureComp = dplyr::group_by(NSSECsizeTenureComp, hhSize5,NSSEC10,Tenure5,hhComp15)
  # NSSECsizeTenureComp = dplyr::summarise(NSSECsizeTenureComp, households = sum(households))
  # NSSECsizeTenureComp = dplyr::ungroup(NSSECsizeTenureComp)
  #
  # NSSECsizeTenureComp$hhSize5 = simplify_hhSize5(NSSECsizeTenureComp$hhSize5)
  # NSSECsizeTenureComp$NSSEC10 = simplify_nssec(NSSECsizeTenureComp$NSSEC10)
  # NSSECsizeTenureComp$Tenure5 = simplify_Tenure5(NSSECsizeTenureComp$Tenure5)

  # Collapse to 2 dimensions

  AccsizeTenureComp$shared = paste0(AccsizeTenureComp$hhSize5,"_",
                                    AccsizeTenureComp$Tenure5,"_",
                                    AccsizeTenureComp$hhComp15)
  sizeCarTenureComp$shared = paste0(sizeCarTenureComp$hhSize5,"_",
                                    sizeCarTenureComp$Tenure5,"_",
                                    sizeCarTenureComp$hhComp15)
  # NSSECsizeTenureComp$shared = paste0(NSSECsizeTenureComp$hhSize5,"_",
  #                                     NSSECsizeTenureComp$Tenure5,"_",
  #                                     NSSECsizeTenureComp$hhComp15)

  AccByShared = AccsizeTenureComp[,c("Acc5","shared","households")]
  CarByShared = sizeCarTenureComp[,c("CarVan5","shared","households")]
  #NSSECByShared = NSSECsizeTenureComp[,c("NSSEC10","shared","households")]

  AccByShared = pivot_wider(AccByShared, names_from = "Acc5", values_from = "households")
  CarByShared = pivot_wider(CarByShared, names_from = "CarVan5", values_from = "households")
  #NSSECByShared = pivot_wider(NSSECByShared, names_from = "NSSEC10", values_from = "households")

  AccByShared = AccByShared[order(AccByShared$shared),]
  CarByShared = CarByShared[order(CarByShared$shared),]
  #NSSECByShared = NSSECByShared[order(NSSECByShared$shared),]

  CarByShared = CarByShared[,c("shared","car0","car1","car2","car3+")]
  #NSSECByShared = NSSECByShared[,c("shared","DNA","L1L2L3","L4L5L6","L7","L8L9","L10L11","L12","L13","L14","L15")]

  AccBySharedM = as.matrix(AccByShared[,2:6])
  rownames(AccBySharedM) = AccByShared$shared

  CarBySharedM = as.matrix(CarByShared[,2:5])
  rownames(CarBySharedM) = CarByShared$shared

  # NSSECBySharedM = as.matrix(NSSECByShared[,2:11])
  # rownames(NSSECBySharedM) = NSSECByShared$shared

  rownames(CarBySharedM) = gsub("_rented","rented",rownames(CarBySharedM))
  rownames(AccBySharedM) = gsub("_rented","rented",rownames(AccBySharedM))
  #rownames(NSSECBySharedM) = gsub("_rented","rented",rownames(NSSECBySharedM))

  CarBySharedM = match_matrix_rsums(AccBySharedM, CarBySharedM)
  # NSSECBySharedM = match_matrix_rsums(AccBySharedM, NSSECBySharedM)
  #NSSECBySharedM = match_matrix_rsums(CarBySharedM, NSSECBySharedM)

  dim = c(nrow(AccBySharedM),ncol(AccBySharedM),ncol(CarBySharedM))#,ncol(NSSECBySharedM))
  #dim = c(nrow(CarBySharedM),ncol(CarBySharedM),ncol(NSSECBySharedM))
  seed = array(rep(1,prod(dim)), dim = dim)

  res_seed = humanleague::qisi(seed,
                               #indices = list(c(1,2), c(1,3),c(1,4)),
                               indices = list(c(1,2), c(1,3)),
                               marginals = list(AccBySharedM,
                                 CarBySharedM))#,NSSECBySharedM))

  result_seed_df = expand.grid(rownames(AccBySharedM),colnames(AccBySharedM),
    #rownames(CarBySharedM),
    colnames(CarBySharedM))#,colnames(NSSECBySharedM))
  names(result_seed_df) = c("shared","Acc5",
                            "CarVan5")#,"NSSEC10")
  result_seed_df$households = as.numeric(res_seed$result)

  slt = strsplit(as.character(result_seed_df$shared),"_")

  result_seed_df$hhSize5 = sapply(slt, `[[`, 1)
  result_seed_df$Tenure5 = sapply(slt, `[[`, 2)
  result_seed_df$hhComp15 = sapply(slt, `[[`, 3)

  #length(unique(result_seed_df$hhSize5))
  #length(unique(result_seed_df$Tenure5))
  #length(unique(result_seed_df$hhSize5))
  #length(unique(result_seed_df$hhComp15))
  #length(unique(result_seed_df$Acc5))
  #length(unique(result_seed_df$hhSizeCarVan))

  result_seed_df$hhSizeCarVan = paste0(result_seed_df$hhSize5,"_",result_seed_df$CarVan5)

  result_seed_df = result_seed_df[order(result_seed_df$Acc5,
                                        result_seed_df$Tenure5,
                                        result_seed_df$hhComp15,
                                        result_seed_df$hhSizeCarVan),]

  result_seed_df$seed = result_seed_df$households / 40000
  result_seed_df$seed = ifelse(result_seed_df$seed > 1, 1,  result_seed_df$seed)
  result_seed_df$seed = ifelse(result_seed_df$seed < 1 &    result_seed_df$seed > 0.03, 0.75 , result_seed_df$seed)
  result_seed_df$seed = ifelse(result_seed_df$seed < 0.03 & result_seed_df$seed > 0.01, 0.5  , result_seed_df$seed)
  result_seed_df$seed = ifelse(result_seed_df$seed < 0.01 & result_seed_df$seed > 0   , 0.001, result_seed_df$seed)

  result_seed_df$seed[result_seed_df$seed == 0] = 1e-15

  result_seed_df
  # seed_new3 = array(result_seed_df$households_bin2, dim = c(5,4,11,16,10))
  # seed_new3[seed_new3 == 0] = 1e-15
  #
  # seed_new3

}




