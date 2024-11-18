load_house_prices = function(path = file.path(parameters$path_data,"house_price_age"),
                             lsoa_11_21_tools){
  hp1 = read.csv(file.path(path,"Median_Prices_Quarterly.csv"))
  hp2 = read.csv(file.path(path,"hpssa202103.csv"))

  #Standerdies to Q1 as 2020/2021 data is for March

  hp1 = hp1[,c("lsoa_cd",names(hp1)[grepl("Q1",names(hp1))])]
  names(hp1) = c("LSOA11CD",paste0("houseprice_",1995:2018))
  hp1 = hp1[!is.na(hp1$LSOA11CD),]
  hp1 = hp1[!substr(hp1$LSOA11CD,1,1) == "S",]

  names(hp2) = c("LSOA11CD","houseprice_2020","houseprice_2021")

  hpall = dplyr::left_join(hp1, hp2, by = "LSOA11CD")

  hp_S = hpall[hpall$LSOA11CD %in% lsoa_11_21_tools$lookup_split$LSOA11CD,]
  hp_M = hpall[hpall$LSOA11CD %in% lsoa_11_21_tools$lookup_merge$LSOA11CD,]
  hp_U = hpall[hpall$LSOA11CD %in% lsoa_11_21_tools$lookup_unchanged$LSOA11CD,]

  #Unchanged
  hp_U = dplyr::left_join(hp_U, lsoa_11_21_tools$lookup_unchanged, by = "LSOA11CD")

  # Merge
  hp_M = dplyr::left_join(hp_M, lsoa_11_21_tools$lookup_merge, by = "LSOA11CD")
  hp_M = dplyr::group_by(hp_M, LSOA21CD)
  hp_M = dplyr::summarise(hp_M,
                          houseprice_1995 = mean(houseprice_1995, na.rm = TRUE),
                          houseprice_1996 = mean(houseprice_1996, na.rm = TRUE),
                          houseprice_1997 = mean(houseprice_1997, na.rm = TRUE),
                          houseprice_1998 = mean(houseprice_1998, na.rm = TRUE),
                          houseprice_1999 = mean(houseprice_1999, na.rm = TRUE),
                          houseprice_2000 = mean(houseprice_2000, na.rm = TRUE),
                          houseprice_2001 = mean(houseprice_2001, na.rm = TRUE),
                          houseprice_2002 = mean(houseprice_2002, na.rm = TRUE),
                          houseprice_2003 = mean(houseprice_2003, na.rm = TRUE),
                          houseprice_2004 = mean(houseprice_2004, na.rm = TRUE),
                          houseprice_2005 = mean(houseprice_2005, na.rm = TRUE),
                          houseprice_2006 = mean(houseprice_2006, na.rm = TRUE),
                          houseprice_2007 = mean(houseprice_2007, na.rm = TRUE),
                          houseprice_2008 = mean(houseprice_2008, na.rm = TRUE),
                          houseprice_2009 = mean(houseprice_2009, na.rm = TRUE),
                          houseprice_2010 = mean(houseprice_2010, na.rm = TRUE),
                          houseprice_2011 = mean(houseprice_2011, na.rm = TRUE),
                          houseprice_2012 = mean(houseprice_2012, na.rm = TRUE),
                          houseprice_2013 = mean(houseprice_2013, na.rm = TRUE),
                          houseprice_2014 = mean(houseprice_2014, na.rm = TRUE),
                          houseprice_2015 = mean(houseprice_2015, na.rm = TRUE),
                          houseprice_2016 = mean(houseprice_2016, na.rm = TRUE),
                          houseprice_2017 = mean(houseprice_2017, na.rm = TRUE),
                          houseprice_2018 = mean(houseprice_2018, na.rm = TRUE),
                          houseprice_2020 = mean(houseprice_2020, na.rm = TRUE),
                          houseprice_2021 = mean(houseprice_2021, na.rm = TRUE)
  )
  hp_M = dplyr::ungroup(hp_M)

  #Split - Only average so no clear way to split
  lookup_split = lsoa_11_21_tools$lookup_split
  lookup_split = lookup_split[,c("LSOA21CD","LSOA11CD")]
  lookup_split = lookup_split[!duplicated(lookup_split$LSOA21CD),]

  hp_S = dplyr::left_join(lookup_split, hp_S,
                                            by = "LSOA11CD", relationship = "many-to-many")
  hp_S = as.data.frame(hp_S)

  nms = c("LSOA21CD",paste0("houseprice_",c(1995:2018,2020,2021)))

  hp_S = hp_S[,nms]
  hp_M = hp_M[,nms]
  hp_U = hp_U[,nms]


  final = rbind(hp_S, hp_M, hp_U)

  final = tidyr::pivot_longer(final,
                              cols = names(final)[grepl("houseprice",names(final))],
                              names_sep = "_",
                              names_to = c(".value","year"))

  final

}


load_house_transactions = function(path = file.path(parameters$path_data,"house_price_age"),
                             lsoa_11_21_tools){
  hp1 = read.csv(file.path(path,"Transaction_Count_Quarterly.csv"))


  #Standardised to Q1 as 2020/2021 data is for March

  hp1 = hp1[,c("lsoa_cd",names(hp1)[grepl("Q1",names(hp1))])]
  names(hp1) = c("LSOA11CD",paste0("housetransactions_",1995:2018))
  hp1 = hp1[!is.na(hp1$LSOA11CD),]
  hp1 = hp1[substr(hp1$LSOA11CD,1,1) != "S",]

  hp1 = tidyr::pivot_longer(hp1,
                            cols = names(hp1)[grepl("housetransactions_",names(hp1))],
                            names_sep = "_",
                            names_to = c(".value","year"))

  hp1 = hp1[hp1$year > 2002,]


  hp_S = hp1[hp1$LSOA11CD %in% lsoa_11_21_tools$lookup_split$LSOA11CD,]
  hp_M = hp1[hp1$LSOA11CD %in% lsoa_11_21_tools$lookup_merge$LSOA11CD,]
  hp_U = hp1[hp1$LSOA11CD %in% lsoa_11_21_tools$lookup_unchanged$LSOA11CD,]

  #Unchanged
  hp_U = dplyr::left_join(hp_U, lsoa_11_21_tools$lookup_unchanged, by = "LSOA11CD")

  # MERGE
  hp_M = dplyr::left_join(hp_M, lsoa_11_21_tools$lookup_merge, by = "LSOA11CD")
  hp_M = dplyr::group_by(hp_M, LSOA21CD, year)
  hp_M = dplyr::summarise(hp_M,
                          housetransactions = sum(housetransactions, na.rm = TRUE))
  hp_M = dplyr::ungroup(hp_M)

  #Split
  lookup_split = lsoa_11_21_tools$lookup_split
  lookup_split = lookup_split[,c("LSOA21CD","LSOA11CD","year","household_ratio")]
  hp_S$year = as.numeric(hp_S$year)

  hp_S = dplyr::left_join(lookup_split,
                          hp_S,
                          by = c("LSOA11CD","year"),
                          relationship = "many-to-many")
  hp_S$housetransactions[is.na(hp_S$housetransactions)] = 0

  hp_S$housetransactions = hp_S$housetransactions * hp_S$household_ratio

  nms = c("LSOA21CD","year","housetransactions")

  hp_S = hp_S[,nms]
  hp_M = hp_M[,nms]
  hp_U = hp_U[,nms]


  final = rbind(hp_S, hp_M, hp_U)
  final

}
