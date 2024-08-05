load_house_prices = function(path = file.path(parameters$path_data,"house_price_age"),
                             lsoa_11_21_tools){
  hp1 = read.csv(file.path(path,"Median_Prices_Quarterly.csv"))
  hp2 = read.csv(file.path(path,"hpssa202103.csv"))

  #Standerdies to Q1 as 2020/2021 data is for March

  hp1 = hp1[,c("lsoa_cd",names(hp1)[grepl("Q1",names(hp1))])]
  names(hp1) = c("LSOA11CD",paste0("houseprice_",1995:2018))
  hp1 = hp1[!is.na(hp1$LSOA11CD),]

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

  #Split
  hp_S = dplyr::left_join(lsoa_11_21_tools$lookup_split, hp_S,
                                            by = "LSOA11CD", relationship = "many-to-many")
  hp_S = as.data.frame(hp_S)

  nms = c("LSOA21CD",paste0("houseprice_",c(1995:2018,2020,2021)))

  hp_S = hp_S[,nms]
  hp_M = hp_M[,nms]
  hp_U = hp_U[,nms]


  final = rbind(hp_S, hp_M, hp_U)
  final

}


load_house_transactions = function(path = file.path(parameters$path_data,"house_price_age"),
                             lsoa_11_21_tools){
  hp1 = read.csv(file.path(path,"Transaction_Count_Quarterly.csv"))


  #Standardised to Q1 as 2020/2021 data is for March

  hp1 = hp1[,c("lsoa_cd",names(hp1)[grepl("Q1",names(hp1))])]
  names(hp1) = c("LSOA11CD",paste0("housetransactions_",1995:2018))
  hp1 = hp1[!is.na(hp1$LSOA11CD),]


  hp_S = hp1[hp1$LSOA11CD %in% lsoa_11_21_tools$lookup_split$LSOA11CD,]
  hp_M = hp1[hp1$LSOA11CD %in% lsoa_11_21_tools$lookup_merge$LSOA11CD,]
  hp_U = hp1[hp1$LSOA11CD %in% lsoa_11_21_tools$lookup_unchanged$LSOA11CD,]

  #Unchanged
  hp_U = dplyr::left_join(hp_U, lsoa_11_21_tools$lookup_unchanged, by = "LSOA11CD")

  # MERGE
  hp_M = dplyr::left_join(hp_M, lsoa_11_21_tools$lookup_merge, by = "LSOA11CD")
  hp_M = dplyr::group_by(hp_M, LSOA21CD)
  hp_M = dplyr::summarise(hp_M,
                          housetransactions_1995 = sum(housetransactions_1995, na.rm = TRUE),
                          housetransactions_1996 = sum(housetransactions_1996, na.rm = TRUE),
                          housetransactions_1997 = sum(housetransactions_1997, na.rm = TRUE),
                          housetransactions_1998 = sum(housetransactions_1998, na.rm = TRUE),
                          housetransactions_1999 = sum(housetransactions_1999, na.rm = TRUE),
                          housetransactions_2000 = sum(housetransactions_2000, na.rm = TRUE),
                          housetransactions_2001 = sum(housetransactions_2001, na.rm = TRUE),
                          housetransactions_2002 = sum(housetransactions_2002, na.rm = TRUE),
                          housetransactions_2003 = sum(housetransactions_2003, na.rm = TRUE),
                          housetransactions_2004 = sum(housetransactions_2004, na.rm = TRUE),
                          housetransactions_2005 = sum(housetransactions_2005, na.rm = TRUE),
                          housetransactions_2006 = sum(housetransactions_2006, na.rm = TRUE),
                          housetransactions_2007 = sum(housetransactions_2007, na.rm = TRUE),
                          housetransactions_2008 = sum(housetransactions_2008, na.rm = TRUE),
                          housetransactions_2009 = sum(housetransactions_2009, na.rm = TRUE),
                          housetransactions_2010 = sum(housetransactions_2010, na.rm = TRUE),
                          housetransactions_2011 = sum(housetransactions_2011, na.rm = TRUE),
                          housetransactions_2012 = sum(housetransactions_2012, na.rm = TRUE),
                          housetransactions_2013 = sum(housetransactions_2013, na.rm = TRUE),
                          housetransactions_2014 = sum(housetransactions_2014, na.rm = TRUE),
                          housetransactions_2015 = sum(housetransactions_2015, na.rm = TRUE),
                          housetransactions_2016 = sum(housetransactions_2016, na.rm = TRUE),
                          housetransactions_2017 = sum(housetransactions_2017, na.rm = TRUE),
                          housetransactions_2018 = sum(housetransactions_2018, na.rm = TRUE),
  )


  hp_M = dplyr::ungroup(hp_M)

  #Split
  hp_S = dplyr::left_join(lsoa_11_21_tools$lookup_split, hp_S,
                                            by = "LSOA11CD", relationship = "many-to-many")
  hp_S = as.data.frame(hp_S)
  for(i in 4:27){
    hp_S[i] = round(hp_S[,i ,drop = TRUE] * hp_S$pop_ratio,1)
  }

  nms = c("LSOA21CD",paste0("housetransactions_",c(1995:2018)))

  hp_S = hp_S[,nms]
  hp_M = hp_M[,nms]
  hp_U = hp_U[,nms]


  final = rbind(hp_S, hp_M, hp_U)
  final

}
