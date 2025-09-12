load_cenus11_households = function(path = "../inputdata/population/cenus2011_QS402UK_LSOA_Households_AcommodationType.csv"){
  cenus11 = read.csv(path, skip = 7)
  cenus11 = cenus11[,2:3]
  names(cenus11) = c("LSOA11CD","households_total")
  cenus11 = cenus11[!is.na(cenus11$households_total),]
  cenus11$year = 2011
  cenus11
}


# Estimate the population and number of households for Census 2021 boundaries even when there have been changes.
# Number of households are a function of adults (20+) / dwelling and number of dwellings

#TODO: Check odd LSOA resutls E01033274 E01024150 E01024301 E01016129 E01024504 W01001971

extrapolate_population_households = function(households_cenus11,
                                             households_cenus21,
                                             lookup_lsoa_2011_21,
                                             dwellings_tax_band,
                                             population_2002_2020,
                                             population_2021,
                                             population_2022
                                             ){


  names(dwellings_tax_band)[names(dwellings_tax_band) == "ecode"] = "LSOA21CD" #TODO: Move this upstream
  dwellings_tax_band = dwellings_tax_band[order(dwellings_tax_band$LSOA21CD, dwellings_tax_band$year),]

  names(households_cenus11)[names(households_cenus11) == "LSOA11CD"] = "LSOA"
  names(households_cenus21)[names(households_cenus21) == "LSOA21"] = "LSOA"

  households_cenus11 = households_cenus11[order(households_cenus11$LSOA),]
  households_cenus21 = households_cenus21[order(households_cenus21$LSOA),]

  # Standardise Age Data
  population_2002_2020$`85+` = rowSums(population_2002_2020[,c("85-89","90+")])
  population_2002_2020$`85-89` = NULL
  population_2002_2020$`90+` = NULL

  adult_bands = c("20-24","25-29","30-34","35-39","40-44","45-49","50-54",
                  "55-59","60-64","65-69","70-74","75-79","80-84","85+")

  population_2002_2020$adults = rowSums(population_2002_2020[,adult_bands])


  population_2022$`85+` = rowSums(population_2022[,c("85-89","90+")])
  population_2022$`85-89` = NULL
  population_2022$`90+` = NULL
  population_2022$year = 2022

  population_2022$adults = rowSums(population_2022[,adult_bands])

  population_2021$adults = rowSums(population_2021[,adult_bands])

  names(population_2021)[names(population_2021) == "LSOA21"] = "LSOA21CD"


  # Step 1: Unchanged LSOA
  lookup_U = lookup_lsoa_2011_21[lookup_lsoa_2011_21$CHGIND == "U",]

  # Complex change replace with unchanged as changes are small
  lookup_X = lookup_lsoa_2011_21[lookup_lsoa_2011_21$CHGIND == "X",]
  lookup_X = lookup_X[!(lookup_X$LSOA11CD == "E01027506" & lookup_X$LSOA21CD == "E01035624"),]
  lookup_X = lookup_X[!(lookup_X$LSOA11CD == "E01008187" & lookup_X$LSOA21CD == "E01035637"),]

  lookup_X = lookup_X[!(lookup_X$LSOA11CD == "E01023964" & lookup_X$LSOA21CD == "E01035581"),]
  lookup_X = lookup_X[!(lookup_X$LSOA11CD == "E01023679" & lookup_X$LSOA21CD == "E01035608"),]

  lookup_X = lookup_X[!(lookup_X$LSOA11CD == "E01023508" & lookup_X$LSOA21CD == "E01035582"),]
  lookup_X = lookup_X[!(lookup_X$LSOA11CD == "E01023768" & lookup_X$LSOA21CD == "E01035609"),]

  lookup_X$CHGIND = "U"
  lookup_U = rbind(lookup_U, lookup_X)
  lookup_U = lookup_U[,c("LSOA11CD","LSOA21CD")]

  # Update households
  households_cenus11 = dplyr::left_join(households_cenus11, lookup_U, by = c("LSOA" = "LSOA11CD"))
  households_cenus11$LSOA21CD = ifelse(is.na(households_cenus11$LSOA21CD),
                                       households_cenus11$LSOA,
                                       households_cenus11$LSOA21CD)
  households_cenus11$LSOA = households_cenus11$LSOA21CD
  households_cenus11$LSOA21CD = NULL

  pop_U_old = population_2002_2020[population_2002_2020$LSOA11CD %in% lookup_U$LSOA11CD,]
  pop_U_old = dplyr::left_join(pop_U_old, lookup_U, by = "LSOA11CD")
  pop_U_old$LSOA11CD = NULL

  lsoa_U = unique(c(lookup_U$LSOA21CD, lookup_X$LSOA11CD))


  pop_U_21 = population_2021[population_2021$LSOA21CD %in% lookup_U$LSOA21CD,]
  pop_U_22 = population_2022[population_2022$LSOA21CD %in% lookup_U$LSOA21CD,]

  pop_U = rbind(pop_U_old, pop_U_21, pop_U_22)
  pop_U = pop_U[order(pop_U$LSOA21CD, pop_U$year),]


  d_U = dwellings_tax_band[dwellings_tax_band$LSOA21CD %in% lsoa_U,]
  hh21_U = households_cenus21[households_cenus21$LSOA %in% lsoa_U,]
  hh11_U = households_cenus11[households_cenus11$LSOA %in% lsoa_U,]

  hh21_U = hh21_U[order(hh21_U$LSOA),]
  hh11_U = hh11_U[order(hh11_U$LSOA),]

  pop_U = dplyr::group_split(pop_U, LSOA21CD)
  d_U = dplyr::group_split(d_U, LSOA21CD)
  hh21_U = dplyr::group_split(hh21_U, LSOA)
  hh11_U = dplyr::group_split(hh11_U, LSOA)

  res_U = purrr::pmap(.l = list(p = pop_U, d = d_U, hh21 = hh21_U, hh11 = hh11_U),
                    .f = extrapolate_households, .progress = TRUE)
  res_U = dplyr::bind_rows(res_U)

  # Step 2: Merged LSOA
  lookup_M = lookup_lsoa_2011_21[lookup_lsoa_2011_21$CHGIND == "M",]
  lookup_M = lookup_M[,c("LSOA11CD","LSOA21CD")]

  pop_M_old = population_2002_2020[population_2002_2020$LSOA11CD %in% lookup_M$LSOA11CD,]
  pop_M_old = dplyr::left_join(pop_M_old, lookup_M, by = "LSOA11CD")
  pop_M_old = dplyr::group_by(pop_M_old, LSOA21CD, year)
  pop_M_old = dplyr::summarise(pop_M_old,
                               all_ages = sum(all_ages),
                               `0-4` = sum(`0-4`),
                               `5-9` = sum(`5-9`),
                               `10-14` = sum(`10-14`),
                               `15-19` = sum(`15-19`),
                               `20-24` = sum(`20-24`),
                               `25-29` = sum(`25-29`),
                               `30-34` = sum(`30-34`),
                               `35-39` = sum(`35-39`),
                               `40-44` = sum(`40-44`),
                               `45-49` = sum(`45-49`),
                               `50-54` = sum(`50-54`),
                               `55-59` = sum(`55-59`),
                               `60-64` = sum(`60-64`),
                               `65-69` = sum(`65-69`),
                               `70-74` = sum(`70-74`),
                               `75-79` = sum(`75-79`),
                               `80-84` = sum(`80-84`),
                               `85+`  = sum(`85+`),
                               adults = sum(adults))
  pop_M_old = dplyr::ungroup(pop_M_old)

  pop_M_21 = population_2021[population_2021$LSOA21CD %in% lookup_M$LSOA21CD,]
  pop_M_22 = population_2022[population_2022$LSOA21CD %in% lookup_M$LSOA21CD,]

  pop_M = rbind(pop_M_old, pop_M_21, pop_M_22)
  pop_M = pop_M[order(pop_M$LSOA21CD, pop_M$year),]


  d_M = dwellings_tax_band[dwellings_tax_band$LSOA21CD %in% lookup_M$LSOA21CD,]
  hh21_M = households_cenus21[households_cenus21$LSOA %in% lookup_M$LSOA21CD,]
  hh11_M = households_cenus11[households_cenus11$LSOA %in% lookup_M$LSOA11CD,]

  hh11_M = dplyr::left_join(hh11_M, lookup_M, by = c("LSOA" = "LSOA11CD"))

  hh11_M$LSOA = hh11_M$LSOA21CD

  pop_M = dplyr::group_split(pop_M, LSOA21CD)
  d_M = dplyr::group_split(d_M, LSOA21CD)
  hh21_M = dplyr::group_split(hh21_M, LSOA)
  hh11_M = dplyr::group_split(hh11_M, LSOA21CD)

  res_M = purrr::pmap(.l = list(p = pop_M, d = d_M, hh21 = hh21_M, hh11 = hh11_M),
                      .f = extrapolate_households, .progress = TRUE)
  res_M = dplyr::bind_rows(res_M)

  # Step 3: Split LSOA
  lookup_S = lookup_lsoa_2011_21[lookup_lsoa_2011_21$CHGIND == "S",]
  lookup_S = lookup_S[,c("LSOA11CD","LSOA21CD")]

  pop_S_old = population_2002_2020[population_2002_2020$LSOA11CD %in% lookup_S$LSOA11CD,]
  #pop_S_old = dplyr::left_join(pop_S_old, lookup_S, by = "LSOA11CD")

  pop_S_21 = population_2021[population_2021$LSOA21CD %in% lookup_S$LSOA21CD,]
  pop_S_22 = population_2022[population_2022$LSOA21CD %in% lookup_S$LSOA21CD,]

  pop_S_new = rbind(pop_S_21, pop_S_22)
  pop_S_new = dplyr::left_join(pop_S_new, lookup_S, by = "LSOA21CD")

  d_S = dwellings_tax_band[dwellings_tax_band$LSOA21CD %in% lookup_S$LSOA21CD,]
  d_S = dplyr::left_join(d_S, lookup_S, by = "LSOA21CD")
  hh21_S = households_cenus21[households_cenus21$LSOA %in% lookup_S$LSOA21CD,]
  hh11_S = households_cenus11[households_cenus11$LSOA %in% lookup_S$LSOA11CD,]

  hh21_S = dplyr::left_join(hh21_S, lookup_S, by = c("LSOA" = "LSOA21CD"))

  pop_S_old = dplyr::group_split(pop_S_old, LSOA11CD)
  pop_S_new = dplyr::group_split(pop_S_new, LSOA11CD)
  d_S = dplyr::group_split(d_S, LSOA11CD)
  hh21_S = dplyr::group_split(hh21_S, LSOA11CD)
  hh11_S = dplyr::group_split(hh11_S, LSOA)

  res_S = purrr::pmap(.l = list(p = pop_S_old, d = d_S, hh21 = hh21_S, hh11 = hh11_S, pn = pop_S_new),
                      .f = extrapolate_households, .progress = TRUE)
  res_S = dplyr::bind_rows(res_S)

  res_final = dplyr::bind_rows(res_U, res_M, res_S)

  res_final

}

# p = pop_U[pop_U$LSOA21CD == "E01000001",]
# d = dwellings_tax_band[dwellings_tax_band$LSOA21CD == "E01000001",]
# hh21 = households_cenus21[households_cenus21$LSOA == "E01000001",]
# hh11 = households_cenus11[households_cenus11$LSOA == "E01000001",]
# list(p = pop_U, d = d_U, hh21 = hh21_U, hh11 = hh11_U)
# foo = sapply(pop_U, function(x){x$LSOA21CD[1]})
# p = pop_U[foo == "W01001971"][[1]]
# #pn = pop_S_new[[1]]
# d = d_U[foo == "W01001971"][[1]]
# hh21 = hh21_U[foo == "W01001971"][[1]]
# hh11 = hh11_U[foo == "W01001971"][[1]]

# p = pop_M[[1]]
# d = d_M[[1]]
# hh21 = hh21_M[[1]]
# hh11 = hh11_M[[1]]

extrapolate_households = function(p, d, hh21, hh11, pn = NULL){
  # Initial check
  if(is.null(pn)){
    if(any(unique(p$LSOA21CD) != unique(d$LSOA21CD))){
      stop("LSOAs don't match")
    }
    if(!all(unique(c(hh21$LSOA, hh11$LSOA)) %in% unique(d$LSOA21CD))){
      stop("LSOAs don't match")
    }

    if(nrow(hh21) == 1 & nrow(hh11) == 1){
      # Unchanged Simple Case: Do nothing
    } else if (nrow(hh21) == 1 & nrow(hh11) > 1){
      # Merged: Combine household counts then proceed
      hh11 = dplyr::group_by(hh11, LSOA21CD, year)
      hh11 = dplyr::summarise(hh11, households_total = sum(households_total))
      names(hh11)[1] = "LSOA"
    } else {
      stop("Unknown type of 2011 -2021 change")
    }

    hh = rbind(hh11, hh21)
    hh = dplyr::left_join(hh, p[,c("year","adults")], by = c("year"))
    hh = dplyr::left_join(hh, d[,c("year","all_properties")], by = c("year"))
    hh$adults_per_household = hh$adults / hh$households_total

    m = lm(adults_per_household ~ year, hh)
    aph = data.frame(year = 2002:2022)
    aph$adults_per_household = predict(m, newdata = aph)
    aph$adults_per_household = ifelse(aph$adults_per_household < 1,1,aph$adults_per_household)
    p = dplyr::left_join(p, aph, by = "year")
    p$households_est = round(p$adults / p$adults_per_household)
    p = dplyr::left_join(p, d[,c("year","all_properties")], by = "year")

    # plot(p$all_ages, type = "l", col = "red", ylim = c(0,max(p$all_ages)))
    # lines(p$households_est, col = "blue")
    # lines(p$all_properties, col = "green")

    return(p)

  } else {
    #TODO Some Checks go here

    # Split
    hh21$LSOA11CD = NULL
    hh = rbind(hh11, hh21)
    hh = dplyr::left_join(hh, p[,c("year","adults")], by = c("year"))
    hh = dplyr::left_join(hh, pn[,c("year","adults","LSOA21CD")], by = c("year", "LSOA" = "LSOA21CD"))
    hh$adults = rowSums(hh[,c("adults.x","adults.y")], na.rm = TRUE)
    hh = hh[,c("LSOA","households_total","year","adults")]
    hh = dplyr::left_join(hh, d[,c("year","all_properties","LSOA21CD")], by = c("year", "LSOA" = "LSOA21CD"))
    hh$all_properties[hh$year == 2011] = sum(d$all_properties[d$year == 2011])
    hh$adults_per_household = hh$adults / hh$households_total


    d2 = dplyr::left_join(d[d$year < 2021,], hh[,c("LSOA","adults_per_household")],
                          by = c("LSOA21CD" = "LSOA"))
    d2 = dplyr::group_by(d2, year)
    d2 = dplyr::mutate(d2,
          household_share = all_properties * adults_per_household  / sum(all_properties * adults_per_household))
    d2 = dplyr::ungroup(d2)
    d2$household_share = ifelse(is.nan(d2$household_share),0,d2$household_share)

    d2 = dplyr::left_join(d2[d2$year < 2021,], p, by = c("year" = "year"))

    nms = c("all_ages","0-4",
            "5-9","10-14","15-19","20-24","25-29","30-34",
            "35-39","40-44","45-49","50-54","55-59","60-64",
            "65-69","70-74","75-79","80-84","85+","adults")
    for(i in nms){
      d2[[i]] = round(d2[[i]] * d2$household_share,2)
    }

    hh_old = hh[hh$year == 2011,]
    hh_new = hh[hh$year == 2021,]

    hh_list  = list()
    for(i in seq_len(nrow(hh_new))){
      hh_list[[i]] = rbind(hh_old, hh_new[i,])
    }

    aph_list = list()
    for(i in seq_len(length(hh_list))){
      m = lm(adults_per_household ~ year, hh_list[[i]])
      aph = data.frame(year = 2002:2020)
      aph$adults_per_household = predict(m, newdata = aph)
      aph$adults_per_household = ifelse(aph$adults_per_household < 1,1,aph$adults_per_household)
      aph$LSOA21CD = hh_list[[i]]$LSOA[hh_list[[i]]$year == 2021]
      aph_list[[i]] = aph
    }

    aph = dplyr::bind_rows(aph_list)
    d2$adults_per_household = NULL
    aph = dplyr::left_join(aph, d2, by = c("year" = "year", "LSOA21CD" = "LSOA21CD"))
    aph$households_est =  round(aph$adults / aph$adults_per_household)

    aph_new = dplyr::left_join(pn, hh[,c("LSOA","adults_per_household")], by = c("LSOA21CD" = "LSOA"))
    aph_new$households_est = aph_new$adults / aph_new$adults_per_household
    aph_new$LSOA11CD = NULL
    aph = aph[,names(aph_new)]
    aph = rbind(aph, aph_new)

    aph = dplyr::left_join(aph, d[,c("LSOA21CD","year","all_properties")], by = c("year","LSOA21CD"))

    aph = aph[order(aph$LSOA21CD, aph$year),]

    if(anyNA(aph$adults)){
      stop("NA population")
    }

    return(aph)

    # ggplot(aph) +
    #   geom_line(aes(y = all_ages, x = year, group = LSOA21CD), colour = "red") +
    #   geom_line(aes(y = adults, x = year, group = LSOA21CD), colour = "blue") +
    #   geom_line(aes(y = households_est, x = year, group = LSOA21CD), colour = "green") +
    #   scale_x_continuous(breaks = seq(2002,2022,2), expand = c(0,1)) +
    #   scale_y_continuous(
    #     expand = c(0,0),
    #     limits = c(0, NA)
    #   ) +
    #   geom_point(data = hh, aes(x = year, y = households_total),stat="identity") +
    #   geom_line(data = d[d$year > 2001,], aes(y = all_properties, x = year, group = LSOA21CD), colour = "black")
  }

}



