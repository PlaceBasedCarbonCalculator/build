library(targets)
library(ggplot2)
library(dplyr)
tar_load(population)
tar_load(dwellings_tax_band)
tar_load(parameters)
tar_load(households_cenus21)
tar_load(lsoa_11_21_tools)
tar_load(lookup_lsoa_2011_21)
tar_load(population_2002_2020)

cenus11 = read.csv("../inputdata/population/cenus2011_QS402UK_LSOA_Households_AcommodationType.csv", skip = 7)
cenus11 = cenus11[,2:3]
names(cenus11) = c("LSOA11CD","households_2011")

dwellings_tax_band = dwellings_tax_band[dwellings_tax_band$year > 2001,]
dwellings_tax_band = dwellings_tax_band[,c("ecode","year","all_properties")]

joined = dplyr::left_join(population, dwellings_tax_band, by = c("year" = "year", "LSOA21CD" = "ecode"))
joined = joined[substr(joined$LSOA21CD,1,1) %in% c("E","W"),]
joined$adults = rowSums(joined[,c("20-24","25-29","30-34","35-39","40-44","45-49","50-54",
                                  "55-59","60-64","65-69","70-74","75-79","80-84","85+")])

population_2002_2020$adults = rowSums(population_2002_2020[,c("20-24","25-29","30-34","35-39","40-44","45-49","50-54",
                                  "55-59","60-64","65-69","70-74","75-79","80-84","85-89","90+")])
population_2002_2020$`85+` = rowSums(population_2002_2020[,c("85-89","90+")])
population_2002_2020$`85-89` = NULL
population_2002_2020$`95+` = NULL

# Unchanged
LSOA = lsoa_11_21_tools$lookup_unchanged$LSOA21CD[10]
#LSOA = "E01033173"

if(TRUE){
  foo = joined[joined$LSOA21CD == LSOA,]
  foo2 = cenus11[cenus11$LSOA11CD == LSOA,]
  foo3 = households_cenus21[households_cenus21$LSOA21 == LSOA,]
  names(foo3) = c("year","LSOA","households" )
  if(nrow(foo2) > 0){
    foo2$year = 2011
    names(foo2) = c("LSOA","households","year" )
    foo4 = rbind(foo2, foo3)
  } else {
    foo4 = foo3
  }

  # Unchanged: interpolate adult:household ratio between 2011 and 2021
  foo4 = dplyr::left_join(foo4, foo[,c("year","adults","all_properties")], by = "year")
  foo4$adults_per_household = foo4$adults / foo4$households

  m = lm(adults_per_household ~ year, foo4)
  aph = data.frame(year = 2002:2022)
  aph$adults_per_household = predict(m, newdata = aph)

  plot(aph$year, aph$adults_per_household, ylim = c(0,2))
  points(foo4$year, foo4$adults_per_household, col = "red")
  foo$households_est =  round(foo$adults / aph$adults_per_household)

  ggplot(foo, aes(x = year)) +
    geom_line(aes(y = all_ages), colour = "red") +
    geom_line(aes(y = all_properties)) +
    geom_line(aes(y = adults), colour = "blue") +
    geom_line(aes(y = households_est), colour = "green") +
    scale_x_continuous(breaks = seq(2002,2022,2), expand = c(0,1)) +
    scale_y_continuous(
      expand = c(0,0),
      limits = c(0, NA)
    ) +
    geom_point(data = foo4, aes(x = year, y = households),stat="identity")
}


# Merged
LSOA = lsoa_11_21_tools$lookup_merge$LSOA21CD[10]
LSOA_old = lsoa_11_21_tools$lookup_merge$LSOA11CD[lsoa_11_21_tools$lookup_merge$LSOA21CD == LSOA]

if(TRUE){
  foo = joined[joined$LSOA21CD == LSOA,]
  foo_old = population_2002_2020[population_2002_2020$LSOA11CD %in% LSOA_old,]
  foo_old = foo_old[,c("year","LSOA11CD","adults","all_ages")]
  foo_old2 = dplyr::group_by(foo_old, year)
  foo_old2 = dplyr::summarise(foo_old2, adults = sum(adults), all_ages = sum(all_ages))

  foo_new = foo[foo$year > 2020,]

  foo2 = cenus11[cenus11$LSOA11CD %in% LSOA_old,]
  build = dwellings_tax_band[dwellings_tax_band$ecode %in% LSOA,]

  foo3 = households_cenus21[households_cenus21$LSOA21 == LSOA,]
  names(foo3) = c("year","LSOA","households" )
  if(nrow(foo2) > 0){
    foo2$year = 2011
    names(foo2) = c("LSOA","households","year" )
    foo4 = rbind(foo2, foo3)
  } else {
    foo4 = foo3
  }

  foo4 = dplyr::group_by(foo4, year)
  foo4 = dplyr::summarise(foo4, households = sum(households))

  foo4 = dplyr::left_join(foo4, foo[,c("year","adults","all_properties")], by = "year")
  foo4$adults_per_household = foo4$adults / foo4$households

  m = lm(adults_per_household ~ year, foo4)
  aph = data.frame(year = 2002:2022)
  aph$adults_per_household = predict(m, newdata = aph)

  plot(aph$year, aph$adults_per_household, ylim = c(0,2))
  points(foo4$year, foo4$adults_per_household, col = "red")
  foo$households_est =  round(foo$adults / aph$adults_per_household)

  ggplot(foo_old2) +
    geom_line(aes(y = all_ages, x = year), colour = "red") +
    geom_line(aes(y = adults, x = year), colour = "blue") +
    scale_x_continuous(breaks = seq(2002,2022,2), expand = c(0,1)) +
    scale_y_continuous(
      expand = c(0,0),
      limits = c(0, NA)
    ) +
    geom_point(data = foo4, aes(x = year, y = households),stat="identity") +
    geom_line(data = build, aes(y = all_properties, x = year), colour = "black") +
    geom_line(data = foo_new, aes(y = all_ages, x = year), colour = "red") +
    geom_line(data = foo_new, aes(y = adults, x = year), colour = "blue") +
    geom_line(data = foo, aes(y = households_est, x = year), colour = "green")




}

# Split
LSOA = lsoa_11_21_tools$lookup_split$LSOA21CD[1]
LSOA_old = lsoa_11_21_tools$lookup_split$LSOA11CD[lsoa_11_21_tools$lookup_split$LSOA21CD == LSOA]
LSOA_new = lsoa_11_21_tools$lookup_split$LSOA21CD[lsoa_11_21_tools$lookup_split$LSOA11CD == LSOA_old]

if(TRUE){
  foo = joined[joined$LSOA21CD %in% LSOA_new,]
  foo_old = population_2002_2020[population_2002_2020$LSOA11CD %in% LSOA_old,]
  #foo_old = foo_old[,c("year","LSOA11CD","adults","all_ages")]

  # ggplot(foo) +
  #   geom_line(aes(y = all_ages, x = year, group = LSOA21CD))

  foo_new = foo[foo$year > 2020,]


  foo2 = cenus11[cenus11$LSOA11CD %in% LSOA_old,]
  build = dwellings_tax_band[dwellings_tax_band$ecode %in% LSOA_new,]

  foo3 = households_cenus21[households_cenus21$LSOA21 %in% LSOA_new,]
  names(foo3) = c("year","LSOA","households" )
  if(nrow(foo2) > 0){
    foo2$year = 2011
    names(foo2) = c("LSOA","households","year" )
    foo4 = rbind(foo2, foo3)
  } else {
    foo4 = foo3
  }



  foo4 = dplyr::left_join(foo4, foo[,c("year","LSOA21CD","adults","all_properties")], by = c("year" = "year","LSOA" = "LSOA21CD"))
  foo4$adults[foo4$year == 2011] = foo_old$adults[foo_old$year == 2011]
  foo4$all_properties[foo4$year == 2011] = sum(build$all_properties[build$year == 2011])
  foo4$adults_per_household = foo4$adults / foo4$all_properties

  # Back project population and households in proportion to property
  # TODO: account for difference in adults per househodl for each
  build2 = dplyr::left_join(build, foo4[,c("LSOA","adults_per_household")], by =c("ecode" = "LSOA"))

  build2 = dplyr::group_by(build2, year) %>%
            dplyr::mutate(building_share = all_properties / sum(all_properties),
                          household_share = all_properties * adults_per_household  / sum(all_properties * adults_per_household),
                          ) %>%
            dplyr::ungroup()


  build2 = left_join(build2[build2$year < 2021,], foo_old, by = c("year" = "year"))

  nms = c("all_ages","0-4",
          "5-9","10-14","15-19","20-24","25-29","30-34",
          "35-39","40-44","45-49","50-54","55-59","60-64",
          "65-69","70-74","75-79","80-84","85+","adults")
  for(i in nms){
    build2[[i]] = round(build2[[i]] * build2$household_share,2)
  }

  foo4_old = foo4[foo4$year == 2011,]
  foo4_new = foo4[foo4$year == 2021,]

  foo4_list  = list()
  for(i in seq_len(nrow(foo4_new))){
    foo4_list[[i]] = rbind(foo4_old, foo4_new[i,])
  }

  m_list = list()
  for(i in seq_len(length(foo4_list))){
    m = lm(adults_per_household ~ year, foo4_list[[i]])
    aph = data.frame(year = 2002:2020)
    aph$adults_per_household = predict(m, newdata = aph)
    aph$LSOA21CD = foo4_list[[i]]$LSOA[foo4_list[[i]]$year == 2021]
    m_list[[i]] = aph
  }

  aph = dplyr::bind_rows(m_list)
  aph$adults_per_household = NULL
  aph = dplyr::left_join(aph, build2, by = c("year" = "year", "LSOA21CD" = "ecode"))
  aph$households_est =  round(aph$adults / aph$adults_per_household)

  #TODO: households 2021-2022
  foo_new2 = dplyr::left_join(foo_new, foo4[,c("LSOA","adults_per_household")], by = c("LSOA21CD" = "LSOA"))
  foo_new2$households_est = foo_new2$adults / foo_new2$adults_per_household

  #TODO: fix this missalingment in ag bands
  aph$`90+` = NULL
  aph$building_share = NULL
  aph$household_share = NULL
  aph$LSOA11CD = NULL

  aph2 = rbind(aph, foo_new2)
  aph2 = aph2[order(aph2$LSOA21CD),]

  aph3 = aph2[aph2$LSOA21CD == "E01033780",]

  ggplot(aph2) +
    geom_line(aes(y = all_ages, x = year, group = LSOA21CD), colour = "red") +
    geom_line(aes(y = adults, x = year, group = LSOA21CD), colour = "blue") +
    geom_line(aes(y = households_est, x = year, group = LSOA21CD), colour = "green") +
    scale_x_continuous(breaks = seq(2002,2022,2), expand = c(0,1)) +
    scale_y_continuous(
      expand = c(0,0),
      limits = c(0, NA)
    ) +
    geom_point(data = foo4, aes(x = year, y = households),stat="identity") +
    geom_line(data = build, aes(y = all_properties, x = year, group = ecode), colour = "black")# +
    #geom_line(data = foo_new, aes(y = all_ages, x = year, group = LSOA21CD), colour = "red") +
    #geom_line(data = foo_new, aes(y = adults, x = year, group = LSOA21CD), colour = "blue") +





}


