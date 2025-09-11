load_income_scotland = function(path = "../inputdata/income/scotland/"){

  income_2018 = readxl::read_xlsx(file.path(path,"CHMA+-+2018+-+Publication+-+LLHIE+Estimates+Data+Summary+-+2018+-+Minor+Revsions+-+24+May+2019.xlsx"),
                                  sheet = "Income Estimates 2018",
                                skip = 5)

  income_2017 = readxl::read_xlsx(file.path(path,"CHMA+-+2018+-+Publication+-+LLHIE+Estimates+Data+Summary+-+2017+-+Embargoed+Until+9.30+5th+November+2020.xlsx"),
                                  sheet = "Income Estimates 2017",
                                  skip = 4)

  income_2015 = readxl::read_xlsx(file.path(path,"CHMA+-+2018+-+Publication+-+LLHIE+Estimates+Data+Summary+-+2015+-+Embargoed+Until+9.30+5th+November+2020.xlsx"),
                                  sheet = "Income Estimates 2015",
                                  skip = 4)

  income_2014 = readxl::read_xlsx(file.path(path,"CHMA+-+2018+-+Publication+-+LLHIE+Estimates+Data+Summary+-+2014+-+September+2019.xlsx"),
                                  sheet = "Income Estimates 2014",
                                  skip = 4)


  # Use mean to match ONS in E&W
  # https://www.ons.gov.uk/employmentandlabourmarket/peopleinwork/earningsandworkinghours/methodologies/smallareaincomeestimatesmodelbasedestimatesofthemeanhouseholdweeklyincomeformiddlelayersuperoutputareas201314technicalreport

  income_2018_lims = purrr::pmap_dfr(.f = income_limit_estimator,
                                 .l = list(u50 = income_2018$`Gross Household Income under £50 per week (proportion of households)`,
                                           u100 = income_2018$`Gross Household Income under £100 per week (proportion of households)`,
                                           u150 = income_2018$`Gross Household Income under £150 per week (proportion of households)`,
                                           u200 = income_2018$`Gross Household Income under £200 per week (proportion of households)`,
                                           u250 = income_2018$`Gross Household Income under £250 per week (proportion of households)`,
                                           u300 = income_2018$`Gross Household Income under £300 per week (proportion of households)`,
                                           u350 = income_2018$`Gross Household Income under £350 per week (proportion of households)`,
                                           u400 = income_2018$`Gross Household Income under £400 per week (proportion of households)`,
                                           u500 = income_2018$`Gross Household Income under £500 per week (proportion of households)`,
                                           u600 = income_2018$`Gross Household Income under £600 per week (proportion of households)`,
                                           u700 = income_2018$`Gross Household Income under £700 per week (proportion of households)`,
                                           u800 = income_2018$`Gross Household Income under £800 per week (proportion of households)`,
                                           u900 = income_2018$`Gross Household Income under £900 per week (proportion of households)`,
                                           u1000 = income_2018$`Gross Household Income under £1,000 per week (proportion of households)`,
                                           u1200 = income_2018$`Gross Household Income under £1,200 per week (proportion of households)`,
                                           u2000 = income_2018$`Gross Household Income under £2,000 per week (proportion of households)`))

  income_2018 = cbind(income_2018, income_2018_lims)
  income_2018 = income_2018[,c("2011 Data Zone","Mean Gross Household Income per week","lower_limit","upper_limit")]


  income_2017_lims = purrr::pmap_dfr(.f = income_limit_estimator,
                                     .l = list(u50 = income_2017$`Gross Household Income under £50 per week (proportion of households)`,
                                               u100 = income_2017$`Gross Household Income under £100 per week (proportion of households)`,
                                               u150 = income_2017$`Gross Household Income under £150 per week (proportion of households)`,
                                               u200 = income_2017$`Gross Household Income under £200 per week (proportion of households)`,
                                               u250 = income_2017$`Gross Household Income under £250 per week (proportion of households)`,
                                               u300 = income_2017$`Gross Household Income under £300 per week (proportion of households)`,
                                               u350 = income_2017$`Gross Household Income under £350 per week (proportion of households)`,
                                               u400 = income_2017$`Gross Household Income under £400 per week (proportion of households)`,
                                               u500 = income_2017$`Gross Household Income under £500 per week (proportion of households)`,
                                               u600 = income_2017$`Gross Household Income under £600 per week (proportion of households)`,
                                               u700 = income_2017$`Gross Household Income under £700 per week (proportion of households)`,
                                               u800 = income_2017$`Gross Household Income under £800 per week (proportion of households)`,
                                               u900 = income_2017$`Gross Household Income under £900 per week (proportion of households)`,
                                               u1000 = income_2017$`Gross Household Income under £1,000 per week (proportion of households)`,
                                               u1200 = income_2017$`Gross Household Income under £1,200 per week (proportion of households)`,
                                               u2000 = income_2017$`Gross Household Income under £2,000 per week (proportion of households)`))

  income_2017 = cbind(income_2017, income_2017_lims)
  income_2017 = income_2017[,c("2011 Data Zone","Mean Gross Household Income per week","lower_limit","upper_limit")]


  income_2015_lims = purrr::pmap_dfr(.f = income_limit_estimator,
                                     .l = list(u50 = income_2015$`Gross Household Income under £50 per week (proportion of households)`,
                                               u100 = income_2015$`Gross Household Income under £100 per week (proportion of households)`,
                                               u150 = income_2015$`Gross Household Income under £150 per week (proportion of households)`,
                                               u200 = income_2015$`Gross Household Income under £200 per week (proportion of households)`,
                                               u250 = income_2015$`Gross Household Income under £250 per week (proportion of households)`,
                                               u300 = income_2015$`Gross Household Income under £300 per week (proportion of households)`,
                                               u350 = income_2015$`Gross Household Income under £350 per week (proportion of households)`,
                                               u400 = income_2015$`Gross Household Income under £400 per week (proportion of households)`,
                                               u500 = income_2015$`Gross Household Income under £500 per week (proportion of households)`,
                                               u600 = income_2015$`Gross Household Income under £600 per week (proportion of households)`,
                                               u700 = income_2015$`Gross Household Income under £700 per week (proportion of households)`,
                                               u800 = income_2015$`Gross Household Income under £800 per week (proportion of households)`,
                                               u900 = income_2015$`Gross Household Income under £900 per week (proportion of households)`,
                                               u1000 = income_2015$`Gross Household Income under £1,000 per week (proportion of households)`,
                                               u1200 = income_2015$`Gross Household Income under £1,200 per week (proportion of households)`,
                                               u2000 = income_2015$`Gross Household Income under £2,000 per week (proportion of households)`))

  income_2015 = cbind(income_2015, income_2015_lims)
  income_2015 = income_2015[,c("2011 Data Zone","Mean Gross Household Income per week","lower_limit","upper_limit")]


  income_2014_lims = purrr::pmap_dfr(.f = income_limit_estimator,
                                     .l = list(u50 = income_2014$`Gross Household Income under £50 per week (proportion of households)`,
                                               u100 = income_2014$`Gross Household Income under £100 per week (proportion of households)`,
                                               u150 = income_2014$`Gross Household Income under £150 per week (proportion of households)`,
                                               u200 = income_2014$`Gross Household Income under £200 per week (proportion of households)`,
                                               u250 = income_2014$`Gross Household Income under £250 per week (proportion of households)`,
                                               u300 = income_2014$`Gross Household Income under £300 per week (proportion of households)`,
                                               u350 = income_2014$`Gross Household Income under £350 per week (proportion of households)`,
                                               u400 = income_2014$`Gross Household Income under £400 per week (proportion of households)`,
                                               u500 = income_2014$`Gross Household Income under £500 per week (proportion of households)`,
                                               u600 = income_2014$`Gross Household Income under £600 per week (proportion of households)`,
                                               u700 = income_2014$`Gross Household Income under £700 per week (proportion of households)`,
                                               u800 = income_2014$`Gross Household Income under £800 per week (proportion of households)`,
                                               u900 = income_2014$`Gross Household Income under £900 per week (proportion of households)`,
                                               u1000 = income_2014$`Gross Household Income under £1,000 per week (proportion of households)`,
                                               u1200 = income_2014$`Gross Household Income under £1,200 per week (proportion of households)`,
                                               u2000 = income_2014$`Gross Household Income under £2,000 per week (proportion of households)`))

  income_2014 = cbind(income_2014, income_2014_lims)
  income_2014 = income_2014[,c("2011 Data Zone code","Mean Gross Household Income per week","lower_limit","upper_limit")]
  names(income_2014)[1] = "2011 Data Zone"

  income_2018$year = 2018
  income_2017$year = 2017
  income_2015$year = 2015
  income_2014$year = 2014

  income_all = rbind(income_2014, income_2015, income_2017, income_2018)

  income_all$upper_limit = round(income_all$upper_limit * (365/7))
  income_all$lower_limit = round(income_all$lower_limit * (365/7))
  income_all$total_annual_income = round(income_all$`Mean Gross Household Income per week` * (365/7))

  income_all = income_all[,c("2011 Data Zone","year","lower_limit","upper_limit","total_annual_income")]
  income_all

}

income_limit_estimator = function(u50, u100, u150, u200, u250, u300, u350, u400, u500, u600, u700, u800, u900, u1000, u1200, u2000){

  comb = c(u50, u100, u150, u200, u250, u300, u350, u400, u500, u600, u700, u800, u900, u1000, u1200, u2000)
  names(comb) = c("u50", "u100", "u150", "u200", "u250", "u300", "u350", "u400", "u500", "u600", "u700", "u800", "u900", "u1000", "u1200", "u2000")

  # Trim to 95% confidence interval
  low = comb[comb > 0.025]
  low = low[1]

  high = comb[comb < 0.975]
  high = high[length(high)]

  res = data.frame(lower_limit = as.numeric(gsub("u","",names(low))),
                   upper_limit = as.numeric(gsub("u","",names(high))))

  res


}


esimate_income_scotland_dz22 = function(income_scot_dz11, lookup_dz_2011_22_pre){

  lookup_dz_2011_22_pre = sf::st_drop_geometry(lookup_dz_2011_22_pre)
  lookup_dz_2011_22_pre = lookup_dz_2011_22_pre[,c("DataZone","DataZone22","count")]

  lookup_dz_2011_22_pre = dplyr::group_split(lookup_dz_2011_22_pre, DataZone22)

  res = list()
  for(i in seq_along(lookup_dz_2011_22_pre)){
    lookup_sub = lookup_dz_2011_22_pre[[i]]
    income_sub = income_scot_dz11[income_scot_dz11$`2011 Data Zone` %in% lookup_sub$DataZone, ]
    income_sub = dplyr::left_join(income_sub, lookup_sub, by = c("2011 Data Zone" = "DataZone"))

    income_sub2 = income_sub |>
      dplyr::group_by(DataZone22, year) |>
      dplyr::summarise(lower_limit = weighted.mean(lower_limit, count, na.rm = TRUE),
                       upper_limit = weighted.mean(upper_limit, count, na.rm = TRUE),
                       total_annual_income = weighted.mean(total_annual_income, count, na.rm = TRUE),

                       )
    res[[i]] = income_sub2

  }

  res = dplyr::bind_rows(res)
  res
}
