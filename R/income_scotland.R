#' Load Scottish small-area household income estimates (CHMA/LLHIE)
#'
#' @description Reads the Scottish Government local-level household income
#'   estimates for 2014, 2015, 2017 and 2018 (2011 Data Zones). Mean gross
#'   weekly income is annualised (x 365/7) to match the E&W ONS estimates,
#'   and approximate 95% confidence limits are derived from the cumulative
#'   income-band proportions via `income_limit_estimator()`. Used by the
#'   `income_scot_dz11` target.
#' @param path Folder of the CHMA/LLHIE xlsx files.
#' @return A data frame with `2011 Data Zone`, `year`, `lower_limit`,
#'   `upper_limit` and `total_annual_income`.
#' @keywords internal
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

#' Estimate 95% income limits from cumulative band proportions
#'
#' @description Given the cumulative proportion of households with weekly
#'   income below each threshold (50 to 2000 pounds), returns the band
#'   thresholds nearest the 2.5th and 97.5th percentiles as approximate
#'   lower/upper limits.
#' @param u50,u100,u150,u200,u250,u300,u350,u400,u500,u600,u700,u800,u900,u1000,u1200,u2000
#'   Cumulative proportions of households below each weekly income threshold.
#' @return A one-row data frame with `lower_limit` and `upper_limit`
#'   (pounds per week).
#' @keywords internal
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


#' Convert Scottish income estimates to 2022 Data Zones and extend to 2020
#'
#' @description Re-averages the 2011 Data Zone income estimates onto 2022
#'   Data Zones using UPRN-count weights from `make_dz_11_22_lookup()`, then
#'   projects the 2018 values to 2019 and 2020 using a national income trend
#'   workbook. Used by the `income_scot_dz22` target, feeding the retrofit
#'   map and the Scottish synthetic population income matching.
#' @param income_scot_dz11 Output of `load_income_scotland()`.
#' @param lookup_dz_2011_22_pre The `lookup_dz_2011_22_pre` target.
#' @param path Path to `Scotland Income Trends.xlsx`.
#' @return A data frame with `DataZone22`, `year` (2014-2020),
#'   `lower_limit`, `upper_limit` and `total_annual_income`.
#' @keywords internal
esimate_income_scotland_dz22 = function(income_scot_dz11, lookup_dz_2011_22_pre, path = "../inputdata/income/scotland/Scotland Income Trends.xlsx"){

  lookup_dz_2011_22_pre = sf::st_drop_geometry(lookup_dz_2011_22_pre)
  lookup_dz_2011_22_pre = lookup_dz_2011_22_pre[,c("DataZone","DataZone22","count")]

  lookup_dz_2011_22_pre = dplyr::group_split(lookup_dz_2011_22_pre, DataZone22)

  trend = readxl::read_excel(path,"Combined")
  trend = trend[,c("year","historic")]
  trend$year1 = as.integer(substr(trend$year,1,4))
  trend = trend[trend$year >= 2018,]
  trend$weight = trend$historic / trend$historic[trend$year1 == 2018]

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

  # Add on Extra Years
  res_2018 = res[res$year == 2018,]

  res_extra = list()
  for(i in 2019:2020){
    sub = res_2018
    sub$year = i
    weight = trend$weight[trend$year1 == i]
    sub$lower_limit = sub$lower_limit * weight
    sub$upper_limit = sub$upper_limit * weight
    sub$total_annual_income = sub$total_annual_income * weight
    res_extra[[i]] = sub
  }

  res_extra = dplyr::bind_rows(res_extra)

  res = rbind(res, res_extra)

  res
}
