#' Load Income Scotland
#'
#' @description Load income scotland data from the source path and return it as an R object.
#' @details This function is used as part of the pipeline input ingestion stage.
#' @param path File or directory path.
#' @return A data frame containing the loaded dataset.
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

#' Income Limit Estimator
#'
#' @description Use income band data to estimate upper and lower income
#'   confidence limits
#' @param u50 Input object or parameter named `u50`.
#' @param u100 Input object or parameter named `u100`.
#' @param u150 Input object or parameter named `u150`.
#' @param u200 Input object or parameter named `u200`.
#' @param u250 Input object or parameter named `u250`.
#' @param u300 Input object or parameter named `u300`.
#' @param u350 Input object or parameter named `u350`.
#' @param u400 Input object or parameter named `u400`.
#' @param u500 Input object or parameter named `u500`.
#' @param u600 Input object or parameter named `u600`.
#' @param u700 Input object or parameter named `u700`.
#' @param u800 Input object or parameter named `u800`.
#' @param u900 Input object or parameter named `u900`.
#' @param u1000 Input object or parameter named `u1000`.
#' @param u1200 Input object or parameter named `u1200`.
#' @param u2000 Input object or parameter named `u2000`.
#' @return A data frame produced by the function.
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


#' Esimate Income Scotland Dz22
#'
#' @description Perform processing for esimate income scotland dz22.
#' @param income_scot_dz11 Input object or parameter named `income_scot_dz11`.
#' @param lookup_dz_2011_22_pre Lookup table used to map area codes or classifications.
#' @param path File or directory path.
#' @return The function result, typically a data frame or list used in the pipeline.
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
