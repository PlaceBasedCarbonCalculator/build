
#' Load Msoa Income
#'
#' @description Load msoa income data from the source path and return it as an R object.
#' @details This function is used as part of the pipeline input ingestion stage.
#' @param path File or directory path.
#' @return A data frame containing the loaded dataset.
#' @keywords internal
load_msoa_income = function(path = file.path(parameters$path_data,"income")){
  #TODO: Total Weakly Income or Net weekly Income / Equivalised  / Before / After Housing Costs

  income2012 = readxl::read_excel(file.path(path, "income2012.xls"), "Total weekly income")
  income2014 = readxl::read_excel(file.path(path, "income2014.xls"), "Total weekly income")
  income2016 = readxl::read_excel(file.path(path, "income2016.xls"), "Total annual income")
  income2018 = readxl::read_excel(file.path(path, "income2018.xls"), "Total annual income")
  income2020 = readxl::read_excel(file.path(path, "income2020.xlsx"), "Total annual income")
  income2023 = readxl::read_excel(file.path(path, "income2023.xlsx"), "Total annual income")

  names(income2012) = c("MSOA11","MSOAname","Localauthoritycode","Localauthorityname","Regioncode","Regionname",
                        "total_weekly_income","upper_limit","lower_limit","interval")
  income2012 = income2012[5:nrow(income2012), ]
  income2012 = income2012[!is.na(income2012$MSOAname),]
  income2012 = income2012[,c("MSOA11","total_weekly_income","upper_limit","lower_limit")]
  income2012$total_weekly_income = as.numeric(income2012$total_weekly_income)
  income2012$upper_limit = as.numeric(income2012$upper_limit)
  income2012$lower_limit = as.numeric(income2012$lower_limit)
  income2012$year = 2012

  names(income2014) = c("MSOA11","MSOAname","Localauthoritycode","Localauthorityname","Regioncode","Regionname",
                        "total_weekly_income","upper_limit","lower_limit","interval")
  income2014 = income2014[5:nrow(income2014), ]
  income2014 = income2014[!is.na(income2014$MSOAname),]
  income2014 = income2014[,c("MSOA11","total_weekly_income","upper_limit","lower_limit")]
  income2014$total_weekly_income = as.numeric(income2014$total_weekly_income)
  income2014$upper_limit = as.numeric(income2014$upper_limit) * 52
  income2014$lower_limit = as.numeric(income2014$lower_limit) * 52
  income2014$year = 2014

  names(income2016) = c("MSOA11","MSOAname","Localauthoritycode","Localauthorityname","Regioncode","Regionname",
                        "total_annual_income","upper_limit","lower_limit","interval")
  income2016 = income2016[5:nrow(income2016), ]
  income2016 = income2016[!is.na(income2016$MSOAname),]
  income2016 = income2016[,c("MSOA11","total_annual_income","upper_limit","lower_limit")]
  income2016$total_annual_income = as.numeric(income2016$total_annual_income)
  income2016$upper_limit = as.numeric(income2016$upper_limit)
  income2016$lower_limit = as.numeric(income2016$lower_limit)
  income2016$year = 2016

  names(income2018) = c("MSOA11","MSOAname","Localauthoritycode","Localauthorityname","Regioncode","Regionname",
                        "total_annual_income","upper_limit","lower_limit","interval")
  income2018 = income2018[5:nrow(income2018), ]
  income2018 = income2018[!is.na(income2018$MSOAname),]
  income2018 = income2018[,c("MSOA11","total_annual_income","upper_limit","lower_limit")]
  income2018$total_annual_income = as.numeric(income2018$total_annual_income)
  income2018$upper_limit = as.numeric(income2018$upper_limit)
  income2018$lower_limit = as.numeric(income2018$lower_limit)
  income2018$year = 2018

  names(income2020) = c("MSOA11","MSOAname","Localauthoritycode","Localauthorityname","Regioncode","Regionname",
                        "total_annual_income","upper_limit","lower_limit","interval")
  income2020 = income2020[5:nrow(income2020), ]
  income2020 = income2020[!is.na(income2020$MSOAname),]
  income2020 = income2020[,c("MSOA11","total_annual_income","upper_limit","lower_limit")]
  income2020$total_annual_income = as.numeric(income2020$total_annual_income)
  income2020$upper_limit = as.numeric(income2020$upper_limit)
  income2020$lower_limit = as.numeric(income2020$lower_limit)
  income2020$year = 2020

  names(income2023) = c("MSOA21","MSOAname","Localauthoritycode","Localauthorityname","Regioncode","Regionname",
                        "total_annual_income","upper_limit","lower_limit","interval")
  income2023 = income2023[4:nrow(income2023), ]
  income2023 = income2023[!is.na(income2023$MSOAname),]
  income2023 = income2023[,c("MSOA21","total_annual_income","upper_limit","lower_limit")]
  income2023$total_annual_income = as.numeric(income2023$total_annual_income)
  income2023$upper_limit = as.numeric(income2023$upper_limit)
  income2023$lower_limit = as.numeric(income2023$lower_limit)
  income2023$year = 2023

  income2012$total_annual_income = round(income2012$total_weekly_income * (365/7))
  income2014$total_annual_income = round(income2014$total_weekly_income * (365/7))

  income2012$upper_limit = round(income2012$upper_limit * (365/7))
  income2012$lower_limit = round(income2012$lower_limit * (365/7))

  income2014$upper_limit = round(income2014$upper_limit * (365/7))
  income2014$lower_limit = round(income2014$lower_limit * (365/7))

  income2012$total_weekly_income = NULL
  income2014$total_weekly_income = NULL

  income = dplyr::bind_rows(income2012,income2014,income2016,income2018,income2020,income2023)

  income

}

