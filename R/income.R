dowload_income_msoa = function(path = file.path(parameters$path_data,"income")){
  if(!dir.exists(path)){
    dir.create(path)
  }

  #https://www.ons.gov.uk/employmentandlabourmarket/peopleinwork/earningsandworkinghours/datasets/smallareaincomeestimatesformiddlelayersuperoutputareasenglandandwales
  url2020 = "https://www.ons.gov.uk/file?uri=/employmentandlabourmarket/peopleinwork/earningsandworkinghours/datasets/smallareaincomeestimatesformiddlelayersuperoutputareasenglandandwales/financialyearending2020/saiefy1920finalqaddownload280923.xlsx"
  url2018 = "https://www.ons.gov.uk/file?uri=/employmentandlabourmarket/peopleinwork/earningsandworkinghours/datasets/smallareaincomeestimatesformiddlelayersuperoutputareasenglandandwales/financialyearending2018/incomeestimatesforsmallareasdatasetfinancialyearending20181.xls"
  url2016 = "https://www.ons.gov.uk/file?uri=/employmentandlabourmarket/peopleinwork/earningsandworkinghours/datasets/smallareaincomeestimatesformiddlelayersuperoutputareasenglandandwales/financialyearending2016/1smallareaincomeestimatesdata.xls"
  url2014 = "https://www.ons.gov.uk/file?uri=/employmentandlabourmarket/peopleinwork/earningsandworkinghours/datasets/smallareaincomeestimatesformiddlelayersuperoutputareasenglandandwales/financialyearending2014/1smallareaincomeestimatesdataupdate.xls"
  url2012 = "https://www.ons.gov.uk/file?uri=/employmentandlabourmarket/peopleinwork/earningsandworkinghours/datasets/smallareaincomeestimatesformiddlelayersuperoutputareasenglandandwales/201112/1smallareaincomeestimatesdatatcm77420299.xls"

  download.file(url2020, destfile = file.path(path,"income2020.xlsx"), mode = "wb")
  download.file(url2018, destfile = file.path(path,"income2018.xls"), mode = "wb")
  download.file(url2016, destfile = file.path(path,"income2016.xls"), mode = "wb")
  download.file(url2014, destfile = file.path(path,"income2014.xls"), mode = "wb")
  download.file(url2012, destfile = file.path(path,"income2012.xls"), mode = "wb")

  return(TRUE)

}

load_experian_income = function(path = file.path(parameters$path_secure_data,"CREDS Data/Tim Share/From Malcolm/Experian.zip")){
  # https://beta.ukdataservice.ac.uk/datacatalogue/studies/study?id=5738&type=Data%20catalogue
  dir.create(file.path(tempdir(),"income"))
  unzip(path, exdir = file.path(tempdir(),"income"))
  income <- read.csv(file.path(tempdir(),"income/UKDA-5738-csv/csv/2011-experian-data.csv"))
  unlink(file.path(tempdir(),"income"))
  names(income) <- income[4,]
  income <- income[5:nrow(income),]
  income <- income[,c("GeographyValue","Median_(H) Household Income Value")]
  names(income) <- c("LSOA01","median_household_income")
  income$median_household_income <- as.numeric(income$median_household_income)
  income
}

load_msoa_income = function(path = file.path(parameters$path_data,"income")){
  #TODO: Total Weakly Income or Net weekly Income / Equivalised  / Before / After Housing Costs

  income2012 = readxl::read_excel(file.path(path, "income2012.xls"), "Total weekly income")
  income2014 = readxl::read_excel(file.path(path, "income2014.xls"), "Total weekly income")
  income2016 = readxl::read_excel(file.path(path, "income2016.xls"), "Total annual income")
  income2018 = readxl::read_excel(file.path(path, "income2018.xls"), "Total annual income")
  income2020 = readxl::read_excel(file.path(path, "income2020.xlsx"), "Total annual income")

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
  income2014$upper_limit = as.numeric(income2014$upper_limit)
  income2014$lower_limit = as.numeric(income2014$lower_limit)
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


  income2012$total_annual_income = round(income2012$total_weekly_income * (365/7))
  income2014$total_annual_income = round(income2014$total_weekly_income * (365/7))

  income2012$total_weekly_income = NULL
  income2014$total_weekly_income = NULL

  income = rbind(income2012,income2014,income2016,income2018,income2020)

  income

}




estimate_income = function(experian_income, income_msoa, lookup_lsoa_2001_11, lookup_OA_LSOA_MSOA_classifications, lookup_lsoa_2011_21){

  lookup_lsoa_2011_21 = lookup_lsoa_2011_21[,c("LSOA11CD","LSOA21CD")]
  lookup_lsoa_2011_21 = lookup_lsoa_2011_21[!duplicated(lookup_lsoa_2011_21$LSOA21CD),] #TODO: check that is is valid assumption

  lookup_OA_LSOA_MSOA_classifications = lookup_OA_LSOA_MSOA_classifications[,c("LSOA11CD","MSOA11CD")]
  lookup_OA_LSOA_MSOA_classifications = lookup_OA_LSOA_MSOA_classifications[!duplicated(lookup_OA_LSOA_MSOA_classifications$LSOA11CD),]

  income_msoa = income_msoa[,c("MSOA11","total_annual_income_2020")]

  lookup_lsoa_2001_11 = lookup_lsoa_2001_11[,c("LSOA01CD","LSOA11CD")]
  lookup_lsoa_2001_11 = lookup_lsoa_2001_11[!duplicated(lookup_lsoa_2001_11$LSOA11CD),]

  lsoa_income = dplyr::left_join(lookup_OA_LSOA_MSOA_classifications, income_msoa, by = c("MSOA11CD" = "MSOA11"))
  lsoa_income = dplyr::left_join(lsoa_income, lookup_lsoa_2001_11, by = c("LSOA11CD" = "LSOA11CD"))
  lsoa_income = dplyr::left_join(lsoa_income, experian_income, by = c("LSOA01CD" = "LSOA01"))

  weightings = dplyr::group_split(lsoa_income, MSOA11CD, .keep = )

  income_final <- lapply(weightings, function(x){
    x$weight <- x$median_household_income / mean(x$median_household_income, na.rm = TRUE)
    x$income_lsoa <- x$total_annual_income_2020 * x$weight
    x$income_lsoa[is.na(x$income_lsoa)] <- x$total_annual_income_2020[1]
    x <- x[,c("LSOA11CD","income_lsoa")]
    return(x)
  })
  income_final <- dplyr::bind_rows(income_final)

  income_final <- dplyr::left_join(lookup_lsoa_2011_21, income_final, by = c("LSOA11CD"))
  income_final$LSOA11CD = NULL

  income_final
}
