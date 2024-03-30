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
  income2012 = income2012[,c("MSOA11","total_weekly_income")]
  income2012$total_weekly_income = as.numeric(income2012$total_weekly_income)


  names(income2014) = c("MSOA11","MSOAname","Localauthoritycode","Localauthorityname","Regioncode","Regionname",
                        "total_weekly_income","upper_limit","lower_limit","interval")
  income2014 = income2014[5:nrow(income2014), ]
  income2014 = income2014[!is.na(income2014$MSOAname),]
  income2014 = income2014[,c("MSOA11","total_weekly_income")]
  income2014$total_weekly_income = as.numeric(income2014$total_weekly_income)

  names(income2016) = c("MSOA11","MSOAname","Localauthoritycode","Localauthorityname","Regioncode","Regionname",
                        "total_annual_income","upper_limit","lower_limit","interval")
  income2016 = income2016[5:nrow(income2016), ]
  income2016 = income2016[!is.na(income2016$MSOAname),]
  income2016 = income2016[,c("MSOA11","total_annual_income")]
  income2016$total_annual_income = as.numeric(income2016$total_annual_income)

  names(income2018) = c("MSOA11","MSOAname","Localauthoritycode","Localauthorityname","Regioncode","Regionname",
                        "total_annual_income","upper_limit","lower_limit","interval")
  income2018 = income2018[5:nrow(income2018), ]
  income2018 = income2018[!is.na(income2018$MSOAname),]
  income2018 = income2018[,c("MSOA11","total_annual_income")]
  income2018$total_annual_income = as.numeric(income2018$total_annual_income)

  names(income2020) = c("MSOA11","MSOAname","Localauthoritycode","Localauthorityname","Regioncode","Regionname",
                        "total_annual_income","upper_limit","lower_limit","interval")
  income2020 = income2020[5:nrow(income2020), ]
  income2020 = income2020[!is.na(income2020$MSOAname),]
  income2020 = income2020[,c("MSOA11","total_annual_income")]
  income2020$total_annual_income = as.numeric(income2020$total_annual_income)

  names(income2012) = paste0(names(income2012),"_2012")
  names(income2014) = paste0(names(income2014),"_2014")
  names(income2016) = paste0(names(income2016),"_2016")
  names(income2018) = paste0(names(income2018),"_2018")
  names(income2020) = paste0(names(income2020),"_2020")

  income2012$total_annual_income_2012 = round(income2012$total_weekly_income_2012 * (365/7))
  income2014$total_annual_income_2014 = round(income2014$total_weekly_income_2014 * (365/7))

  income2012$total_weekly_income_2012 = NULL
  income2014$total_weekly_income_2014 = NULL

  income = dplyr::left_join(income2012, income2014, by = c("MSOA11_2012" = "MSOA11_2014"))
  income = dplyr::left_join(income, income2016, by = c("MSOA11_2012" = "MSOA11_2016"))
  income = dplyr::left_join(income, income2018, by = c("MSOA11_2012" = "MSOA11_2018"))
  income = dplyr::left_join(income, income2020, by = c("MSOA11_2012" = "MSOA11_2020"))
  names(income)[1] = "MSOA11"

  income

}




estimate_income = function(experian_income, income_msoa, lookup_lsoa_2001_11, lookup_OA_LSOA_MSOA_classifications){

  #TODO; Finish porting this function


  lsoa_income <- left_join(msoa2lsoa, msoa, by = "MSOA11CD")

  lsoa_lookup <- lsoa_lookup[lsoa_lookup$LSOA01 %in% income$LSOA01,]
  lsoa_lookup <- lsoa_lookup[!duplicated(lsoa_lookup$LSOA11),]

  lsoa_income <- left_join(lsoa_income, lsoa_lookup, by = c("LSOA11CD" = "LSOA11"))

  lsoa_income <- left_join(lsoa_income, income, by = c("LSOA01" = "LSOA01"))

  weightings <- lsoa_income %>%
    group_by(MSOA11CD) %>%
    group_split()

  income_final <- lapply(weightings, function(x){
    x$weight <- x$median_household_income / mean(x$median_household_income, na.rm = T)
    x$income_lsoa <- x$annual_income * x$weight
    x$income_lsoa[is.na(x$income_lsoa)] <- x$annual_income[1]
    x <- x[,c("LSOA11CD","income_lsoa")]
    return(x)
  })
  income_final <- bind_rows(income_final)

  saveRDS(income_final, "data/income/lsoa_income_estimates.Rds")
}
