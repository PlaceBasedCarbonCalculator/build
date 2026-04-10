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



estimate_income = function(experian_income, income_msoa, lookup_lsoa_2001_11, lookup_OA_LSOA_MSOA_classifications, lookup_lsoa_2011_21){

  lookup_lsoa_2011_21 = lookup_lsoa_2011_21[,c("LSOA11CD","LSOA21CD")]
  lookup_lsoa_2011_21 = lookup_lsoa_2011_21[!duplicated(lookup_lsoa_2011_21$LSOA21CD),] #TODO: check that is is valid assumption

  lookup_OA_LSOA_MSOA_classifications = lookup_OA_LSOA_MSOA_classifications[,c("LSOA11CD","MSOA11CD")]
  lookup_OA_LSOA_MSOA_classifications = lookup_OA_LSOA_MSOA_classifications[!duplicated(lookup_OA_LSOA_MSOA_classifications$LSOA11CD),]

  income_msoa = income_msoa[income_msoa$year == 2020,]
  income_msoa = income_msoa[,c("MSOA11","total_annual_income")]

  lookup_lsoa_2001_11 = lookup_lsoa_2001_11[,c("LSOA01CD","LSOA11CD")]
  lookup_lsoa_2001_11 = lookup_lsoa_2001_11[!duplicated(lookup_lsoa_2001_11$LSOA11CD),]

  lsoa_income = dplyr::left_join(lookup_OA_LSOA_MSOA_classifications, income_msoa, by = c("MSOA11CD" = "MSOA11"))
  lsoa_income = dplyr::left_join(lsoa_income, lookup_lsoa_2001_11, by = c("LSOA11CD" = "LSOA11CD"))
  lsoa_income = dplyr::left_join(lsoa_income, experian_income, by = c("LSOA01CD" = "LSOA01"))

  weightings = dplyr::group_split(lsoa_income, MSOA11CD, .keep = )

  income_final <- lapply(weightings, function(x){
    x$weight <- x$median_household_income / mean(x$median_household_income, na.rm = TRUE)
    x$income_lsoa <- x$total_annual_income * x$weight
    x$income_lsoa[is.na(x$income_lsoa)] <- x$total_annual_income[1]
    x <- x[,c("LSOA11CD","income_lsoa")]
    return(x)
  })
  income_final <- dplyr::bind_rows(income_final)

  income_final <- dplyr::left_join(lookup_lsoa_2011_21, income_final, by = c("LSOA11CD"))
  income_final$LSOA11CD = NULL

  income_final
}
