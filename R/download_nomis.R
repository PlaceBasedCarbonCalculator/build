#' Download 2021 census bulk tables from Nomis
#'
#' @description Downloads 23 zipped 2021 census topic summary tables (TS001
#'   etc. covering households, demographics, housing, travel and health) from
#'   the Nomis bulk download service into `path`. Existing files are
#'   re-downloaded. This is the `dl_nomis` target, a prerequisite of the
#'   census-table loaders below and in other files.
#' @param path Folder to store the zips; created if missing.
#' @return TRUE, for use as a targets dependency.
#' @keywords internal
dowload_nomis = function(path = file.path(parameters$path_data,"nomis")){
  if(!dir.exists(path)){
    dir.create(path)
  }


  #2021
  baseurl = "https://www.nomisweb.co.uk/output/census/2021/"

  urls = c("census2021-ts041.zip", #Number of Households
           "census2021-ts011.zip", #Households by deprivation dimensions,
           "census2021-ts003.zip", #Household composition
           "census2021-ts001.zip", #Number of usual residents in households and communal establishments
           "census2021-ts007a.zip", #Age by five-year age bands

           "census2021-ts021.zip", # Ethnic Group
           "census2021-ts058.zip", # Distance Travelled to Work
           "census2021-ts060.zip", # Industry
           "census2021-ts062.zip", # NS-SeC
           "census2021-ts063.zip", # Occupation
           "census2021-ts065.zip", # Unemployment history
           "census2021-ts066.zip", # Economic activity status

           "census2021-ts044.zip", # Accommodation type
           "census2021-ts045.zip", # Car or van availability
           "census2021-ts046.zip", # Central heating
           "census2021-ts048.zip", # Communal establishment management and type
           "census2021-ts050.zip", # Number of bedrooms
           "census2021-ts051.zip", # Number of rooms
           "census2021-ts054.zip", # Tenure
           "census2021-ts055.zip", # Purpose of second address

           "census2021-ts068.zip", # Schoolchildren and full-time students

           "census2021-ts037.zip", # General health
           "census2021-ts038.zip" # Disability
  )


  for(i in 1:length(urls)){
    download.file(url = paste0(baseurl,urls[i]),
                  destfile = file.path(path,urls[i]))
  }
  return(TRUE)

}

#' Load 2021 census population by 5-year age band (TS007a) for LSOAs
#'
#' @description Reads the LSOA-level table from the downloaded
#'   `census2021-ts007a.zip`. Used by the `population_2021` target.
#' @param path Folder of Nomis downloads (`dl_nomis` target).
#' @return A data frame with `year`, `LSOA21`, `all_ages` and age bands
#'   "0-4" ... "85+".
#' @keywords internal
load_population_2021 = function(path = file.path(parameters$path_data,"nomis")){
  dat = unzip_nomis(file.path(path,"census2021-ts007a.zip"))
  names(dat) = c("year","LSOA21NM","LSOA21","all_ages","0-4","5-9","10-14","15-19","20-24",
                 "25-29","30-34","35-39","40-44","45-49","50-54","55-59",
                 "60-64","65-69","70-74","75-79","80-84","85+")
  dat$LSOA21NM = NULL
  dat
}

#' Unzip a Nomis census bulk zip and read its LSOA-level CSV
#'
#' @description Helper for the census-table loaders: extracts a Nomis bulk
#'   zip to a temp folder, reads the file matching `lsoa.csv`, and cleans up.
#' @param file Path to a `census2021-*.zip` bulk download.
#' @return A data frame of the LSOA-level census table.
#' @keywords internal
unzip_nomis = function(file = file.path(path,"census2021-ts007a.zip")){

  dir.create(file.path(tempdir(),"nomis"))
  unzip(file, exdir = file.path(tempdir(),"nomis"))
  fl = list.files(file.path(tempdir(),"nomis"), pattern = "lsoa.csv", full.names = TRUE)
  fl = read.csv(fl)
  unlink(file.path(tempdir(),"nomis"), recursive = TRUE)
  fl
}

#' Load 2021 census car/van availability (TS045) for LSOAs
#'
#' @description Reads households by number of cars/vans from the downloaded
#'   `census2021-ts045.zip` and estimates the total vehicles per LSOA
#'   (3+ car households counted as 3). Used by the `vehicle_cenus21` target.
#' @param path Folder of Nomis downloads (`dl_nomis` target).
#' @return A data frame with `year`, `LSOA21`, household counts by car
#'   ownership and `total_carvan_est`.
#' @keywords internal
load_census_2021_vehicles = function(path = file.path(parameters$path_data,"nomis")){
  dat = unzip_nomis(file.path(path,"census2021-ts045.zip"))
  names(dat) = c("year","LSOA21NM","LSOA21","households_total","households_noCarVan","households_1CarVan","households_2CarVan","households_3plusCarVan")
  dat$total_carvan_est = dat$households_1CarVan + 2 * dat$households_2CarVan + 3 * dat$households_3plusCarVan
  dat$LSOA21NM = NULL
  dat
}

#' Load 2021 census household counts (TS041) for LSOAs
#'
#' @description Reads the number of households per LSOA from the downloaded
#'   `census2021-ts041.zip`. Used by the `households_cenus21` target, which
#'   anchors the historical household extrapolation.
#' @param path Folder of Nomis downloads (`dl_nomis` target).
#' @return A data frame with `year`, `LSOA21` and `households_total`.
#' @keywords internal
load_census_2021_households = function(path = file.path(parameters$path_data,"nomis")){
  dat = unzip_nomis(file.path(path,"census2021-ts041.zip"))
  names(dat) = c("year","LSOA21NM","LSOA21","households_total")
  dat$LSOA21NM = NULL
  dat
}
