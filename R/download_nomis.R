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

load_population_2021 = function(path = file.path(parameters$path_data,"nomis")){
  dat = unzip_nomis(file.path(path,"census2021-ts007a.zip"))
  names(dat) = c("year","LSOA21NM","LSOA21","all_ages","0-4","5-9","10-14","15-19","20-24",
                 "25-29","30-34","35-39","40-44","45-49","50-54","55-59",
                 "60-64","65-69","70-74","75-79","80-84","85+")
  dat$LSOA21NM = NULL
  dat
}

# Helper function to unzip to temp dir and read in LSOA table
unzip_nomis = function(file = file.path(path,"census2021-ts007a.zip")){

  dir.create(file.path(tempdir(),"nomis"))
  unzip(file, exdir = file.path(tempdir(),"nomis"))
  fl = list.files(file.path(tempdir(),"nomis"), pattern = "lsoa.csv", full.names = TRUE)
  fl = read.csv(fl)
  unlink(file.path(tempdir(),"nomis"), recursive = TRUE)
  fl
}

load_census_2021_vehicles = function(path = file.path(parameters$path_data,"nomis")){
  dat = unzip_nomis(file.path(path,"census2021-ts045.zip"))
  names(dat) = c("year","LSOA21NM","LSOA21","households_total","households_noCarVan","households_1CarVan","households_2CarVan","households_3plusCarVan")
  dat$total_carvan_est = dat$households_1CarVan + 2 * dat$households_2CarVan + 3 * dat$households_3plusCarVan
  dat$LSOA21NM = NULL
  dat
}

