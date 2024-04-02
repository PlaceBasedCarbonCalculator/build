load_building_age_2011 = function(path = file.path(parameters$path_secure_data,"CDRC/building age price")) {
  age = read.csv(file.path(path,"voapropertyage.csv"))
  names(age)[1] = "LSAO11CD"
  age
}

load_housing_type_2021 = function(path = file.path(parameters$path_data,"nomis")){
  dat = unzip_nomis(file.path(path,"census2021-ts044.zip"))
  names(dat) = c("year","LSOA21NM","LSOA21CD","all_households",
                 "detached","semidetached","terraced","flats_purposebuilt",
                 "flats_converted","converted_building",
                 "dwelling_in_commerical_building","mobile_tempoary")
  dat = dat[,3:ncol(dat)]
  dat
}


load_central_heating_2021 = function(path = file.path(parameters$path_data,"nomis")){
  dat = unzip_nomis(file.path(path,"census2021-ts046.zip"))
  names(dat) = c("year","LSOA21NM","LSOA21CD",
                 "all_households",
                 "no_central_heating",
                 "mains_gas",
                 "bottled_gas",
                 "electric",
                 "oil",
                 "wood",
                 "solid_fuel",
                 "renewable_energy",
                 "heat_network",
                 "other_central_heating",
                 "two_types_no_renewable_energy",
                 "two_types_inc_renewable_energy"  )
  dat = dat[,3:ncol(dat)]
  dat
}



