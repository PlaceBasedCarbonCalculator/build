load_uprn = function(path = file.path(parameters$path_data,"os_uprn")) {

  dir.create(file.path(tempdir(),"uprn"))
  unzip(file.path(path,"osopenuprn_202406_csv.zip"), exdir = file.path(tempdir(),"uprn"))

  uprn = readr::read_csv(file.path(tempdir(),"uprn","osopenuprn_202406.csv"))
  uprn = uprn[,c("UPRN","LATITUDE","LONGITUDE")]
  uprn = sf::st_as_sf(uprn, coords = c("LONGITUDE","LATITUDE"), crs = 4326)
  uprn
}


load_uprn_27770 = function(path = file.path(parameters$path_data,"os_uprn")) {

  dir.create(file.path(tempdir(),"uprn"))
  unzip(file.path(path,"osopenuprn_202406_csv.zip"), exdir = file.path(tempdir(),"uprn"))

  uprn = readr::read_csv(file.path(tempdir(),"uprn","osopenuprn_202406.csv"))
  uprn = uprn[,c("UPRN","EASTING","NORTHING")]
  uprn = sf::st_as_sf(uprn, coords = c("EASTING","NORTHING"), crs = 27700)
  uprn
}
