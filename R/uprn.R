load_uprn = function(path = file.path(parameters$path_data,"os_uprn")) {

  dir.create(file.path(tempdir(),"uprn"))
  unzip(file.path(path,"osopenuprn_202406_csv.zip"), exdir = file.path(tempdir(),"uprn"))

  uprn = readr::read_csv(file.path(tempdir(),"uprn","osopenuprn_202406.csv"))
  uprn = uprn[,c("UPRN","LATITUDE","LONGITUDE")]
  uprn = sf::st_as_sf(uprn, coords = c("LONGITUDE","LATITUDE"), crs = 4326)
  uprn
}


load_uprn_27700 = function(path = file.path(parameters$path_data,"os_uprn")) {

  dir.create(file.path(tempdir(),"uprn"))
  unzip(file.path(path,"osopenuprn_202406_csv.zip"), exdir = file.path(tempdir(),"uprn"))

  uprn = readr::read_csv(file.path(tempdir(),"uprn","osopenuprn_202406.csv"))
  uprn = uprn[,c("UPRN","X_COORDINATE","Y_COORDINATE")]
  uprn = sf::st_as_sf(uprn, coords = c("X_COORDINATE","Y_COORDINATE"), crs = 27700)
  uprn
}


load_uprn_historical = function(path = "../inputdata/os_uprn/osopenuprn_2020_2025_all.zip"){
  dir.create(file.path(tempdir(),"uprn"))
  unzip(path, exdir = file.path(tempdir(),"uprn"))
  fls = list.files(file.path(tempdir(),"uprn"), pattern = ".csv", recursive = TRUE)
  dts = as.numeric(substr(fls,nchar(fls)[1] - 9, nchar(fls)[1] - 4))
  fls = fls[order(dts)]

  uprn = list()
  for(i in 1:length(fls)){
    sub = readr::read_csv(file.path(tempdir(),"uprn",fls[i]))
    sub$date = lubridate::ym(substr(fls[i],nchar(fls[i]) - 9, nchar(fls[i]) - 4))
    uprn[[i]] = sub
  }
  uprn = dplyr::bind_rows(uprn)

  unlink(file.path(tempdir(),"uprn"), recursive = TRUE)

  uprn = dplyr::group_by(uprn, UPRN) |>
    dplyr::summarise(
                     date_first = min(date),
                     date_last = max(date),
                     X_COORDINATE = last(X_COORDINATE),
                     Y_COORDINATE = last(Y_COORDINATE),
                     LATITUDE = last(LATITUDE),
                     LONGITUDE = last(LONGITUDE)
                     )




  uprn

}
