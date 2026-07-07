#' Load OS Open UPRN points in WGS84
#'
#' @description Unzips and reads the June 2024 OS Open UPRN CSV and returns
#'   every UPRN as an sf point in lng/lat. Used by the `uprn` target. Note the
#'   full dataset is ~40M points, so this is memory-hungry.
#' @param path Folder containing `osopenuprn_202406_csv.zip`.
#' @return An sf POINT data frame (EPSG:4326) with `UPRN`.
#' @keywords internal
load_uprn = function(path = file.path(parameters$path_data,"os_uprn")) {

  dir.create(file.path(tempdir(),"uprn"))
  unzip(file.path(path,"osopenuprn_202406_csv.zip"), exdir = file.path(tempdir(),"uprn"))

  uprn = readr::read_csv(file.path(tempdir(),"uprn","osopenuprn_202406.csv"))
  unlink(file.path(tempdir(),"uprn"), recursive = TRUE)
  uprn = uprn[,c("UPRN","LATITUDE","LONGITUDE")]
  uprn = sf::st_as_sf(uprn, coords = c("LONGITUDE","LATITUDE"), crs = 4326)
  uprn
}


#' Load OS Open UPRN points in British National Grid
#'
#' @description As `load_uprn()` but keeps the easting/northing coordinates
#'   (EPSG:27700) instead of lng/lat. Used by the `uprn_bng` target, which
#'   feeds the Data Zone 2011-to-2022 lookup.
#' @param path Folder containing `osopenuprn_202406_csv.zip`.
#' @return An sf POINT data frame (EPSG:27700) with `UPRN`.
#' @keywords internal
load_uprn_27700 = function(path = file.path(parameters$path_data,"os_uprn")) {

  dir.create(file.path(tempdir(),"uprn"))
  unzip(file.path(path,"osopenuprn_202406_csv.zip"), exdir = file.path(tempdir(),"uprn"))

  uprn = readr::read_csv(file.path(tempdir(),"uprn","osopenuprn_202406.csv"))
  unlink(file.path(tempdir(),"uprn"), recursive = TRUE)
  uprn = uprn[,c("UPRN","X_COORDINATE","Y_COORDINATE")]
  uprn = sf::st_as_sf(uprn, coords = c("X_COORDINATE","Y_COORDINATE"), crs = 27700)
  uprn
}


#' Build first/last-seen dates for UPRNs from historical OS releases
#'
#' @description Reads every monthly OS Open UPRN CSV in the archive
#'   (2020-2025), stamps each with its release date (parsed from the yyyymm in
#'   the file name), and summarises per UPRN: the first and last release it
#'   appears in and its most recent coordinates. Used by the `uprn_historical`
#'   target, which supports matching EPC/Land Registry records to addresses
#'   that have been created or retired over time.
#' @param path Path to `osopenuprn_2020_2025_all.zip` containing the monthly
#'   CSVs.
#' @return A data frame with `UPRN`, `date_first`, `date_last`,
#'   `X_COORDINATE`, `Y_COORDINATE`, `LATITUDE`, `LONGITUDE`.
#' @keywords internal
load_uprn_historical = function(path = "../inputdata/os_uprn/osopenuprn_2020_2025_all.zip"){
  dir.create(file.path(tempdir(),"uprn"))
  unzip(path, exdir = file.path(tempdir(),"uprn"))
  fls = list.files(file.path(tempdir(),"uprn"), pattern = ".csv", recursive = TRUE)
  dts = as.numeric(substr(fls,nchar(fls) - 9, nchar(fls) - 4))
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
