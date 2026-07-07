#' Load CDRC dwelling ages per 2011 LSOA
#'
#' @description Reads the CDRC/VOA property age CSV (secure data). Used by
#'   the `building_age_2011` target. Note the first column is (mis)named
#'   `LSAO11CD` to match the source file.
#' @param path Folder containing `voapropertyage.csv`.
#' @return A data frame of dwelling counts by build period per 2011 LSOA.
#' @keywords internal
load_building_age_2011 = function(path = file.path(parameters$path_secure_data,"CDRC/building age price")) {
  age = read.csv(file.path(path,"voapropertyage.csv"))
  names(age)[1] = "LSAO11CD"
  age
}

#' Load 2021 census accommodation type (TS044) for LSOAs
#'
#' @description Reads households by accommodation type (detached, semi,
#'   terraced, flats, etc.) from the downloaded `census2021-ts044.zip`. Used
#'   by the `housing_type_2021` target.
#' @param path Folder of Nomis downloads (`dl_nomis` target).
#' @return A data frame with `LSOA21CD`, `all_households` and dwelling-type
#'   counts.
#' @keywords internal
load_housing_type_2021 = function(path = file.path(parameters$path_data,"nomis")){
  dat = unzip_nomis(file.path(path,"census2021-ts044.zip"))
  names(dat) = c("year","LSOA21NM","LSOA21CD","all_households",
                 "detached","semidetached","terraced","flats_purposebuilt",
                 "flats_converted","converted_building",
                 "dwelling_in_commerical_building","mobile_tempoary")
  dat = dat[,3:ncol(dat)]
  dat
}


#' Load 2021 census central heating (TS046) for LSOAs
#'
#' @description Reads households by central heating type from the downloaded
#'   `census2021-ts046.zip`. Used by the `central_heating_2021` target, an
#'   input to `calculate_other_heating()`.
#' @param path Folder of Nomis downloads (`dl_nomis` target).
#' @return A data frame with `LSOA21CD`, `all_households` and heating-type
#'   counts (including wood, renewables, heat networks and mixed types).
#' @keywords internal
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



