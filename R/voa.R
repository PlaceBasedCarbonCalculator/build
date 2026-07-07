#Counts in the tables are rounded to the nearest 10 with counts of zero being
#reported as ‘0’ and counts fewer than five reported as negligible and denoted
#by ‘-’.

#' Load VOA CTSOP1.1: dwellings by council tax band per LSOA, 1993-2024
#'
#' @description Unzips and reads the VOA "counts of properties by council tax
#'   band" annual CSVs, keeping LSOA rows. Counts are rounded to the nearest
#'   10 by VOA, with fewer-than-five shown as "-" (read as NA). Used by the
#'   `dwellings_tax_band` target - the key annual dwelling-count series for
#'   the household extrapolation and VOA JSONs.
#' @param path Folder containing `CTSOP1-1-1993-2024.zip`.
#' @return A data frame with `ecode` (LSOA code), `year`, `band_a` ...
#'   `band_i` and `all_properties`.
#' @keywords internal
load_voa_CTSOP1 = function(path = "../inputdata/voa/"){

  tmp_dir = file.path(tempdir(),"voa")
  dir.create(tmp_dir)
  unzip(file.path(path,"CTSOP1-1-1993-2024.zip"), exdir = tmp_dir)

  ctsop1 = list()
  fls = list.files(tmp_dir, pattern = ".csv")
  coltype = readr::cols(geography = readr::col_character(),
                         ba_code = readr::col_character(),
                         ecode = readr::col_character(),
                         area_name = readr::col_character(),
                         band_a = readr::col_integer(),
                         band_b = readr::col_integer(),
                         band_c = readr::col_integer(),
                         band_d = readr::col_integer(),
                         band_e = readr::col_integer(),
                         band_f = readr::col_integer(),
                         band_g = readr::col_integer(),
                         band_h = readr::col_integer(),
                         band_i = readr::col_integer(),
                         all_properties = readr::col_integer())

  for(i in seq_along(fls)){
    sub = readr::read_csv(file.path(tmp_dir, fls[i]), col_types = coltype)
    sub = sub[sub$geography == "LSOA",]
    sub$year  = as.integer(substr(fls[i],10,13))
    ctsop1[[i]] = sub
  }

  ctsop1 = dplyr::bind_rows(ctsop1)
  ctsop1 = ctsop1[,c("ecode","year","band_a","band_b","band_c","band_d","band_e",
                     "band_f","band_g","band_h","band_i","all_properties")]

  unlink(tmp_dir, recursive = TRUE)

  ctsop1

}

#' Load VOA CTSOP3.1: dwellings by type and bedrooms per LSOA, 2020-2024
#'
#' @description Unzips and reads the VOA "counts of properties by type and
#'   number of bedrooms" CSVs (bungalow / flat-maisonette / terraced / semi /
#'   detached x 1-6+ bedrooms, per council tax band), keeping LSOA rows.
#'   Used by the `dwellings_type` target.
#' @param path Folder containing `CTSOP3-1-2020-2024.zip`.
#' @return A data frame with `ecode`, `band`, `year` and type-by-bedroom
#'   count columns.
#' @keywords internal
load_voa_CTSOP3 = function(path = "../inputdata/voa/"){

  tmp_dir = file.path(tempdir(),"voa")
  dir.create(tmp_dir)
  unzip(file.path(path,"CTSOP3-1-2020-2024.zip"), exdir = tmp_dir)
  ctsop3 = list()
  fls = list.files(tmp_dir, pattern = ".csv")

  coltype = readr::cols(geography = readr::col_character(),
                        ba_code = readr::col_character(),
                        ecode = readr::col_character(),
                        area_name = readr::col_character(),
                        band = readr::col_character(),
                        bungalow_1 = readr::col_integer(),
                        bungalow_2 = readr::col_integer(),
                        bungalow_3 = readr::col_integer(),
                        bungalow_4 = readr::col_integer(),
                        bungalow_5 = readr::col_integer(),
                        bungalow_6 = readr::col_integer(),
                        bungalow_unkw = readr::col_integer(),
                        bungalow_total = readr::col_integer(),
                        flat_mais_1 = readr::col_integer(),
                        flat_mais_2 = readr::col_integer(),
                        flat_mais_3 = readr::col_integer(),
                        flat_mais_4 = readr::col_integer(),
                        flat_mais_5 = readr::col_integer(),
                        flat_mais_6 = readr::col_integer(),
                        flat_mais_unkw = readr::col_integer(),
                        flat_mais_total = readr::col_integer(),
                        house_terraced_1 = readr::col_integer(),
                        house_terraced_2 = readr::col_integer(),
                        house_terraced_3 = readr::col_integer(),
                        house_terraced_4 = readr::col_integer(),
                        house_terraced_5 = readr::col_integer(),
                        house_terraced_6 = readr::col_integer(),
                        house_terraced_unkw   = readr::col_integer(),
                        house_terraced_total = readr::col_integer(),
                        house_semi_1 = readr::col_integer(),
                        house_semi_2 = readr::col_integer(),
                        house_semi_3 = readr::col_integer(),
                        house_semi_4 = readr::col_integer(),
                        house_semi_5 = readr::col_integer(),
                        house_semi_6 = readr::col_integer(),
                        house_semi_unkw = readr::col_integer(),
                        house_semi_total = readr::col_integer(),
                        house_detached_1 = readr::col_integer(),
                        house_detached_2 = readr::col_integer(),
                        house_detached_3 = readr::col_integer(),
                        house_detached_4 = readr::col_integer(),
                        house_detached_5 = readr::col_integer(),
                        house_detached_6 = readr::col_integer(),
                        house_detached_unkw = readr::col_integer(),
                        house_detached_total = readr::col_integer(),
                        annexe = readr::col_integer(),
                        caravan_houseboat_mobilehome = readr::col_integer(),
                        unknown = readr::col_integer(),
                        all_properties = readr::col_integer())

  for(i in seq_along(fls)){
    sub = readr::read_csv(file.path(tmp_dir, fls[i]), col_types = coltype)
    sub = sub[sub$geography == "LSOA",]
    sub$year  = as.integer(substr(fls[i],10,13))
    ctsop3[[i]] = sub
  }

  ctsop3 = dplyr::bind_rows(ctsop3)

  unlink(tmp_dir, recursive = TRUE)

  ctsop3$geography = NULL
  ctsop3$ba_code = NULL
  ctsop3$geography = NULL
  ctsop3$area_name = NULL

  ctsop3

}

#' Load VOA CTSOP4.1: dwellings by build period per LSOA, 2020-2024
#'
#' @description Unzips and reads the VOA "counts of properties by build
#'   period" CSVs (pre-1900 through 2022-2024, per council tax band), keeping
#'   LSOA rows. Used by the `dwellings_age` target.
#' @param path Folder containing `CTSOP4-1-2020-2024.zip`.
#' @return A data frame with `ecode`, `band`, `year` and `bp_*` build-period
#'   count columns.
#' @keywords internal
load_voa_CTSOP4 = function(path = "../inputdata/voa/"){

  tmp_dir = file.path(tempdir(),"voa")
  dir.create(tmp_dir)
  unzip(file.path(path,"CTSOP4-1-2020-2024.zip"), exdir = tmp_dir)
  ctsop4 = list()
  fls = list.files(tmp_dir, pattern = ".csv")

  coltype = readr::cols(geography = readr::col_character(),
                        ba_code = readr::col_character(),
                        ecode = readr::col_character(),
                        area_name = readr::col_character(),
                        band = readr::col_character(),
                        bp_pre_1900 = readr::col_integer(),
                        bp_1900_1918 = readr::col_integer(),
                        bp_1919_1929 = readr::col_integer(),
                        bp_1930_1939 = readr::col_integer(),
                        bp_1945_1954 = readr::col_integer(),
                        bp_1955_1964 = readr::col_integer(),
                        bp_1965_1972 = readr::col_integer(),
                        bp_1973_1982 = readr::col_integer(),
                        bp_1983_1992 = readr::col_integer(),
                        bp_1993_1999 = readr::col_integer(),
                        bp_2000_2008 = readr::col_integer(),
                        bp_2009 = readr::col_integer(),
                        bp_2010 = readr::col_integer(),
                        bp_2011 = readr::col_integer(),
                        bp_2012 = readr::col_integer(),
                        bp_2013 = readr::col_integer(),
                        bp_2014 = readr::col_integer(),
                        bp_2015 = readr::col_integer(),
                        bp_2016 = readr::col_integer(),
                        bp_2017 = readr::col_integer(),
                        bp_2018 = readr::col_integer(),
                        bp_2019 = readr::col_integer(),
                        bp_2020  = readr::col_integer(),
                        bp_2021 = readr::col_integer(),
                        bp_2022_2024 = readr::col_integer(),
                        bp_unkw = readr::col_integer(),
                        all_properties = readr::col_integer())

  for(i in seq_along(fls)){
    sub = readr::read_csv(file.path(tmp_dir, fls[i]), col_types = coltype)
    sub = sub[sub$geography == "LSOA",]
    sub$year  = as.integer(substr(fls[i],10,13))
    ctsop4[[i]] = sub
  }

  ctsop4 = dplyr::bind_rows(ctsop4)

  unlink(tmp_dir, recursive = TRUE)

  ctsop4$geography = NULL
  ctsop4$ba_code = NULL
  ctsop4$geography = NULL
  ctsop4$area_name = NULL

  ctsop4

}

#' Prepare the council-tax-band series for JSON export (2010+)
#'
#' @description Filters the CTSOP1 series to 2010 onwards and shortens the
#'   column names (banda, bandb, ...) for the per-zone JSON files. Used by
#'   the `voa_json_2010` target.
#' @param dwellings_tax_band CTSOP1 table (`dwellings_tax_band` target).
#' @return A data frame with `LSOA21CD`, `year` and band counts, sorted.
#' @keywords internal
summarise_voa_post2010 = function(dwellings_tax_band) {
  dwellings_tax_band = dwellings_tax_band[dwellings_tax_band$year >= 2010,]
  names(dwellings_tax_band) = gsub("_","",names(dwellings_tax_band))
  names(dwellings_tax_band)[1] = "LSOA21CD"
  dwellings_tax_band = dwellings_tax_band[order(dwellings_tax_band$LSOA21CD, dwellings_tax_band$year),]

  dwellings_tax_band

}

#' Combine dwelling type/bedrooms and build period for JSON export (2020+)
#'
#' @description Takes the all-bands rows of the CTSOP3 and CTSOP4 tables,
#'   derives bedroom-count totals across dwelling types, collapses the
#'   2009-2021 single-year build periods into one band, shortens column
#'   names and joins the two tables. Used by the `voa_json_2020` target.
#' @param dwellings_type CTSOP3 table (`dwellings_type` target).
#' @param dwellings_age CTSOP4 table (`dwellings_age` target).
#' @return A data frame per LSOA-year of dwelling type, bedroom and build
#'   period counts.
#' @keywords internal
summarise_voa_post2020 = function(dwellings_type, dwellings_age) {
  dwellings_type = dwellings_type[dwellings_type$band == "All",]
  dwellings_age = dwellings_age[dwellings_age$band == "All",]

  for(i in 1:6){
    dwellings_type[paste0("bed",i)] = rowSums(dwellings_type[,grepl(paste0("_",i),names(dwellings_type))], na.rm = TRUE)
  }

  dwellings_type = dwellings_type[,c("ecode","year","bungalow_total","flat_mais_total",
                                     "house_terraced_total","house_semi_total",
                                     "house_detached_total","annexe",
                                     "caravan_houseboat_mobilehome","unknown","all_properties",
                                     "bed1","bed2","bed3","bed4","bed5","bed6")]
  names(dwellings_type) = gsub("_total","",names(dwellings_type))
  names(dwellings_type) = gsub("_","",names(dwellings_type))
  names(dwellings_type) = gsub("house","",names(dwellings_type))
  names(dwellings_type)[1] = "LSOA21CD"

  dwellings_age$bp_2009_2021 = rowSums(
    dwellings_age[,c("bp_2009","bp_2010","bp_2011","bp_2012","bp_2013","bp_2014",
                     "bp_2015","bp_2016","bp_2017","bp_2018","bp_2019","bp_2020",
                     "bp_2021")], na.rm = TRUE)

  dwellings_age = dwellings_age[,c("ecode","year","bp_pre_1900","bp_1900_1918","bp_1919_1929",
                                   "bp_1930_1939","bp_1945_1954","bp_1955_1964","bp_1965_1972",
                                   "bp_1973_1982","bp_1983_1992","bp_1993_1999","bp_2000_2008",
                                   "bp_2009_2021","bp_2022_2024","bp_unkw")]
  names(dwellings_age) = gsub("_","",names(dwellings_age))
  names(dwellings_age)[1] = "LSOA21CD"

  dwellings = dplyr::left_join(dwellings_type, dwellings_age, by = c("LSOA21CD","year"))
  dwellings = dwellings[order(dwellings$LSOA21CD, dwellings$year),]
  dwellings
}
