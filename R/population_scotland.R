# https://www.nrscotland.gov.uk/statistics-and-data/statistics/statistics-by-theme/population/population-estimates/small-area-population-estimates-2011-data-zone-based/time-series
# Population in 2011 DataZones
#' Download NRS small-area population estimates for Scotland
#'
#' @description Downloads the NRS SAPE time-series spreadsheets (2001-2021,
#'   2011 Data Zones) into `path`. Skipped if the folder already contains
#'   exactly 21 xlsx files. Not currently wired to a target (the
#'   `population_scot` target assumes files are already present).
#' @param path Folder to store the downloads; created if missing.
#' @return `path`.
#' @keywords internal
download_scotland_population = function(path = file.path(parameters$path_data,"population_scotland")){
  if(!dir.exists(path)){
    dir.create(path)
  } else {
    fls = list.files(path, pattern = "xlsx")
    if(length(fls) == 21){
      return(path)
    }
  }

  base_url = "https://www.nrscotland.gov.uk/files//statistics/population-estimates/sape-time-series/sape-"

  for(i in 2001:2021){
    download.file(paste0(base_url,i,".xlsx"), destfile = file.path(path,paste0("pop",i,".xlsx")), mode = "wb")
  }

  return(path)

}


#' Load Scottish population estimates 2001-2022 on 2011 Data Zones
#'
#' @description Reads the NRS SAPE spreadsheets for 2001-2022 (one per year,
#'   `popYYYY.xlsx`, "Persons" sheet), converts single year of age to 5-year
#'   bands and stacks the years. The Data Zone code column is named `LSOA11CD`
#'   for consistency with the E&W data. Used by the `population_scot` target,
#'   which is interpolated onto 2022 Data Zones by
#'   `interpolate_population_dz11_dz22()`. Note: expects `pop2022.xlsx` to
#'   exist even though `download_scotland_population()` only fetches to 2021.
#' @param path Folder of `popYYYY.xlsx` files.
#' @return A data frame with `year`, `LSOA11CD` (2011 DZ code), `all_ages` and
#'   5-year age-band columns "0-4" ... "90+".
#' @keywords internal
load_scotland_population = function(path = file.path(parameters$path_data,"population_scotland")){

  pops = list()
  for(i in 2001:2022){
    sub = readxl::read_excel(file.path(path,paste0("pop",i,".xlsx")), sheet = "Persons")
    names(sub) = as.character(sub[3,])
    sub = sub[4:nrow(sub),]
    sub$`Data zone name` = NULL
    sub$`Council area code` = NULL
    sub$`Council area name` = NULL
    names(sub)[1] = "LSOA11CD"
    sub[2:ncol(sub)] = lapply(sub[2:ncol(sub)], as.numeric)

    # Convert to 5 year bands
    for(j in seq(0,85,5)){
      sub[paste0(j,"-",j+4)] = rowSums(sub[,paste0("Age ",j:(j+4))])
    }
    names(sub)[names(sub) == "Total population"] = "all_ages"
    names(sub)[names(sub) == "Age 90 and over"] = "90+"
    sub = sub[,!grepl("Age ",names(sub))]
    pops[[i]] = sub
    rm(sub)
  }

  pops = dplyr::bind_rows(pops, .id = "year")
  pops$year = as.numeric(pops$year) + 2000

  pops
}


#' Combine the E&W and Scottish population/household series into one GB table
#'
#' @description Builds the master `population` target by appending the
#'   Scottish 2022 Data Zone series (from
#'   `interpolate_population_dz11_dz22()`) to the E&W series (from
#'   `extrapolate_population_households()`, which uses VOA/council-tax data).
#'   Scottish age bands 85-89 and 90+ are collapsed to "85+", column names are
#'   aligned (`DataZone22` -> `LSOA21CD`, `households` -> `households_est`)
#'   and an adult count is added for Scotland.
#' @param population_households_historical E&W population and households per
#'   2021 LSOA per year.
#' @param population_scot_dz22 Scottish population and households per 2022
#'   Data Zone per year.
#' @return A GB data frame with `year`, `LSOA21CD`, `all_ages`, age bands
#'   "0-4" ... "85+", `all_properties`, `households_est`, `adults` and
#'   `adults_per_household` (NA for Scotland).
#' @keywords internal
combine_populations2 = function(population_households_historical, population_scot_dz22) {



  nms = c("year","LSOA21CD","all_ages","0-4","5-9","10-14","15-19","20-24",
          "25-29","30-34","35-39","40-44","45-49",
          "50-54","55-59","60-64","65-69","70-74","75-79","80-84","85+","all_properties","households_est")

  names(population_scot_dz22)[names(population_scot_dz22) == "DataZone22"] = "LSOA21CD"
  names(population_scot_dz22)[names(population_scot_dz22) == "households"] = "households_est"
  #population_scot = population_scot[population_scot$year > 2001,]
  population_scot_dz22$`85+` = population_scot_dz22$`85-89` + population_scot_dz22$`90+`
  population_scot_dz22  = population_scot_dz22[,nms]

  #dwellings_tax_band_scotland = dwellings_tax_band_scotland[,c("LSOA11CD","year","all_properties")]

  # scot2  = dplyr::left_join(population_scot, dwellings_tax_band_scotland,
  #                         by = c("LSOA21CD" = "LSOA11CD",
  #                                "year" = "year"
  #                                ))
  scot2 = population_scot_dz22

  scot2$adults_per_household = NA
  scot2$adults = rowSums(scot2[,c("20-24","25-29","30-34","35-39","40-44","45-49",
                                  "50-54","55-59","60-64","65-69","70-74","75-79",
                                  "80-84","85+")])


  population = rbind(population_households_historical, scot2)
  population

}
