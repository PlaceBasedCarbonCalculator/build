# https://www.nrscotland.gov.uk/statistics-and-data/statistics/statistics-by-theme/population/population-estimates/small-area-population-estimates-2011-data-zone-based/time-series
# Population in 2011 DataZones
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

# Make single population dataset for E,S,W from 2002 to 2021 + E&W 2022
# Note this uses 2011 DataZones but 2021 LSOAs
combine_populations = function(population_2002_2020, population_2021, population_2022, population_scot, lookup_lsoa_2011_21) {

  #TODO: Get Scotland 2022 population
  population_2002_2020$`85+` = population_2002_2020$`85-89` + population_2002_2020$`90+`
  population_2002_2020$`85-89` = NULL
  population_2002_2020$`90+` = NULL

  lookup_lsoa_2011_21 = lookup_lsoa_2011_21[,c("LSOA11CD","LSOA21CD","CHGIND")]

  lookup_lsoa_2011_21_U = lookup_lsoa_2011_21[lookup_lsoa_2011_21$CHGIND == "U",]
  lookup_lsoa_2011_21_M = lookup_lsoa_2011_21[lookup_lsoa_2011_21$CHGIND == "M",]
  lookup_lsoa_2011_21_S = lookup_lsoa_2011_21[lookup_lsoa_2011_21$CHGIND == "S",]
  lookup_lsoa_2011_21_X = lookup_lsoa_2011_21[lookup_lsoa_2011_21$CHGIND == "X",]

  population_2002_2020_U = population_2002_2020[population_2002_2020$LSOA11CD %in% lookup_lsoa_2011_21_U$LSOA11CD,]
  population_2002_2020_M = population_2002_2020[population_2002_2020$LSOA11CD %in% lookup_lsoa_2011_21_M$LSOA11CD,]
  population_2002_2020_S = population_2002_2020[population_2002_2020$LSOA11CD %in% lookup_lsoa_2011_21_S$LSOA11CD,]
  population_2002_2020_X = population_2002_2020[population_2002_2020$LSOA11CD %in% lookup_lsoa_2011_21_X$LSOA11CD,]

  # Merge
  population_2002_2020_M = dplyr::left_join(population_2002_2020_M, lookup_lsoa_2011_21_M, by = "LSOA11CD")
  population_2002_2020_M = dplyr::group_by(population_2002_2020_M, year, LSOA21CD)
  population_2002_2020_M = dplyr::summarise(population_2002_2020_M,
                                            all_ages = sum(all_ages),
                                            `0-4` = sum(`0-4`),
                                            `5-9` = sum(`5-9`),
                                            `10-14` = sum(`10-14`),
                                            `15-19` = sum(`15-19`),
                                            `20-24` = sum(`20-24`),
                                            `25-29` = sum(`25-29`),
                                            `30-34` = sum(`30-34`),
                                            `35-39` = sum(`35-39`),
                                            `40-44` = sum(`40-44`),
                                            `45-49` = sum(`45-49`),
                                            `50-54` = sum(`50-54`),
                                            `55-59` = sum(`55-59`),
                                            `60-64` = sum(`60-64`),
                                            `65-69` = sum(`65-69`),
                                            `70-74` = sum(`70-74`),
                                            `75-79` = sum(`75-79`),
                                            `80-84` = sum(`80-84`),
                                            `85+` = sum(`85+`))
  population_2002_2020_M = dplyr::ungroup(population_2002_2020_M)

  # Split
  split_pop = population_2021[,c("LSOA21","all_ages")]
  names(split_pop) = c("LSOA21","pop2021")
  split_pop = dplyr::left_join(lookup_lsoa_2011_21_S, split_pop, by = c("LSOA21CD" = "LSOA21"))
  split_pop = dplyr::group_by(split_pop, LSOA11CD)
  split_pop = dplyr::mutate(split_pop, pop_ratio = pop2021 / sum(pop2021))
  split_pop = dplyr::ungroup(split_pop, LSOA21CD)

  population_2002_2020_S = dplyr::left_join(split_pop, population_2002_2020_S,
                                            by = "LSOA11CD", relationship = "many-to-many")
  population_2002_2020_S = as.data.frame(population_2002_2020_S)

  for(i in 7:25){
    population_2002_2020_S[i] = round(population_2002_2020_S[,i ,drop = TRUE] * population_2002_2020_S$pop_ratio)
  }

  # Other
  # b11 = bounds_lsoa11_full[bounds_lsoa11_full$LSOA11CD %in% lookup_lsoa_2011_21_X$LSOA11CD,]
  # b21 = bounds_lsoa21_full[bounds_lsoa21_full$LSOA21CD %in% lookup_lsoa_2011_21_X$LSOA21CD,]
  #
  # tm_shape(b11) +
  #   tm_fill("blue", alpha = 0.1) +
  #   tm_borders() +
  # tm_shape(b21) +
  #   tm_fill("red", alpha = 0.1) +
  #   tm_borders()
  # Other changes are very subtle so go for a 1 to 1 match

  lookup_lsoa_2011_21_X = lookup_lsoa_2011_21_X[!(lookup_lsoa_2011_21_X$LSOA11CD == "E01027506" &
                                                    lookup_lsoa_2011_21_X$LSOA21CD == "E01035624"),]
  lookup_lsoa_2011_21_X = lookup_lsoa_2011_21_X[!(lookup_lsoa_2011_21_X$LSOA11CD == "E01008187" &
                                                    lookup_lsoa_2011_21_X$LSOA21CD == "E01035637"),]

  lookup_lsoa_2011_21_X = lookup_lsoa_2011_21_X[!(lookup_lsoa_2011_21_X$LSOA11CD == "E01023964" &
                                                    lookup_lsoa_2011_21_X$LSOA21CD == "E01035581"),]
  lookup_lsoa_2011_21_X = lookup_lsoa_2011_21_X[!(lookup_lsoa_2011_21_X$LSOA11CD == "E01023679" &
                                                    lookup_lsoa_2011_21_X$LSOA21CD == "E01035608"),]

  lookup_lsoa_2011_21_X = lookup_lsoa_2011_21_X[!(lookup_lsoa_2011_21_X$LSOA11CD == "E01023508" &
                                                    lookup_lsoa_2011_21_X$LSOA21CD == "E01035582"),]
  lookup_lsoa_2011_21_X = lookup_lsoa_2011_21_X[!(lookup_lsoa_2011_21_X$LSOA11CD == "E01023768" &
                                                    lookup_lsoa_2011_21_X$LSOA21CD == "E01035609"),]

  population_2002_2020_X = dplyr::left_join(lookup_lsoa_2011_21_X, population_2002_2020_X, by = "LSOA11CD")

  population_2002_2020_U$LSOA21CD = population_2002_2020_U$LSOA11CD

  nms = c("year","LSOA21CD","all_ages","0-4","5-9","10-14","15-19","20-24",
          "25-29","30-34","35-39","40-44","45-49",
          "50-54","55-59","60-64","65-69","70-74","75-79","80-84","85+")

  population_2002_2020_U = population_2002_2020_U[,nms]
  population_2002_2020_M = population_2002_2020_M[,nms]
  population_2002_2020_S = population_2002_2020_S[,nms]
  population_2002_2020_X = population_2002_2020_X[,nms]

  population_2022$year = 2022
  population_2022$`85+` = population_2022$`85-89` + population_2022$`90+`
  population_2022 = population_2022[,nms]

  names(population_2021)[names(population_2021) == "LSOA21"] = "LSOA21CD"

  population_2020_2021 = rbind( population_2002_2020_U,
                                population_2002_2020_M,
                                population_2002_2020_S,
                                population_2002_2020_X,
                                population_2021, population_2022)

  names(population_scot)[names(population_scot) == "LSOA11CD"] = "LSOA21CD"
  population_scot = population_scot[population_scot$year > 2001,]
  population_scot$`85+` = population_scot$`85-89` + population_scot$`90+`
  population_scot  = population_scot[,nms]
  population_2020_2021 = rbind(population_2020_2021, population_scot)
  population_2020_2021

}

# New version that uses the VOA/Council Tax data
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
