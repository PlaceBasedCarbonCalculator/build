# dowload_gas_electric <- function(path = file.path(data_path(),"gas_electric")){
#   if(!dir.exists(path)){
#     dir.create(path)
#   } else {
#     fls = list.files(path, pattern = "xlsx")
#     if(length(fls) > 3){
#       return(path)
#     }
#   }
#
#   url_elec = "https://assets.publishing.service.gov.uk/media/63a2ea3fd3bf7f375b61c12d/LSOA_domestic_elec_2010-21.xlsx"
#   download.file(url_elec, file.path(path,"lsoa_elec_dom.xlsx"), mode = "wb")
#
#   url_elec = "https://assets.publishing.service.gov.uk/media/63a2e9e58fa8f5390dfdf56b/MSOA_non-domestic_elec_2010-21.xlsx"
#   download.file(url_elec, file.path(path,"msoa_elec_nondom.xlsx"), mode = "wb")
#
#   url_gas = "https://assets.publishing.service.gov.uk/media/63a2ef098fa8f53913e8071b/LSOA_domestic_gas_2010-21.xlsx"
#   download.file(url_gas, file.path(path,"lsoa_gas_dom.xlsx"), mode = "wb")
#
#   url_gas = "https://assets.publishing.service.gov.uk/media/63a31f44e90e075870e2cf0f/MSOA_non-domestic_gas_2010-21.xlsx"
#   download.file(url_gas, file.path(path,"msoa_gas_nondom.xlsx"), mode = "wb")
#
#   return(path)
#
# }



load_lsoa_electric <- function(path){

  elec = list()
  for(i in 2010:2024){
    sub <- readxl::read_excel(file.path(path,"LSOA_domestic_elec_2010-2024.xlsx"),
                              sheet = as.character(i))
    sub <- as.data.frame(sub)
    sub <- sub[5:nrow(sub),]
    names(sub) <- c("LAcode","LAname",
                    "MSOA","MSOAname",
                    "LSOA","LSOAname",
                    "meters","total_elec_kwh",
                    "mean_elec_kwh","median_elec_kwh")

    sub$year <- i
    elec[[i]] <- sub
  }
  elec = dplyr::bind_rows(elec)

  elec$meters = as.numeric(elec$meters)
  elec$total_elec_kwh = as.numeric(elec$total_elec_kwh)
  elec$mean_elec_kwh = as.numeric(elec$mean_elec_kwh)
  elec$median_elec_kwh = as.numeric(elec$median_elec_kwh)

  elec$meters[is.na(elec$meters)] = 0
  elec$total_elec_kwh[is.na(elec$total_elec_kwh)] = 0
  elec$mean_elec_kwh[is.na(elec$mean_elec_kwh)] = 0
  elec$median_elec_kwh[is.na(elec$median_elec_kwh)] = 0

  elec


}



load_msoa_electric_nondom <- function(path){



  elec = list()
  for(i in 2010:2024){
    sub <- readxl::read_excel(file.path(path,"MSOA_non-domestic_elec_2010-2024.xlsx"),
                              sheet = as.character(i))
    sub <- as.data.frame(sub)
    sub <- sub[5:nrow(sub),]
    names(sub) <- c("LAcode","LAname",
                    "MSOA","MSOAname",
                    "meter_type",
                    "meters","total_elec_kwh",
                    "mean_elec_kwh","median_elec_kwh")

    sub$year <- i
    elec[[i]] <- sub
  }
  elec = dplyr::bind_rows(elec)
  unlink(file.path(tempdir(),"gaselec"), recursive = TRUE)

  elec$meters = as.numeric(elec$meters)
  elec$total_elec_kwh = as.numeric(elec$total_elec_kwh)
  elec$mean_elec_kwh = as.numeric(elec$mean_elec_kwh)
  elec$median_elec_kwh = as.numeric(elec$median_elec_kwh)

  elec$total_elec_kwh[is.na(elec$total_elec_kwh)] = 0
  elec$mean_elec_kwh[is.na(elec$mean_elec_kwh)] = 0
  elec$median_elec_kwh[is.na(elec$median_elec_kwh)] = 0



  elec

}



load_lsoa_gas <- function(path){



  gas = list()
  for(i in 2010:2024){
    sub <- readxl::read_excel(file.path(path,"LSOA_domestic_gas_2010-2024.xlsx"),
                              sheet = as.character(i))
    sub <- as.data.frame(sub)
    sub <- sub[5:nrow(sub),]

    if(ncol(sub) == 10){
      names(sub) <- c("LAcode","LAname",
                      "MSOA","MSOAname",
                      "LSOA","LSOAname",
                      "meters","total_gas_kwh",
                      "mean_gas_kwh","median_gas_kwh")
    } else {
      names(sub) <- c("LAcode","LAname",
                      "MSOA","MSOAname",
                      "LSOA","LSOAname",
                      "meters","total_gas_kwh",
                      "mean_gas_kwh","median_gas_kwh",
                      "nonconsump_meters")
    }



    sub$year <- i
    gas[[i]] <- sub
  }
  gas = dplyr::bind_rows(gas)
  unlink(file.path(tempdir(),"gaselec"), recursive = TRUE)

  gas$meters = as.numeric(gas$meters)
  gas$total_gas_kwh = as.numeric(gas$total_gas_kwh)
  gas$mean_gas_kwh = as.numeric(gas$mean_gas_kwh)
  gas$median_gas_kwh = as.numeric(gas$median_gas_kwh)

  gas$meters[is.na(gas$meters)] = 0
  gas$total_gas_kwh[is.na(gas$total_gas_kwh)] = 0
  gas$mean_gas_kwh[is.na(gas$mean_gas_kwh)] = 0
  gas$median_gas_kwh[is.na(gas$median_gas_kwh)] = 0

  gas


}



load_msoa_gas_nondom <- function(path){

  gas = list()
  for(i in 2010:2021){
    sub <- readxl::read_excel(file.path(path,"MSOA_non_domestic_gas_2010-2024.xlsx"),
                              sheet = as.character(i))
    sub <- as.data.frame(sub)
    sub <- sub[5:nrow(sub),]
    names(sub) <- c("LAcode","LAname",
                    "MSOA","MSOAname",
                    "meters","total_gas_kwh",
                    "mean_gas_kwh","median_gas_kwh")

    sub$year <- i
    gas[[i]] <- sub
  }
  gas = dplyr::bind_rows(gas)


  gas$meters = as.numeric(gas$meters)
  gas$total_gas_kwh = as.numeric(gas$total_gas_kwh)
  gas$mean_gas_kwh = as.numeric(gas$mean_gas_kwh)
  gas$median_gas_kwh = as.numeric(gas$median_gas_kwh)

  gas

}

lsoa_gas_to_2021 <- function(domestic_gas_11, lsoa_11_21_tools, lookup_dz_2011_22_pre){

  #Update new data uses 2021/22 boundaries from 2015 onwards!
  domestic_gas_11 = domestic_gas_11[,c("LSOA","year","meters","total_gas_kwh","mean_gas_kwh","median_gas_kwh")]
  domestic_gas_11 = domestic_gas_11[domestic_gas_11$LSOA != "Unallocated",]

  domestic_gas_11_done = domestic_gas_11[domestic_gas_11$year >= 2015,]
  names(domestic_gas_11_done)[1] = "LSOA21CD"
  domestic_gas_11 = domestic_gas_11[domestic_gas_11$year < 2015,]
  names(domestic_gas_11)[1] = "LSOA11CD"


  # Scotland
  lookup_dz_2011_22_pre = sf::st_drop_geometry(lookup_dz_2011_22_pre)
  # Share of the 2011 households
  lookup_dz_2011_22_pre = lookup_dz_2011_22_pre |>
    dplyr::group_by(DataZone) |>
    dplyr::mutate(splitshare = count / sum(count)) |>
    dplyr::ungroup()

  lookup_dz_2011_22_pre = lookup_dz_2011_22_pre[,c("DataZone","DataZone22","splitshare")]
  names(lookup_dz_2011_22_pre) = c("LSOA11CD","LSOA21CD","splitshare")

  domestic_gas_Scot = domestic_gas_11[domestic_gas_11$LSOA11CD %in% lookup_dz_2011_22_pre$LSOA11CD,]

  domestic_gas_Scot = dplyr::left_join(domestic_gas_Scot,
                                       lookup_dz_2011_22_pre,
                                       by = c("LSOA11CD"),
                                       relationship = "many-to-many")

  domestic_gas_Scot = dplyr::group_by(domestic_gas_Scot, year, LSOA21CD) |>
    dplyr::summarise(total_gas_kwh = round(sum(total_gas_kwh * splitshare)),
                     mean_gas_kwh = weighted.mean(mean_gas_kwh, splitshare),
                     median_gas_kwh = weighted.mean(median_gas_kwh, splitshare),
                     meters = round(sum(meters * splitshare))) |>
    dplyr::ungroup()


  # England and Wales

  domestic_gas_S = domestic_gas_11[domestic_gas_11$LSOA11CD %in% lsoa_11_21_tools$lookup_split$LSOA11CD,]
  domestic_gas_M = domestic_gas_11[domestic_gas_11$LSOA11CD %in% lsoa_11_21_tools$lookup_merge$LSOA11CD,]
  domestic_gas_U = domestic_gas_11[domestic_gas_11$LSOA11CD %in% lsoa_11_21_tools$lookup_unchanged$LSOA11CD,]

  #Unchanged
  domestic_gas_U = dplyr::left_join(domestic_gas_U, lsoa_11_21_tools$lookup_unchanged, by = "LSOA11CD")

  # Merge
  domestic_gas_M = dplyr::left_join(domestic_gas_M, lsoa_11_21_tools$lookup_merge, by = "LSOA11CD")
  domestic_gas_M = dplyr::group_by(domestic_gas_M, year, LSOA21CD)
  domestic_gas_M = dplyr::summarise(domestic_gas_M,
                                    total_gas_kwh = sum(total_gas_kwh, na.rm = TRUE),
                                    mean_gas_kwh = weighted.mean(mean_gas_kwh, meters, na.rm = TRUE),
                                    median_gas_kwh = weighted.mean(median_gas_kwh, meters, na.rm = TRUE),
                                    meters = sum(meters, na.rm = TRUE))
  domestic_gas_M = dplyr::ungroup(domestic_gas_M)

  domestic_gas_M$mean_gas_kwh = ifelse(is.nan(domestic_gas_M$mean_gas_kwh), 0, domestic_gas_M$mean_gas_kwh)
  domestic_gas_M$median_gas_kwh = ifelse(is.nan(domestic_gas_M$median_gas_kwh), 0, domestic_gas_M$median_gas_kwh)

  #Split
  lookup_split = lsoa_11_21_tools$lookup_split
  lookup_split = lookup_split[,c("LSOA11CD","LSOA21CD","year","household_ratio")]
  lookup_split = lookup_split[lookup_split$year %in% unique(domestic_gas_S$year),]
  domestic_gas_S = dplyr::left_join(lookup_split, domestic_gas_S,
                                    by = c("LSOA11CD", "year"),
                                    relationship = "many-to-many")
  domestic_gas_S = as.data.frame(domestic_gas_S)

  domestic_gas_S$meters = domestic_gas_S$meters * domestic_gas_S$household_ratio
  domestic_gas_S$total_gas_kwh = domestic_gas_S$total_gas_kwh * domestic_gas_S$household_ratio
  domestic_gas_S$mean_gas_kwh = domestic_gas_S$total_gas_kwh / domestic_gas_S$meters
  domestic_gas_S$mean_gas_kwh = ifelse(is.nan(domestic_gas_S$mean_gas_kwh),0,domestic_gas_S$mean_gas_kwh)

  #TODO: How do you get the median of a subgroup? For now assuming unchanged
  domestic_gas_S$median_gas_kwh = ifelse(domestic_gas_S$mean_gas_kwh == 0,0,domestic_gas_S$median_gas_kwh)


  nms = c("LSOA21CD","year","meters","total_gas_kwh","mean_gas_kwh","median_gas_kwh")

  domestic_gas_S = domestic_gas_S[,nms]
  domestic_gas_M = domestic_gas_M[,nms]
  domestic_gas_U = domestic_gas_U[,nms]
  domestic_gas_Scot = domestic_gas_Scot[,nms]

  final = rbind(domestic_gas_S, domestic_gas_M, domestic_gas_U, domestic_gas_Scot, domestic_gas_11_done)

  final

}

lsoa_electric_to_2021 <- function(domestic_electricity_11, lsoa_11_21_tools, lookup_dz_2011_22_pre){

  domestic_electricity_11 = domestic_electricity_11[,c("LSOA","year","meters","total_elec_kwh","mean_elec_kwh","median_elec_kwh")]
  domestic_electricity_11 = domestic_electricity_11[domestic_electricity_11$LSOA != "Unallocated",]

  #Update new data uses 2021/22 boundaries from 2015 onwards!
  domestic_electricity_11_done = domestic_electricity_11[domestic_electricity_11$year >= 2015,]
  names(domestic_electricity_11_done)[1] = "LSOA21CD"
  domestic_electricity_11 = domestic_electricity_11[domestic_electricity_11$year < 2015,]
  names(domestic_electricity_11)[1] = "LSOA11CD"


  # Scotland
  lookup_dz_2011_22_pre = sf::st_drop_geometry(lookup_dz_2011_22_pre)
  # Share of the 2011 households
  lookup_dz_2011_22_pre = lookup_dz_2011_22_pre |>
    dplyr::group_by(DataZone) |>
    dplyr::mutate(splitshare = count / sum(count)) |>
    dplyr::ungroup()

  lookup_dz_2011_22_pre = lookup_dz_2011_22_pre[,c("DataZone","DataZone22","splitshare")]
  names(lookup_dz_2011_22_pre) = c("LSOA11CD","LSOA21CD","splitshare")

  domestic_electricity_Scot = domestic_electricity_11[domestic_electricity_11$LSOA11CD %in% lookup_dz_2011_22_pre$LSOA11CD,]

  domestic_electricity_Scot = dplyr::left_join(domestic_electricity_Scot,
                                       lookup_dz_2011_22_pre,
                                       by = c("LSOA11CD"),
                                       relationship = "many-to-many")

  domestic_electricity_Scot = dplyr::group_by(domestic_electricity_Scot, year, LSOA21CD) |>
    dplyr::summarise(total_elec_kwh = round(sum(total_elec_kwh * splitshare)),
                     mean_elec_kwh = weighted.mean(mean_elec_kwh, splitshare),
                     median_elec_kwh = weighted.mean(median_elec_kwh, splitshare),
                     meters = round(sum(meters * splitshare))) |>
    dplyr::ungroup()


  # England and Wales

  domestic_electricity_S = domestic_electricity_11[domestic_electricity_11$LSOA11CD %in% lsoa_11_21_tools$lookup_split$LSOA11CD,]
  domestic_electricity_M = domestic_electricity_11[domestic_electricity_11$LSOA11CD %in% lsoa_11_21_tools$lookup_merge$LSOA11CD,]
  domestic_electricity_U = domestic_electricity_11[domestic_electricity_11$LSOA11CD %in% lsoa_11_21_tools$lookup_unchanged$LSOA11CD,]

  #Unchanged
  domestic_electricity_U = dplyr::left_join(domestic_electricity_U, lsoa_11_21_tools$lookup_unchanged, by = "LSOA11CD")

  # Merge
  domestic_electricity_M = dplyr::left_join(domestic_electricity_M, lsoa_11_21_tools$lookup_merge, by = "LSOA11CD")
  domestic_electricity_M = dplyr::group_by(domestic_electricity_M, year, LSOA21CD)
  domestic_electricity_M = dplyr::summarise(domestic_electricity_M,
                                            total_elec_kwh = sum(total_elec_kwh, na.rm = TRUE),
                                            mean_elec_kwh = weighted.mean(mean_elec_kwh, meters, na.rm = TRUE),
                                            median_elec_kwh = weighted.mean(median_elec_kwh, meters, na.rm = TRUE),
                                            meters = sum(meters, na.rm = TRUE))
  domestic_electricity_M = dplyr::ungroup(domestic_electricity_M)

  lookup_split = lsoa_11_21_tools$lookup_split
  lookup_split = lookup_split[,c("LSOA11CD","LSOA21CD","year","household_ratio")]
  lookup_split = lookup_split[lookup_split$year %in% unique(domestic_electricity_S$year),]
  domestic_electricity_S = dplyr::left_join(lookup_split, domestic_electricity_S,
                                    by = c("LSOA11CD", "year"),
                                    relationship = "many-to-many")
  domestic_electricity_S = as.data.frame(domestic_electricity_S)

  domestic_electricity_S$meters = domestic_electricity_S$meters * domestic_electricity_S$household_ratio
  domestic_electricity_S$total_elec_kwh = domestic_electricity_S$total_elec_kwh * domestic_electricity_S$household_ratio
  domestic_electricity_S$mean_elec_kwh = domestic_electricity_S$total_elec_kwh / domestic_electricity_S$meters

  #TODO: How do you get the median of a subgroup? For now assuming unchanged


  #Split
  # domestic_electricity_S = dplyr::left_join(lsoa_11_21_tools$lookup_split, domestic_electricity_S,
  #                                           by = "LSOA11CD", relationship = "many-to-many")
  # domestic_electricity_S = as.data.frame(domestic_electricity_S)
  # for(i in 5:6){
  #   domestic_electricity_S[i] = domestic_electricity_S[,i ,drop = TRUE] * domestic_electricity_S$pop_ratio
  # }

  nms = c("LSOA21CD","year","meters","total_elec_kwh","mean_elec_kwh","median_elec_kwh")

  domestic_electricity_S = domestic_electricity_S[,nms]
  domestic_electricity_M = domestic_electricity_M[,nms]
  domestic_electricity_U = domestic_electricity_U[,nms]
  domestic_electricity_Scot = domestic_electricity_Scot[,nms]

  final = rbind(domestic_electricity_S, domestic_electricity_M, domestic_electricity_U, domestic_electricity_Scot, domestic_electricity_11_done)
  final

}



lsoa_convert_2011_2021_pre_data = function(lookup_lsoa_2011_21, population) {

  lookup_lsoa_2011_21 = lookup_lsoa_2011_21[,c("LSOA11CD","LSOA21CD","CHGIND")]
  #population_2021 = population_2021[,c("LSOA21","all_ages")]
  #names(population_2021) = c("LSOA21","pop2021")

  lookup_lsoa_2011_21_U = lookup_lsoa_2011_21[lookup_lsoa_2011_21$CHGIND == "U",]
  lookup_lsoa_2011_21_M = lookup_lsoa_2011_21[lookup_lsoa_2011_21$CHGIND == "M",]
  lookup_lsoa_2011_21_S = lookup_lsoa_2011_21[lookup_lsoa_2011_21$CHGIND == "S",]
  lookup_lsoa_2011_21_X = lookup_lsoa_2011_21[lookup_lsoa_2011_21$CHGIND == "X",]

  # Complex change replace with unchanged as changes are small
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

  lookup_lsoa_2011_21_X$CHGIND = "U"

  lookup_lsoa_2011_21_U = rbind(lookup_lsoa_2011_21_U, lookup_lsoa_2011_21_X)

  lookup_lsoa_2011_21_U$CHGIND = NULL
  lookup_lsoa_2011_21_M$CHGIND = NULL
  lookup_lsoa_2011_21_S$CHGIND = NULL

  # population_2021 = dplyr::left_join(lookup_lsoa_2011_21_S, population_2021, by = c("LSOA21CD" = "LSOA21"))
  # population_2021 = dplyr::group_by(population_2021, LSOA11CD)
  # population_2021 = dplyr::mutate(population_2021, pop_ratio = pop2021 / sum(pop2021))
  # population_2021 = dplyr::ungroup(population_2021, LSOA21CD)
  #
  # lookup_lsoa_2011_21_S = population_2021[,c("LSOA11CD","LSOA21CD","pop_ratio")]

  #Split ratio over time
  population = population[population$LSOA21CD %in% lookup_lsoa_2011_21_S$LSOA21CD,]
  population = population[,c("LSOA21CD","year","all_ages","adults","households_est")]
  population = dplyr::left_join(population, lookup_lsoa_2011_21_S, by = "LSOA21CD")
  population = dplyr::group_by(population, LSOA11CD, year)
  population = dplyr::mutate(population,
                      pop_ratio = all_ages / sum(all_ages),
                      household_ratio = households_est / sum(households_est),
                      )
  population = dplyr::ungroup(population)


  res = list(lookup_unchanged = lookup_lsoa_2011_21_U,
             lookup_split = population,
             lookup_merge = lookup_lsoa_2011_21_M)

  res

}





calculate_gas_emissions = function(domestic_gas, emissions_factors, population){

  domestic_gas = domestic_gas[,c("LSOA21CD","year","total_gas_kwh")]
  domestic_gas$total_gas_kwh[is.na(domestic_gas$total_gas_kwh)] = 0

  population = population[,c("LSOA21CD","year","all_ages")]

  domestic_gas = dplyr::left_join(domestic_gas, population, by = c("LSOA21CD","year"))
  domestic_gas = dplyr::left_join(domestic_gas, emissions_factors[,c("year","gas_kgco2e")], by = c("year"))

  domestic_gas$dom_gas_total_emissions = domestic_gas$total_gas_kwh * domestic_gas$gas_kgco2e
  domestic_gas$dom_gas_kgco2e_percap = ifelse(domestic_gas$all_ages == 0,0,
                                              domestic_gas$dom_gas_total_emissions / domestic_gas$all_ages)

  domestic_gas$year = as.integer(domestic_gas$year)

  domestic_gas = domestic_gas[,c("LSOA21CD","year","dom_gas_total_emissions","dom_gas_kgco2e_percap")]

  domestic_gas

}

calculate_electricity_emissions = function(domestic_electricity, emissions_factors, population){

  domestic_electricity = domestic_electricity[,c("LSOA21CD","year","total_elec_kwh")]
  domestic_electricity$total_elec_kwh[is.na(domestic_electricity$total_elec_kwh)] = 0

  population = population[,c("LSOA21CD","year","all_ages")]

  domestic_electricity = dplyr::left_join(domestic_electricity, population, by = c("LSOA21CD","year"))
  domestic_electricity = dplyr::left_join(domestic_electricity, emissions_factors[,c("year","electricity_kgco2e")], by = c("year"))

  domestic_electricity$dom_elec_total_emissions = domestic_electricity$total_elec_kwh * domestic_electricity$electricity_kgco2e
  domestic_electricity$dom_elec_kgco2e_percap = ifelse(domestic_electricity$all_ages == 0,0,
                                                      domestic_electricity$dom_elec_total_emissions / domestic_electricity$all_ages)

  domestic_electricity$year = as.integer(domestic_electricity$year)

  domestic_electricity = domestic_electricity[,c("LSOA21CD","year","dom_elec_total_emissions","dom_elec_kgco2e_percap")]

  domestic_electricity

}

load_postcode_gas_electricity = function(path = file.path(parameters$path_data,"gas_electric/postcode")){

  # Gas
  g15 <- read.csv(file.path(path,"Postcode_level_gas_2015.csv"))
  g16 <- read.csv(file.path(path,"Postcode_level_gas_2016.csv"))
  g17 <- read.csv(file.path(path,"Postcode_level_gas_2017.csv"))
  g18 <- read.csv(file.path(path,"Postcode_level_gas_2018.csv"))
  g19 <- read.csv(file.path(path,"Postcode_level_gas_2019.csv"))
  g20 <- read.csv(file.path(path,"Postcode_level_gas_2020.csv"))
  g21 <- read.csv(file.path(path,"Postcode_level_gas_2021.csv"))
  g22 <- read.csv(file.path(path,"Postcode_level_gas_2022.csv"))
  g23 <- read.csv(file.path(path,"Postcode_level_gas_2023.csv"))
  g24 <- read.csv(file.path(path,"Postcode_level_gas_2024.csv"))

  # Electric
  e15_all <- read.csv(file.path(path,"Postcode_level_all_meters_electricity_2015.csv"))
  e16_all <- read.csv(file.path(path,"Postcode_level_all_meters_electricity_2016.csv"))
  e17_all <- read.csv(file.path(path,"Postcode_level_all_meters_electricity_2017.csv"))
  e18_all <- read.csv(file.path(path,"Postcode_level_all_meters_electricity_2018.csv"))
  e19_all <- read.csv(file.path(path,"Postcode_level_all_meters_electricity_2019.csv"))
  e20_all <- read.csv(file.path(path,"Postcode_level_all_meters_electricity_2020.csv"))
  e21_all <- read.csv(file.path(path,"Postcode_level_all_meters_electricity_2021.csv"))
  e22_all <- read.csv(file.path(path,"Postcode_level_all_meters_electricity_2022.csv"))
  e23_all <- read.csv(file.path(path,"Postcode_level_all_meters_electricity_2023.csv"))
  e24_all <- read.csv(file.path(path,"Postcode_level_all_meters_electricity_2024.csv"))

  e15_st <- read.csv(file.path(path,"Postcode_level_standard_electricity_2015.csv"))
  e16_st <- read.csv(file.path(path,"Postcode_level_standard_electricity_2016.csv"))
  e17_st <- read.csv(file.path(path,"Postcode_level_standard_electricity_2017.csv"))
  e18_st <- read.csv(file.path(path,"Postcode_level_standard_electricity_2018.csv"))
  e19_st <- read.csv(file.path(path,"Postcode_level_standard_electricity_2019.csv"))
  e20_st <- read.csv(file.path(path,"Postcode_level_standard_electricity_2020.csv"))
  e21_st <- read.csv(file.path(path,"Postcode_level_standard_electricity_2021.csv"))
  e22_st <- read.csv(file.path(path,"Postcode_level_standard_electricity_2022.csv"))
  e23_st <- read.csv(file.path(path,"Postcode_level_standard_electricity_2023.csv"))
  e24_st <- read.csv(file.path(path,"Postcode_level_standard_electricity_2024.csv"))

  e15_eco7 <- read.csv(file.path(path,"Postcode_level_economy_7_electricity_2015.csv"))
  e16_eco7 <- read.csv(file.path(path,"Postcode_level_economy_7_electricity_2016.csv"))
  e17_eco7 <- read.csv(file.path(path,"Postcode_level_economy_7_electricity_2017.csv"))
  e18_eco7 <- read.csv(file.path(path,"Postcode_level_economy_7_electricity_2018.csv"))
  e19_eco7 <- read.csv(file.path(path,"Postcode_level_economy_7_electricity_2019.csv"))
  e20_eco7 <- read.csv(file.path(path,"Postcode_level_economy_7_electricity_2020.csv"))
  e21_eco7 <- read.csv(file.path(path,"Postcode_level_economy_7_electricity_2021.csv"))
  e22_eco7 <- read.csv(file.path(path,"Postcode_level_economy_7_electricity_2022.csv"))
  e23_eco7 <- read.csv(file.path(path,"Postcode_level_economy_7_electricity_2023.csv"))
  e24_eco7 <- read.csv(file.path(path,"Postcode_level_economy_7_electricity_2024.csv"))

  # Clean
  e15_all <- clean_postcode_elec(e15_all, 2015, "all")
  e16_all <- clean_postcode_elec(e16_all, 2016, "all")
  e17_all <- clean_postcode_elec(e17_all, 2017, "all")
  e18_all <- clean_postcode_elec(e18_all, 2018, "all")
  e19_all <- clean_postcode_elec(e19_all, 2019, "all")
  e20_all <- clean_postcode_elec(e20_all, 2020, "all")
  e21_all <- clean_postcode_elec(e21_all, 2021, "all")
  e22_all <- clean_postcode_elec(e22_all, 2022, "all")
  e23_all <- clean_postcode_elec(e23_all, 2023, "all")
  e24_all <- clean_postcode_elec(e24_all, 2024, "all")

  e15_st <- clean_postcode_elec(e15_st, 2015, "std")
  e16_st <- clean_postcode_elec(e16_st, 2016, "std")
  e17_st <- clean_postcode_elec(e17_st, 2017, "std")
  e18_st <- clean_postcode_elec(e18_st, 2018, "std")
  e19_st <- clean_postcode_elec(e19_st, 2019, "std")
  e20_st <- clean_postcode_elec(e20_st, 2020, "std")
  e21_st <- clean_postcode_elec(e21_st, 2021, "std")
  e22_st <- clean_postcode_elec(e22_st, 2022, "std")
  e23_st <- clean_postcode_elec(e23_st, 2023, "std")
  e24_st <- clean_postcode_elec(e24_st, 2024, "std")

  e15_eco7 <- clean_postcode_elec(e15_eco7, 2015, "eco7")
  e16_eco7 <- clean_postcode_elec(e16_eco7, 2016, "eco7")
  e17_eco7 <- clean_postcode_elec(e17_eco7, 2017, "eco7")
  e18_eco7 <- clean_postcode_elec(e18_eco7, 2018, "eco7")
  e19_eco7 <- clean_postcode_elec(e19_eco7, 2019, "eco7")
  e20_eco7 <- clean_postcode_elec(e20_eco7, 2020, "eco7")
  e21_eco7 <- clean_postcode_elec(e21_eco7, 2021, "eco7")
  e22_eco7 <- clean_postcode_elec(e22_eco7, 2022, "eco7")
  e23_eco7 <- clean_postcode_elec(e23_eco7, 2023, "eco7")
  e24_eco7 <- clean_postcode_elec(e24_eco7, 2024, "eco7")


  g15 <- clean_postcode_gas(g15, 2015)
  g16 <- clean_postcode_gas(g16, 2016)
  g17 <- clean_postcode_gas(g17, 2017)
  g18 <- clean_postcode_gas(g18, 2018)
  g19 <- clean_postcode_gas(g19, 2019)
  g20 <- clean_postcode_gas(g20, 2020)
  g21 <- clean_postcode_gas(g21, 2021)
  g22 <- clean_postcode_gas(g22, 2022)
  g23 <- clean_postcode_gas(g23, 2023)
  g24 <- clean_postcode_gas(g24, 2024)

  gall <- dplyr::full_join(g22, g21, by = "postcode")
  gall <- dplyr::full_join(gall, g20, by = "postcode")
  gall <- dplyr::full_join(gall, g19, by = "postcode")
  gall <- dplyr::full_join(gall, g18, by = "postcode")
  gall <- dplyr::full_join(gall, g17, by = "postcode")
  gall <- dplyr::full_join(gall, g16, by = "postcode")
  gall <- dplyr::full_join(gall, g15, by = "postcode")
  gall <- dplyr::full_join(gall, g23, by = "postcode")
  gall <- dplyr::full_join(gall, g24, by = "postcode")

  eall_all <- dplyr::full_join(e22_all, e21_all, by = "postcode")
  eall_all <- dplyr::full_join(eall_all, e20_all, by = "postcode")
  eall_all <- dplyr::full_join(eall_all, e19_all, by = "postcode")
  eall_all <- dplyr::full_join(eall_all, e18_all, by = "postcode")
  eall_all <- dplyr::full_join(eall_all, e17_all, by = "postcode")
  eall_all <- dplyr::full_join(eall_all, e16_all, by = "postcode")
  eall_all <- dplyr::full_join(eall_all, e15_all, by = "postcode")
  eall_all <- dplyr::full_join(eall_all, e23_all, by = "postcode")
  eall_all <- dplyr::full_join(eall_all, e24_all, by = "postcode")

  eall_st <- dplyr::full_join(e22_st, e21_st, by = "postcode")
  eall_st <- dplyr::full_join(eall_st, e20_st, by = "postcode")
  eall_st <- dplyr::full_join(eall_st, e19_st, by = "postcode")
  eall_st <- dplyr::full_join(eall_st, e18_st, by = "postcode")
  eall_st <- dplyr::full_join(eall_st, e17_st, by = "postcode")
  eall_st <- dplyr::full_join(eall_st, e16_st, by = "postcode")
  eall_st <- dplyr::full_join(eall_st, e15_st, by = "postcode")
  eall_st <- dplyr::full_join(eall_st, e23_st, by = "postcode")
  eall_st <- dplyr::full_join(eall_st, e24_st, by = "postcode")

  eall_eco7 <- dplyr::full_join(e22_eco7, e21_eco7, by = "postcode")
  eall_eco7 <- dplyr::full_join(eall_eco7, e20_eco7, by = "postcode")
  eall_eco7 <- dplyr::full_join(eall_eco7, e19_eco7, by = "postcode")
  eall_eco7 <- dplyr::full_join(eall_eco7, e18_eco7, by = "postcode")
  eall_eco7 <- dplyr::full_join(eall_eco7, e17_eco7, by = "postcode")
  eall_eco7 <- dplyr::full_join(eall_eco7, e16_eco7, by = "postcode")
  eall_eco7 <- dplyr::full_join(eall_eco7, e15_eco7, by = "postcode")
  eall_eco7 <- dplyr::full_join(eall_eco7, e23_eco7, by = "postcode")
  eall_eco7 <- dplyr::full_join(eall_eco7, e24_eco7, by = "postcode")

  geall = dplyr::full_join(gall, eall_all, by = "postcode")
  geall = dplyr::full_join(geall, eall_st, by = "postcode")
  geall = dplyr::full_join(geall, eall_eco7, by = "postcode")

  yrs = 2015:2024

  geall = geall[,c("postcode",
                   paste0("gas_meters_",yrs),
                   paste0("elec_meters_all_",yrs),
                   paste0("elec_meters_std_",yrs),
                   paste0("elec_meters_eco7_",yrs),
                   paste0("gas_totalkwh_",yrs),
                   paste0("elec_totalkwh_all_",yrs),
                   paste0("elec_totalkwh_std_",yrs),
                   paste0("elec_totalkwh_eco7_",yrs),
                   paste0("gas_meankwh_",yrs),
                   paste0("elec_meankwh_all_",yrs),
                   paste0("elec_meankwh_std_",yrs),
                   paste0("elec_meankwh_eco7_",yrs),
                   paste0("gas_mediankwh_",yrs),
                   paste0("elec_mediankwh_std_",yrs),
                   paste0("elec_mediankwh_all_",yrs),
                   paste0("elec_mediankwh_eco7_",yrs)
                   )]

  geall
}

clean_postcode_elec = function(sub, year, type){
  sub = sub[,c("Postcode","Num_meters","Total_cons_kwh","Mean_cons_kwh","Median_cons_kwh")]
  names(sub) = c("postcode",paste0("elec_meters_",type,"_",year),paste0("elec_totalkwh_",type,"_",year),
                 paste0("elec_meankwh_",type,"_",year),paste0("elec_mediankwh_",type,"_",year))
  sub = sub[sub$postcode != "All postcodes",]
  sub
}

clean_postcode_gas = function(sub, year){
  sub = sub[,c("Postcode","Num_meters","Total_cons_kwh","Mean_cons_kwh","Median_cons_kwh")]
  names(sub) = c("postcode",paste0("gas_meters_",year),paste0("gas_totalkwh_",year),
                 paste0("gas_meankwh_",year),paste0("gas_mediankwh_",year))
  sub = sub[sub$postcode != "All postcodes",]
  sub
}


calculate_postcode_gas_electric_emissions = function(postcode_gas_electricity, emissions_factors){

  names(postcode_gas_electricity) = gsub("_20", "X20", names(postcode_gas_electricity) )
  nms = names(postcode_gas_electricity)
  nms = nms[!nms %in% "postcode"]

  emissions_factors = emissions_factors[,c("year","electricity_kgco2e","gas_kgco2e")]



  bar = tidyr::pivot_longer(postcode_gas_electricity,
                            cols = dplyr::all_of(nms),
                            names_pattern = "(.*)X(.*)",
                            names_to = c("variaible","year"))

  bar = tidyr::pivot_wider(bar, id_cols = c("postcode","year"),
                           names_from = "variaible",
                           values_from = "value")
  bar$year = as.integer(bar$year)

  bar = dplyr::left_join(bar, emissions_factors, by = "year")

  bar$gas_totalkgco2e = bar$gas_totalkwh  * bar$gas_kgco2e
  bar$gas_mediankgco2e = bar$gas_mediankwh * bar$gas_kgco2e
  bar$gas_meankgco2e = bar$gas_meankwh   * bar$gas_kgco2e

  bar$elec_totalkgco2e_all = bar$elec_totalkwh_all * bar$electricity_kgco2e
  bar$elec_meankgco2e_all = bar$elec_meankwh_all * bar$electricity_kgco2e
  bar$elec_mediankgco2e_all = bar$elec_mediankwh_all * bar$electricity_kgco2e

  bar$elec_totalkgco2e_std = bar$elec_totalkwh_std * bar$electricity_kgco2e
  bar$elec_meankgco2e_std = bar$elec_meankwh_std * bar$electricity_kgco2e
  bar$elec_mediankgco2e_std = bar$elec_mediankwh_std * bar$electricity_kgco2e

  bar$elec_totalkgco2e_eco7 = bar$elec_totalkwh_eco7 * bar$electricity_kgco2e
  bar$elec_meankgco2e_eco7 = bar$elec_meankwh_eco7 * bar$electricity_kgco2e
  bar$elec_mediankgco2e_eco7 = bar$elec_mediankwh_eco7 * bar$electricity_kgco2e

  bar$gas_kgco2e = NULL
  bar$electricity_kgco2e = NULL

  # Fill in missing data
  bar$elec_meters_eco7 = dplyr::if_else(is.na(bar$elec_meters_eco7),
                                        bar$elec_meters_all - bar$elec_meters_std,
                                        bar$elec_meters_eco7)
  bar$elec_meters_std = dplyr::if_else(is.na(bar$elec_meters_std),
                                        bar$elec_meters_all - bar$elec_meters_eco7,
                                        bar$elec_meters_std)

  bar

}

sumNA = function(x, y){
  x[is.na(x)] = 0
  y[is.na(y)] = 0
  x+y
}


prep_postcode_gas_electic = function(postcode_gas_electricity_emissions, bounds_postcodes_2024){
  sub = postcode_gas_electricity_emissions[postcode_gas_electricity_emissions$year == max(postcode_gas_electricity_emissions$year, na.rm = TRUE), ]
  sub = sub[sub$postcode %in% bounds_postcodes_2024$POSTCODE,]
  sub = sub[,c("postcode","elec_mediankgco2e_all","gas_mediankgco2e")]
  sub$gas_elec_mediankgco2e = sumNA(sub$gas_mediankgco2e, sub$elec_mediankgco2e_all)
  sub$elec = value2grade(sub$elec_mediankgco2e_all, FALSE)
  sub$gas = value2grade(sub$gas_mediankgco2e, FALSE)
  sub$combined = value2grade(sub$gas_elec_mediankgco2e, FALSE)
  sub = sub[,c("postcode","gas","elec","combined")]
  bounds_postcodes_2024$PC_AREA = NULL
  sub = dplyr::left_join(sub, bounds_postcodes_2024, by = c("postcode" = "POSTCODE"))
  sub = sf::st_as_sf(sub)
  sub
}


calculate_lsoa_gas_electric_emissions = function(domestic_gas, domestic_electricity, emissions_factors,
                                                 bills_gas_electric, bills_other_heating, other_heating_emissions){
  sub = dplyr::full_join(domestic_gas, domestic_electricity, by = c("LSOA21CD","year"))
  names(sub)[names(sub) == "meters.x"] = "meters_gas"
  names(sub)[names(sub) == "meters.y"] = "meters_elec"

  emissions_factors = emissions_factors[,c("year","electricity_kgco2e","gas_kgco2e")]

  bar = dplyr::left_join(sub, emissions_factors, by = "year")

  bar$total_gas_kgco2e = bar$total_gas_kwh  * bar$gas_kgco2e
  bar$median_gas_kgco2e = bar$median_gas_kwh * bar$gas_kgco2e
  bar$mean_gas_kgco2e = bar$mean_gas_kwh   * bar$gas_kgco2e

  bar$total_elec_kgco2e = bar$total_elec_kwh * bar$electricity_kgco2e
  bar$mean_elec_kgco2e = bar$mean_elec_kwh * bar$electricity_kgco2e
  bar$median_elec_kgco2e = bar$median_elec_kwh * bar$electricity_kgco2e

  bar$gas_kgco2e = NULL
  bar$electricity_kgco2e = NULL

  bar$mean_elec_kgco2e[is.nan(bar$mean_elec_kgco2e)] = 0
  bar$mean_elec_kwh[is.nan(bar$mean_elec_kwh)] = 0

  bills_gas_electric = bills_gas_electric[,c("LSOA21CD","year","gas_average_bill","elec_average_bill","energy_average_bill")]
  bills_other_heating = bills_other_heating[,c("LSOA21CD","year","otherheating_average_bill")]
  other_heating_emissions = other_heating_emissions[,c("LSOA21CD","year","heating_other_emissions_total","all_properties")]

  other_heating_emissions$mean_other_kgco2e = round(other_heating_emissions$heating_other_emissions_total / other_heating_emissions$all_properties)

  names(other_heating_emissions)[names(other_heating_emissions) == "heating_other_emissions_total"] = "total_other_kgco2e"

  other_heating_emissions = other_heating_emissions[,c("LSOA21CD","year","mean_other_kgco2e")]

  bar2 = dplyr::full_join(bills_gas_electric, bills_other_heating, by = c("LSOA21CD","year"))
  bar2 = dplyr::full_join(bar2, other_heating_emissions, by = c("LSOA21CD","year"))

  bar3 = dplyr::full_join(bar, bar2, by = c("LSOA21CD","year"))

  # TODO: Missing Scotland data for 2023 and 2024
  bar3$energy_average_bill = ifelse(!is.na(bar3$energy_average_bill + bar3$otherheating_average_bill),
                                    bar3$energy_average_bill + bar3$otherheating_average_bill,
                                    bar3$energy_average_bill
                                    )

  bar3

}
