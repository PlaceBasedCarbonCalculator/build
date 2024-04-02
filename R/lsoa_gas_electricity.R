dowload_gas_electric <- function(path = file.path(data_path(),"gas_electric")){
  if(!dir.exists(path)){
    dir.create(path)
  } else {
    fls = list.files(path, pattern = "xlsx")
    if(length(fls) > 3){
      return(path)
    }
  }

  url_elec = "https://assets.publishing.service.gov.uk/media/63a2ea3fd3bf7f375b61c12d/LSOA_domestic_elec_2010-21.xlsx"
  download.file(url_elec, file.path(path,"lsoa_elec_dom.xlsx"), mode = "wb")

  url_elec = "https://assets.publishing.service.gov.uk/media/63a2e9e58fa8f5390dfdf56b/MSOA_non-domestic_elec_2010-21.xlsx"
  download.file(url_elec, file.path(path,"msoa_elec_nondom.xlsx"), mode = "wb")

  url_gas = "https://assets.publishing.service.gov.uk/media/63a2ef098fa8f53913e8071b/LSOA_domestic_gas_2010-21.xlsx"
  download.file(url_gas, file.path(path,"lsoa_gas_dom.xlsx"), mode = "wb")

  url_gas = "https://assets.publishing.service.gov.uk/media/63a31f44e90e075870e2cf0f/MSOA_non-domestic_gas_2010-21.xlsx"
  download.file(url_gas, file.path(path,"msoa_gas_nondom.xlsx"), mode = "wb")

  return(path)

}



load_lsoa_electric <- function(path){

  elec = list()
  for(i in 2010:2021){
    sub <- readxl::read_excel(file.path(path,"lsoa_elec_dom.xlsx"),
                              sheet = as.character(i))
    sub <- as.data.frame(sub)
    sub <- sub[5:nrow(sub),]
    names(sub) <- c("LAcode","LAname",
                    "MSOA","MSOAname",
                    "LSOA","LSOAname",
                    "metres","total_elec_kwh",
                    "mean_elec_kwh","median_elec_kwh")

    sub$year <- i
    elec[[i]] <- sub
  }
  elec = dplyr::bind_rows(elec)
  unlink(file.path(tempdir(),"gaselec"), recursive = TRUE)

  elec$metres = as.numeric(elec$metres)
  elec$total_elec_kwh = as.numeric(elec$total_elec_kwh)
  elec$mean_elec_kwh = as.numeric(elec$mean_elec_kwh)
  elec$median_elec_kwh = as.numeric(elec$median_elec_kwh)

  elec


}



load_msoa_electric_nondom <- function(path){



  elec = list()
  for(i in 2010:2021){
    sub <- readxl::read_excel(file.path(path,"msoa_elec_nondom.xlsx"),
                              sheet = as.character(i))
    sub <- as.data.frame(sub)
    sub <- sub[5:nrow(sub),]
    names(sub) <- c("LAcode","LAname",
                    "MSOA","MSOAname",
                    "metre_type",
                    "metres","total_elec_kwh",
                    "mean_elec_kwh","median_elec_kwh")

    sub$year <- i
    elec[[i]] <- sub
  }
  elec = dplyr::bind_rows(elec)
  unlink(file.path(tempdir(),"gaselec"), recursive = TRUE)

  elec$metres = as.numeric(elec$metres)
  elec$total_elec_kwh = as.numeric(elec$total_elec_kwh)
  elec$mean_elec_kwh = as.numeric(elec$mean_elec_kwh)
  elec$median_elec_kwh = as.numeric(elec$median_elec_kwh)

  elec

}



load_lsoa_gas <- function(path){



  gas = list()
  for(i in 2010:2021){
    sub <- readxl::read_excel(file.path(path,"lsoa_gas_dom.xlsx"),
                              sheet = as.character(i))
    sub <- as.data.frame(sub)
    sub <- sub[5:nrow(sub),]
    names(sub) <- c("LAcode","LAname",
                    "MSOA","MSOAname",
                    "LSOA","LSOAname",
                    "metres","total_gas_kwh",
                    "mean_gas_kwh","median_gas_kwh")

    sub$year <- i
    gas[[i]] <- sub
  }
  gas = dplyr::bind_rows(gas)
  unlink(file.path(tempdir(),"gaselec"), recursive = TRUE)

  gas$metres = as.numeric(gas$metres)
  gas$total_gas_kwh = as.numeric(gas$total_gas_kwh)
  gas$mean_gas_kwh = as.numeric(gas$mean_gas_kwh)
  gas$median_gas_kwh = as.numeric(gas$median_gas_kwh)

  gas


}



load_msoa_gas_nondom <- function(path){

  gas = list()
  for(i in 2010:2021){
    sub <- readxl::read_excel(file.path(path,"msoa_elec_nondom.xlsx"),
                              sheet = as.character(i))
    sub <- as.data.frame(sub)
    sub <- sub[5:nrow(sub),]
    names(sub) <- c("LAcode","LAname",
                    "MSOA","MSOAname",
                    "metre_type",
                    "metres","total_gas_kwh",
                    "mean_gas_kwh","median_gas_kwh")

    sub$year <- i
    gas[[i]] <- sub
  }
  gas = dplyr::bind_rows(gas)
  unlink(file.path(tempdir(),"gaselec"), recursive = TRUE)

  gas$metres = as.numeric(gas$metres)
  gas$total_gas_kwh = as.numeric(gas$total_gas_kwh)
  gas$mean_gas_kwh = as.numeric(gas$mean_gas_kwh)
  gas$median_gas_kwh = as.numeric(gas$median_gas_kwh)

  gas

}

lsoa_gas_to_2021 <- function(domestic_gas, lsoa_11_21_tools){

  domestic_gas = domestic_gas[,c("LSOA","year","metres","total_gas_kwh","mean_gas_kwh","median_gas_kwh")]
  names(domestic_gas)[1] = "LSOA11CD"


  domestic_gas_S = domestic_gas[domestic_gas$LSOA11CD %in% lsoa_11_21_tools$lookup_split$LSOA11CD,]
  domestic_gas_M = domestic_gas[domestic_gas$LSOA11CD %in% lsoa_11_21_tools$lookup_merge$LSOA11CD,]
  domestic_gas_U = domestic_gas[domestic_gas$LSOA11CD %in% lsoa_11_21_tools$lookup_unchanged$LSOA11CD,]

  #Unchanged
  domestic_gas_U = dplyr::left_join(domestic_gas_U, lsoa_11_21_tools$lookup_unchanged, by = "LSOA11CD")

  # Merge
  domestic_gas_M = dplyr::left_join(domestic_gas_M, lsoa_11_21_tools$lookup_merge, by = "LSOA11CD")
  domestic_gas_M = dplyr::group_by(domestic_gas_M, year, LSOA21CD)
  domestic_gas_M = dplyr::summarise(domestic_gas_M,
                                    total_gas_kwh = sum(total_gas_kwh, na.rm = TRUE),
                                    mean_gas_kwh = weighted.mean(mean_gas_kwh, metres, na.rm = TRUE),
                                    median_gas_kwh = weighted.mean(median_gas_kwh, metres, na.rm = TRUE),
                                    metres = sum(metres, na.rm = TRUE))
  domestic_gas_M = dplyr::ungroup(domestic_gas_M)

  #Split
  domestic_gas_S = dplyr::left_join(lsoa_11_21_tools$lookup_split, domestic_gas_S,
                                            by = "LSOA11CD", relationship = "many-to-many")
  domestic_gas_S = as.data.frame(domestic_gas_S)
  for(i in 5:6){
    domestic_gas_S[i] = domestic_gas_S[,i ,drop = TRUE] * domestic_gas_S$pop_ratio
  }

  nms = c("LSOA21CD","year","metres","total_gas_kwh","mean_gas_kwh","median_gas_kwh")

  domestic_gas_S = domestic_gas_S[,nms]
  domestic_gas_M = domestic_gas_M[,nms]
  domestic_gas_U = domestic_gas_U[,nms]

  final = rbind(domestic_gas_S, domestic_gas_M, domestic_gas_U)
  final

}

lsoa_electric_to_2021 <- function(domestic_electricity, lsoa_11_21_tools){

  domestic_electricity = domestic_electricity[,c("LSOA","year","metres","total_elec_kwh","mean_elec_kwh","median_elec_kwh")]
  names(domestic_electricity)[1] = "LSOA11CD"


  domestic_electricity_S = domestic_electricity[domestic_electricity$LSOA11CD %in% lsoa_11_21_tools$lookup_split$LSOA11CD,]
  domestic_electricity_M = domestic_electricity[domestic_electricity$LSOA11CD %in% lsoa_11_21_tools$lookup_merge$LSOA11CD,]
  domestic_electricity_U = domestic_electricity[domestic_electricity$LSOA11CD %in% lsoa_11_21_tools$lookup_unchanged$LSOA11CD,]

  #Unchanged
  domestic_electricity_U = dplyr::left_join(domestic_electricity_U, lsoa_11_21_tools$lookup_unchanged, by = "LSOA11CD")

  # Merge
  domestic_electricity_M = dplyr::left_join(domestic_electricity_M, lsoa_11_21_tools$lookup_merge, by = "LSOA11CD")
  domestic_electricity_M = dplyr::group_by(domestic_electricity_M, year, LSOA21CD)
  domestic_electricity_M = dplyr::summarise(domestic_electricity_M,
                                            total_elec_kwh = sum(total_elec_kwh, na.rm = TRUE),
                                            mean_elec_kwh = weighted.mean(mean_elec_kwh, metres, na.rm = TRUE),
                                            median_elec_kwh = weighted.mean(median_elec_kwh, metres, na.rm = TRUE),
                                            metres = sum(metres, na.rm = TRUE))
  domestic_electricity_M = dplyr::ungroup(domestic_electricity_M)

  #Split
  domestic_electricity_S = dplyr::left_join(lsoa_11_21_tools$lookup_split, domestic_electricity_S,
                                            by = "LSOA11CD", relationship = "many-to-many")
  domestic_electricity_S = as.data.frame(domestic_electricity_S)
  for(i in 5:6){
    domestic_electricity_S[i] = domestic_electricity_S[,i ,drop = TRUE] * domestic_electricity_S$pop_ratio
  }

  nms = c("LSOA21CD","year","metres","total_elec_kwh","mean_elec_kwh","median_elec_kwh")

  domestic_electricity_S = domestic_electricity_S[,nms]
  domestic_electricity_M = domestic_electricity_M[,nms]
  domestic_electricity_U = domestic_electricity_U[,nms]

  final = rbind(domestic_electricity_S, domestic_electricity_M, domestic_electricity_U)
  final

}



lsoa_convert_2011_2021_pre_data = function(lookup_lsoa_2011_21, population_2021) {

  lookup_lsoa_2011_21 = lookup_lsoa_2011_21[,c("LSOA11CD","LSOA21CD","CHGIND")]
  population_2021 = population_2021[,c("LSOA21","all_ages")]
  names(population_2021) = c("LSOA21","pop2021")

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

  population_2021 = dplyr::left_join(lookup_lsoa_2011_21_S, population_2021, by = c("LSOA21CD" = "LSOA21"))
  population_2021 = dplyr::group_by(population_2021, LSOA11CD)
  population_2021 = dplyr::mutate(population_2021, pop_ratio = pop2021 / sum(pop2021))
  population_2021 = dplyr::ungroup(population_2021, LSOA21CD)

  lookup_lsoa_2011_21_S = population_2021[,c("LSOA11CD","LSOA21CD","pop_ratio")]

  res = list(lookup_unchanged = lookup_lsoa_2011_21_U,
             lookup_split = lookup_lsoa_2011_21_S,
             lookup_merge = lookup_lsoa_2011_21_M)

  res

}

calculate_gas_emissions = function(domestic_gas, emissions_factors, population){

  domestic_gas = domestic_gas[,c("LSOA21CD","year","total_gas_kwh")]
  domestic_gas$total_gas_kwh[is.na(domestic_gas$total_gas_kwh)] = 0
  domestic_gas = tidyr::pivot_wider(domestic_gas, names_from = "year", values_from = "total_gas_kwh")
  domestic_gas = as.data.frame(domestic_gas)

  population = population[,c("LSOA21CD","year","all_ages")]
  population = tidyr::pivot_wider(population, names_from = "year", values_from = "all_ages")

  domestic_gas = domestic_gas[order(domestic_gas$LSOA21CD),]
  population = population[population$LSOA21CD %in% domestic_gas$LSOA21CD,]
  population = population[order(population$LSOA21CD),]
  population = as.data.frame(population)

  if(!all(population$LSOA21CD == domestic_gas$LSOA21CD)){
    stop("LSOA21CD don't match")
  }

  for(i in 2010:2021){
    domestic_gas[paste0("dom_gas_kgco2e_percap_",i)] = (domestic_gas[as.character(i)] / population[as.character(i)]) * emissions_factors$gas_kgco2e[emissions_factors$year == i]
  }

  domestic_gas = domestic_gas[,c("LSOA21CD",paste0("dom_gas_kgco2e_percap_",2010:2021))]
  domestic_gas

}

calculate_electricity_emissions = function(domestic_electricity, emissions_factors, population){

  domestic_electricity = domestic_electricity[,c("LSOA21CD","year","total_elec_kwh")]
  domestic_electricity$total_elec_kwh[is.na(domestic_electricity$total_elec_kwh)] = 0
  domestic_electricity = tidyr::pivot_wider(domestic_electricity, names_from = "year", values_from = "total_elec_kwh")
  domestic_electricity = as.data.frame(domestic_electricity)

  population = population[,c("LSOA21CD","year","all_ages")]
  population = tidyr::pivot_wider(population, names_from = "year", values_from = "all_ages")

  domestic_electricity = domestic_electricity[order(domestic_electricity$LSOA21CD),]
  population = population[population$LSOA21CD %in% domestic_electricity$LSOA21CD,]
  population = population[order(population$LSOA21CD),]
  population = as.data.frame(population)

  if(!all(population$LSOA21CD == domestic_electricity$LSOA21CD)){
    stop("LSOA21CD don't match")
  }

  for(i in 2010:2021){
    domestic_electricity[paste0("dom_elec_kgco2e_percap_",i)] = (domestic_electricity[as.character(i)] / population[as.character(i)]) * emissions_factors$electricity_kgco2e[emissions_factors$year == i]
  }

  domestic_electricity = domestic_electricity[,c("LSOA21CD",paste0("dom_elec_kgco2e_percap_",2010:2021))]
  domestic_electricity

}



