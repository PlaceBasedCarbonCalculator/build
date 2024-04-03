download_consumption_footprint <- function(path){
  if(!dir.exists(path)){
    dir.create(path)
  } else {
    fls = list.files(path, pattern = "ods")
    if(length(fls) > 0){
      return(path)
    }
  }

  url = "https://assets.publishing.service.gov.uk/media/64ca46236ae44e001311b3df/2020_Defra_results_UK_rev.ods"
  download.file(url, file.path(path, "2020_Defra_results_UK_rev.ods"), mode = "wb")

  if(!file.exists(file.path(path, "2020_Defra_results_UK_rev.ods"))){
    stop("Download failed")
  }


  return(file.path(path, "2020_Defra_results_UK_rev.ods"))

}

load_consumption_footprint <- function(path){
  cons = readODS::read_ods(path, sheet = "Summary_product_90-20")
  cons = cons[,2:37]
  cons = as.data.frame(cons)
  names(cons) = cons[2,]
  cons = cons[3:33,]
  names(cons)[1] = "year"
  cons[] <- lapply(cons[], as.numeric)

  lookup = data.frame(
    id = c("FOO","DRI","ALC","TOB","CLO","SHO","ACR","IMR","DIY","WAT","ELG",
           "FRN","TEX","APL","GLA","TOO","GOO","MED","OPS","HOS","VEH","OPP",
           "TRS","PST","TEL","TES","AVI","MRC","ORC","RCS","NBS","EDU","R&H","MSC"),
    desc = c(
    "Food",
    "Non-alcoholic beverages",
    "Alcoholic beverages",
    "Tobacco",
    "Clothing",
    "Footwear",
    "Actual rentals for households",
    "Imputed rentals for households",
    "Maintenance and repair of the dwelling",
    "Water supply and miscellaneous dwelling services",
    "Electricity, gas and other fuels",
    "Furniture, furnishings, carpets etc",
    "Household textiles",
    "Household appliances",
    "Glassware, tableware and household utensils",
    "Tools and equipment for house and garden",
    "Goods and services for household maintenance",
    "Medical products, appliances and equipment",
    "Out-patient services",
    "Hospital services",
    "Purchase of vehicles",
    "Operation of personal transport equipment",
    "Transport services",
    "Postal services",
    "Telephone and telefax equipment",
    "Telephone and telefax services",
    "Audio-visual, photo and info processing equipment",
    "Other major durables for recreation and culture",
    "Other recreational equipment etc",
    "Recreational and cultural services",
    "Newspapers, books and stationery",
    "Education",
    "Restaurants and hotels",
    "Miscellaneous goods and services"
  ),
  group = c(
    "nutrition",
    "nutrition",
    "nutrition",
    "consumables",
    "consumables",
    "consumables",
    "other_shelter",
    "other_shelter",
    "other_shelter",
    "other_shelter",
    "gas_electric",
    "consumables",
    "consumables",
    "consumables",
    "consumables",
    "consumables",
    "other_shelter",
    "consumables",
    "services",
    "services",
    "mobility_purchase",
    "mobility_use",
    "mobility_use",
    "services",
    "services",
    "services",
    "services",
    "recreation",
    "recreation",
    "recreation",
    "recreation",
    "services",
    "recreation",
    "services"
  )
  )

  cons = tidyr::pivot_longer(cons, cols = names(cons)[2:36])
  cons = dplyr::left_join(cons, lookup, by = c("name" = "id"))
  cons = cons[cons$name != "Total",]
  cons


}


load_consumption_income = function(path){
  income = read.csv(file.path(path,"income_energy_fooprint.csv"))
  income
}

calculate_consumption_lsoa = function(consumption_uk, consumption_income, population, income_lsoa){

  check_value = sum(consumption_uk$value[consumption_uk$year == 2018 & consumption_uk$group == "nutrition"]) * 1e6

  # Group shares
  consumption_uk = dplyr::group_by(consumption_uk, year, group)
  consumption_uk = dplyr::summarise(consumption_uk,value = sum(value))
  consumption_uk = dplyr::ungroup(consumption_uk)

  #TODO: weight as GB is not whole UK

  consumption_uk$value = consumption_uk$value * 1e6 # Convert from ktonnes to kg
  consumption_uk = tidyr::pivot_wider(consumption_uk, names_from = "group", values_from = "value")

  if(check_value != consumption_uk$nutrition[consumption_uk$year == 2018]){
    stop("Check failed")
  }

  # Convert Income Based Consumption to shares
  consumption_income = consumption_income[,c("income_group","gas_electric_toe_household",
                                             "other_shelter_toes_household","nutrition_toe_household",
                                             "flights_toes_household","mobility_toes_household",
                                             "consumables_toe_household","recreation_toes_household",
                                             "services_toes_household","total_toes_household")]
  consumption_income[2:ncol(consumption_income)] = lapply(consumption_income[2:ncol(consumption_income)],
                                                          function(x){x/sum(x)})
  names(consumption_income) = gsub("_toes_household","",names(consumption_income))
  names(consumption_income) = gsub("_toe_household","",names(consumption_income))
  consumption_income$total = NULL

  if(!all(sapply(consumption_income[2:ncol(consumption_income)], function(x){round(sum(x),5)}) == 1)){
    stop("Weights don't add to 1")
  }

  income_lsoa$income_band <- income_bands(income_lsoa$income_lsoa)

  #Prep population
  population <- population[,c("year", "LSOA21CD","all_ages")]

  consumption_band = list()
  for(i in 1:20){
    sub = consumption_uk
    sub$income_group = i - 1
    consumption_band[[i]] = sub
  }
  consumption_band = dplyr::bind_rows(consumption_band)
  consumption_band = consumption_band[order(consumption_band$year),]

  names(consumption_income) = paste0(names(consumption_income),"_wt")

  consumption_band = dplyr::left_join(consumption_band, consumption_income, by = c("income_group" = "income_group_wt"))
  consumption_band = as.data.frame(consumption_band)

  for(i in c("consumables","gas_electric","nutrition","other_shelter","recreation","services")){
    consumption_band[paste0(i,"_emissions")] = consumption_band[i] * consumption_band[paste0(i,"_wt")]
  }
  consumption_band = consumption_band[,c("year","income_group","consumables_emissions",
                                         "gas_electric_emissions","nutrition_emissions",
                                         "other_shelter_emissions","recreation_emissions",
                                         "services_emissions")]

  if(round(sum(consumption_band$nutrition_emissions[consumption_band$year == 2018])) != round(check_value)){
    stop("consumption does not match")
  }


  #TODO: change income over time
  lsoa <- dplyr::left_join(population, income_lsoa, by = "LSOA21CD")
  #lsoa = lsoa[lsoa$year == 2018,]
  lsoa = lsoa[!is.na(lsoa$income_band),]

  lsoa = dplyr::left_join(lsoa, consumption_band, by = c("year" = "year","income_band" = "income_group"))

  if(round(sum(consumption_band$nutrition_emissions[consumption_band$year == 2018])) != round(check_value)){
    stop("consumption does not match")
  }

  #sum(lsoa$nutrition_emissions) / (nrow(lsoa)/20)


  #foo = consumption_band[consumption_band$year == 2002, ]
  #sum(foo$nutrition_emissions)

  #sum(lsoa$nutrition_emissions, na.rm = TRUE) / sum(consumption_band$nutrition_emissions[consumption_band$year == 2002])

  # Convert to per person
  # for(i in c("consumables","gas_electric","nutrition","other_shelter","recreation","services")){
  #   lsoa[paste0(i,"_emissions_pz")] = lsoa[paste0(i,"_emissions")] / ((nrow(lsoa)/20))
  # }

  for(i in c("consumables","gas_electric","nutrition","other_shelter","recreation","services")){
    lsoa[paste0(i,"_emissions_percap")] = lsoa[paste0(i,"_emissions")] / ((nrow(lsoa)/20)) / lsoa$all_ages
  }

  if(0.00001 < (round(sum(lsoa$nutrition_emissions_percap[lsoa$year == 2018] * lsoa$all_ages[lsoa$year == 2018])) - round(check_value))/round(check_value) ){
    stop("Per person emissions dont mathc total")
  }

  lsoa = lsoa[,c("year","LSOA21CD",names(lsoa)[grepl("_percap",names(lsoa))])]
  lsoa

}

# Convert incomes into ventiles
income_bands <- function(dat){

  pt1 <- quantile(dat, probs = seq(0, 1, by = 0.05), type = 7, na.rm = TRUE)
  pt2 <- unique(as.data.frame(pt1), fromLast = TRUE)
  pt3 <- rownames(pt2)
  pt4 <- as.integer(strsplit(pt3, "%"))

  if(0 %in% pt2$pt1){
    cts <- c(-0.000001, pt2$pt1)
  } else {
    cts <- c(0, pt2$pt1)
  }
  datp <- pt4[as.integer(cut(dat, cts, labels = 1:length(pt3)))]
  datp <- datp/5 - 1
  datp[datp < 0]= 0

  datp

}



