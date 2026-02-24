download_consumption_footprint <- function(path){
  if(!dir.exists(path)){
    dir.create(path)
  } else {
    fls = list.files(path, pattern = "ods")
    if(length(fls) > 0){
      return(file.path(path, "2020_Defra_results_UK_rev.ods"))
    }
  }

  url = "https://assets.publishing.service.gov.uk/media/64ca46236ae44e001311b3df/2020_Defra_results_UK_rev.ods"
  download.file(url, file.path(path, "2020_Defra_results_UK_rev.ods"), mode = "wb")

  if(!file.exists(file.path(path, "2020_Defra_results_UK_rev.ods"))){
    stop("Download failed")
  }


  return(file.path(path, "2020_Defra_results_UK_rev.ods"))

}

load_consumption_footprint <- function(path, sheet = "Summary_product_90-22"){
  cons = readODS::read_ods(path, sheet = sheet)
  cons = cons[,2:37]
  cons = as.data.frame(cons)
  names(cons) = cons[2,]
  names(cons)[1] = "year"
  cons = cons[3:nrow(cons),]
  first_na <- which(is.na(cons$year))[1] # Two sets of data on the page, only take first
  cons = cons[1:(first_na - 1),]

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


load_la_consumption_accounts = function(path = "../inputdata/consumption/laca_data-c524b8f66272a734d87238602a7cef41.xlsx"){

  yrs = 2001:2022

  res = list()
  for(i in yrs){
    sub = readxl::read_xlsx(path, sheet = as.character(i), col_names = FALSE)
    names(sub) = as.character(sub[3,])
    sub = sub[4:8,c(1,3,47:ncol(sub))]
    sub$`Devolved region`[is.na(sub$`Devolved region`)] = "UK"
    res[[i]] = sub
  }
  res = dplyr::bind_rows(res)
  res[c(1,3:ncol(res))] = lapply(res[c(1,3:ncol(res))], as.numeric)

  names(res) = c("Year","Devolved region","Total",
                 "Food","Housing","Transport",
                 "Goods","Services","Government and Capital Investment",
                 "Bread and cereals","Meat","Fish and seafood",
                 "Dairy and eggs","Fruit","Vegetables",
                 "Beverages","Other_food","Electricity",
                 "Gas and other fuels","Water and waste","Maintenance and repair of the dwelling",
                 "Other_housing","Private transport","Public transport",
                 "Other transport services","Aviation","Clothes",
                 "Furniture and homeware","Electrical appliances","Medicines and medical equipment",
                 "Hobbies, pets and sports","Paper and stationery","Other_goods",
                 "Healthcare","Communication","Education",
                 "Restaurants and cafes","Hotels","Finance and insurance",
                 "Other_services","Government","Capital Investment and other" )

  res


}

make_consumption_scot_wales = function(consumption_uk, consumption_england, consumption_la){

  consumption_uk = consumption_uk[consumption_uk$year %in% unique(consumption_england$year),]
  names(consumption_uk)[names(consumption_uk) == "value"] = "value_uk"
  names(consumption_england)[names(consumption_england) == "value"] = "value_england"

  consumption_uk2 = dplyr::left_join(consumption_uk, consumption_england, by = c("year","name","desc","group"))
  consumption_uk2$valueRUK = consumption_uk2$value_uk - consumption_uk2$value_england




  #TODO: Categories don't match so just use an overall weight
  consumption_la = consumption_la[,c("Year","Devolved region","Total")]
  consumption_la = tidyr::pivot_wider(consumption_la, names_from = "Devolved region", values_from = "Total")
  consumption_la$RUK = consumption_la$Scotland + consumption_la$Wales + consumption_la$`Northern Ireland`

  consumption_la$Scotland_share = consumption_la$Scotland / consumption_la$RUK
  consumption_la$Wales_share = consumption_la$Wales / consumption_la$RUK
  consumption_la$NI_share = consumption_la$`Northern Ireland` / consumption_la$RUK

  consumption_uk2 = dplyr::left_join(consumption_uk2,
                                     consumption_la[,c("Year","Scotland_share","Wales_share","NI_share")],
                                     by = c("year"= "Year")
  )


  consumption_uk2$value_scotland = consumption_uk2$valueRUK * consumption_uk2$Scotland_share
  consumption_uk2$value_wales = consumption_uk2$valueRUK * consumption_uk2$Wales_share
  consumption_uk2$value_ni = consumption_uk2$valueRUK * consumption_uk2$NI_share

  consumption_uk2 = consumption_uk2[,c("year","name","desc","group","value_uk",
                                       "value_england","value_scotland","value_wales","value_ni")]

  consumption_uk2
}

# Old Method
# calculate_consumption_lsoa = function(consumption_uk, consumption_income, population, income_lsoa, domestic_electricity){
#
#   check_value = sum(consumption_uk$value[consumption_uk$year == 2018 & consumption_uk$group == "nutrition"]) * 1e6
#
#   # Use electricity metres as a measure of housholds
#   households = domestic_electricity[,c("LSOA21CD","year","metres")]
#
#   #Prep population
#   population$adults = rowSums(population[,c("20-24","25-29","30-34","35-39",
#                                             "40-44","45-49","50-54","55-59",
#                                             "60-64","65-69","70-74","75-79",
#                                             "80-84","85+")])
#   population = population[,c("year","LSOA21CD","all_ages","adults")]
#   population = dplyr::left_join(population, households, by  = c("LSOA21CD", "year"))
#   population = population[population$year >= 2010,]
#   population = population[population$year <= 2021,]
#   population$peopel_per_houseshold = population$all_ages / population$metres
#   population$adults_per_houseshold = population$adults / population$metres
#   # Very high occupations is probably students etc
#   population$households <- dplyr::if_else(population$adults_per_houseshold < 3,
#                                           population$metres, round(population$adults / 1.7588)) # Median Value
#   population = population[,c("year","LSOA21CD","all_ages","households")]
#
#   # Group shares
#   consumption_uk = dplyr::group_by(consumption_uk, year, group)
#   consumption_uk = dplyr::summarise(consumption_uk,value = sum(value))
#   consumption_uk = dplyr::ungroup(consumption_uk)
#
#   #TODO: weight as GB is not whole UK
#
#   consumption_uk$value = consumption_uk$value * 1e6 # Convert from ktonnes to kg
#   consumption_uk = tidyr::pivot_wider(consumption_uk, names_from = "group", values_from = "value")
#
#   if(check_value != consumption_uk$nutrition[consumption_uk$year == 2018]){
#     stop("Check failed")
#   }
#
#   # Convert Income Based Consumption to shares
#   consumption_income = consumption_income[,c("income_group","gas_electric_toe_household",
#                                              "other_shelter_toes_household","nutrition_toe_household",
#                                              "flights_toes_household","mobility_toes_household",
#                                              "consumables_toe_household","recreation_toes_household",
#                                              "services_toes_household","total_toes_household")]
#   consumption_income[2:ncol(consumption_income)] = lapply(consumption_income[2:ncol(consumption_income)],
#                                                           function(x){x/sum(x)})
#   # consumption_income[2:ncol(consumption_income)] = lapply(consumption_income[2:ncol(consumption_income)],
#   #                                                         function(x){x/mean(x)})
#   names(consumption_income) = gsub("_toes_household","",names(consumption_income))
#   names(consumption_income) = gsub("_toe_household","",names(consumption_income))
#   consumption_income$total = NULL
#
#   if(!all(sapply(consumption_income[2:ncol(consumption_income)], function(x){round(sum(x),5)}) == 1)){
#     stop("Weights don't add to 1")
#   }
#
#   income_lsoa$income_band <- income_bands(income_lsoa$income_lsoa)
#
#   # Make Bands for years and incomes
#   consumption_band = list()
#   for(i in 1:20){
#     sub = consumption_uk
#     sub$income_group = i - 1
#     consumption_band[[i]] = sub
#   }
#   consumption_band = dplyr::bind_rows(consumption_band)
#   consumption_band = consumption_band[order(consumption_band$year),]
#
#   names(consumption_income) = paste0(names(consumption_income),"_wt")
#
#   consumption_band = dplyr::left_join(consumption_band, consumption_income, by = c("income_group" = "income_group_wt"))
#   consumption_band = as.data.frame(consumption_band)
#
#   for(i in c("consumables","gas_electric","nutrition","other_shelter","recreation","services")){
#     consumption_band[paste0(i,"_emissions")] = consumption_band[i] * consumption_band[paste0(i,"_wt")]
#   }
#   consumption_band = consumption_band[,c("year","income_group","consumables_emissions",
#                                          "gas_electric_emissions","nutrition_emissions",
#                                          "other_shelter_emissions","recreation_emissions",
#                                          "services_emissions")]
#
#
#   if(round(sum(consumption_band$nutrition_emissions[consumption_band$year == 2018])) != round(check_value)){
#     stop("consumption does not match")
#   }
#
#
#   #TODO: change income over time
#   lsoa <- dplyr::left_join(population, income_lsoa, by = "LSOA21CD")
#   #lsoa = lsoa[lsoa$year == 2018,]
#   lsoa = lsoa[!is.na(lsoa$income_band),]
#
#   lsoa = dplyr::left_join(lsoa, consumption_band, by = c("year" = "year","income_band" = "income_group"))
#
#   if(round(sum(consumption_band$nutrition_emissions[consumption_band$year == 2018])) != round(check_value)){
#     stop("consumption does not match")
#   }
#
#   # Convert to emissions per LSOA
#   # Get number of LSOA in each band and proportion of households
#   lsoa = dplyr::group_by(lsoa, year, income_band)
#   lsoa = dplyr::mutate(lsoa, n_lsoa_band = dplyr::n())
#   lsoa = dplyr::mutate(lsoa, households_band = sum(households))
#   lsoa = dplyr::ungroup(lsoa)
#   lsoa$housholds_share = lsoa$households/lsoa$households_band
#
#   for(i in c("consumables","gas_electric","nutrition","other_shelter","recreation","services")){
#     lsoa[paste0(i,"_emissions")] = lsoa[paste0(i,"_emissions")] * lsoa$housholds_share
#   }
#
#   #sum(lsoa$nutrition_emissions[lsoa$year == 2018])
#
#   # Convert to emissions per person
#   for(i in c("consumables","gas_electric","nutrition","other_shelter","recreation","services")){
#     lsoa[paste0(i,"_emissions_percap")] = lsoa[paste0(i,"_emissions")] / lsoa$all_ages
#   }
#
#   tot_pp = sum(lsoa$nutrition_emissions_percap[lsoa$year == 2018] * lsoa$all_ages[lsoa$year == 2018])
#
#   if((sqrt((tot_pp - check_value)**2)/check_value) > 0.0001){
#     stop("Per person emissions dont match total")
#   }
#
#   lsoa = lsoa[,c("year","LSOA21CD",names(lsoa)[grepl("_percap",names(lsoa))])]
#   lsoa
#
# }

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



load_consumption_multipliers = function(path = "../inputdata/consumption/Defra22_results_UK.ods"){
    cons = readODS::read_ods(path, sheet = "ghg_coicop_mult")
    names(cons)[1] = "product"

    cons = tidyr::pivot_longer(cons, cols = names(cons)[2:ncol(cons)], names_to = "year")
    names(cons)[3] = "ghg_pound"
    cons


  }
