#' Download Consumption Footprint
#'
#' @description Download the consumption footprint resource and return the local file path.
#' @details This function is used as part of the pipeline input ingestion stage.
#' @param path){ Input object or parameter named `path){`.
#' @return The local path or file name of the downloaded resource.
#' @keywords internal
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

#' Load Consumption Footprint
#'
#' @description Load consumption footprint data from the source path and return it as an R object.
#' @details This function is used as part of the pipeline input ingestion stage.
#' @param path File or directory path.
#' @param sheet Input object or parameter named `sheet`.
#' @return A data frame containing the loaded dataset.
#' @keywords internal
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


#' Load Consumption Income
#'
#' @description Load consumption income data from the source path and return it as an R object.
#' @details This function is used as part of the pipeline input ingestion stage.
#' @param path){ Input object or parameter named `path){`.
#' @return A data frame containing the loaded dataset.
#' @keywords internal
load_consumption_income = function(path){
  income = read.csv(file.path(path,"income_energy_fooprint.csv"))
  income
}


#' Load La Consumption Accounts
#'
#' @description Load la consumption accounts data from the source path and return it as an R object.
#' @details This function is used as part of the pipeline input ingestion stage.
#' @param path File or directory path.
#' @return A data frame containing the loaded dataset.
#' @keywords internal
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

#' Make Consumption Scot Wales
#'
#' @description Build consumption scot wales and return the generated output.
#' @param consumption_uk Input object or parameter named `consumption_uk`.
#' @param consumption_england Input object or parameter named `consumption_england`.
#' @param consumption_la){ Input object or parameter named `consumption_la){`.
#' @return A generated data object, usually a data frame or spatial feature collection.
#' @keywords internal
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


# Convert incomes into ventiles
#' Income Bands
#'
#' @description Perform processing for income bands.
#' @param dat){ Input object or parameter named `dat){`.
#' @return A data frame produced by the function.
#' @keywords internal
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



#' Load Consumption Multipliers
#'
#' @description Load consumption multipliers data from the source path and return it as an R object.
#' @details This function is used as part of the pipeline input ingestion stage.
#' @param path File or directory path.
#' @return A data frame containing the loaded dataset.
#' @keywords internal
load_consumption_multipliers = function(path = "../inputdata/consumption/Defra22_results_UK.ods"){
    cons = readODS::read_ods(path, sheet = "ghg_coicop_mult")
    names(cons)[1] = "product"

    cons = tidyr::pivot_longer(cons, cols = names(cons)[2:ncol(cons)], names_to = "year")
    names(cons)[3] = "ghg_pound"
    cons


  }
