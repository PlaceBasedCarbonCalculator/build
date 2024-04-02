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
    "mobility",
    "mobility",
    "mobility",
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
  cons


}


load_consumption_income = function(path){
  income = read.csv(file.path(path,"income_energy_fooprint.csv"))
  income
}
