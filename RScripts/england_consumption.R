library(targets)
path_england = "../inputdata/consumption/Defra22_results_England.ods"
path_UK = "../inputdata/consumption/Defra22_results_UK.ods"

res_eng = load_consumption_footprint(path = path_england, sheet = "Summary_product_01-22")
res_UK = load_consumption_footprint(path_UK, "Summary_product_90-22")

tar_load(consumption_england)
tar_load(consumption_uk)
tar_load(consumption_la)









res = res[,]


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
  )
)


name_pairs <- c(

  # Food-related
  "Food" = "Food",
  "Beverages" = "Non-alcoholic beverages",
  "Other_food" = "Alcoholic beverages",

  # Housing-related
  "Housing" = "Actual rentals for households",
  "Other_housing" = "Imputed rentals for households",
  "Maintenance and repair of the dwelling" = "Maintenance and repair of the dwelling",
  "Water and waste" = "Water supply and miscellaneous dwelling services",
  "Electricity" = "Electricity, gas and other fuels",
  "Gas and other fuels" = "Electricity, gas and other fuels",

  # Transport
  "Transport" = "Transport services",
  "Private transport" = "Operation of personal transport equipment",
  "Public transport" = "Transport services",
  "Other transport services" = "Transport services",
  "Aviation" = NA,

  # Goods
  "Goods" = "Miscellaneous goods and services",
  "Clothes" = "Clothing",
  "Furniture and homeware" = "Furniture, furnishings, carpets etc",
  "Electrical appliances" = "Household appliances",
  "Paper and stationery" = "Newspapers, books and stationery",
  "Other_goods" = "Miscellaneous goods and services",

  # Services
  "Services" = "Miscellaneous goods and services",
  "Healthcare" = "Hospital services",
  "Communication" = "Telephone and telefax services",
  "Education" = "Education",
  "Restaurants and cafes" = "Restaurants and hotels",
  "Hotels" = "Restaurants and hotels",
  "Finance and insurance" = NA,
  "Other_services" = "Miscellaneous goods and services",

  # Government & Capital
  "Government and Capital Investment" = NA,
  "Government" = NA,
  "Capital Investment and other" = NA,

  # Miscellaneous
  "Medicines and medical equipment" = "Medical products, appliances and equipment",
  "Hobbies, pets and sports" = "Other recreational equipment etc"
)
