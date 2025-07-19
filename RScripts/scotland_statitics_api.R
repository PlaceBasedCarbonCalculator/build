devtools::install_github(
  "ScotGovAnalysis/opendatascot",
  upgrade = "never",
  build_vignettes = TRUE
)
library(opendatascot)

struct = ods_structure("dwellings-type", labelled = TRUE)

# Fails
# dat <- ods_dataset(dataset = "dwellings-type",
#                                  geography = "dz")


csv = ods_get_csv("dwellings-type")


csv2 = csv[csv$Units == "Dwellings",]
csv_11 = csv2[csv2$FeatureType == "2011 Data Zone",]
csv_01 = csv2[csv2$FeatureType == "2001 Data Zone",]
table(csv_11$DateCode)
table(csv_01$DateCode)

csv_11= csv_11[,c("FeatureCode","FeatureName","DateCode","Type Of Dwelling","Value")]
csv_11 = tidyr::pivot_wider(csv_11, names_from = "Type Of Dwelling", values_from = "Value")

csv_01= csv_01[,c("FeatureCode","FeatureName","DateCode","Type Of Dwelling","Value")]
csv_01 = tidyr::pivot_wider(csv_01, names_from = "Type Of Dwelling", values_from = "Value")

write.csv(csv_11, "../inputdata/statistics_scotland/housing_type_datazone11.csv", row.names = FALSE)
write.csv(csv_01, "../inputdata/statistics_scotland/housing_type_datazone01.csv", row.names = FALSE)
