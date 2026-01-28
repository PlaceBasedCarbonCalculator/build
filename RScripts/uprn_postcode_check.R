library(dplyr)
library(sf)
library(targets)
library(tmap)
library(nngeo)
library(tmap)
tmap_mode("view")

tar_load(bounds_postcodes_2024)
tar_load(parameters)
tar_load(uprn_historical_epc_lr)
epc_dom = readRDS(file.path(parameters$path_data,"epc/GB_domestic_epc.Rds"))
epc_non = readRDS(file.path(parameters$path_data,"epc/GB_nondomestic_epc.Rds"))


names(bounds_postcodes_2024) = c("POSTCODE_uprn","PC_AREA","geometry")
bounds_postcodes_2024 = bounds_postcodes_2024[substr(bounds_postcodes_2024$POSTCODE_uprn,1,1) != "V",] #Remove postboxes?

uprn_historical_epc_lr_dom = st_as_sf(uprn_historical_epc_lr$domestic, coords = c("X_COORDINATE","Y_COORDINATE"), crs = 27700)

uprn_historical_epc_lr_dom = st_join(uprn_historical_epc_lr_dom, bounds_postcodes_2024)
# 23912993 rows
uprn_historical_epc_lr_dom$postcode_match = uprn_historical_epc_lr_dom$POSTCODE == uprn_historical_epc_lr_dom$POSTCODE_uprn

foo_na = uprn_historical_epc_lr_dom[is.na(uprn_historical_epc_lr_dom$postcode_match),]
foo_false = uprn_historical_epc_lr_dom[!is.na(uprn_historical_epc_lr_dom$postcode_match),]
foo_false = foo_false[!foo_false$postcode_match,]

cents = bounds_postcodes_2024[bounds_postcodes_2024$POSTCODE_uprn %in% unique(c(foo_false$POSTCODE, foo_false$POSTCODE_uprn)),]
cents = st_centroid(cents)

dists = as.numeric(st_distance(cents[match(foo_false$POSTCODE, cents$POSTCODE_uprn),],
                    cents[match(foo_false$POSTCODE_uprn, cents$POSTCODE_uprn),],
                    by_element = TRUE))

foo_false$distance = dists

bar = foo_false[is.na(foo_false$distance),]
foo_false = foo_false[!is.na(foo_false$distance),]
foo_false = foo_false[foo_false$distance > 10000,]
foo_false = foo_false[,c("UPRN","addr","ADDRESS2","ADDRESS3","POSTCODE","POSTCODE_uprn","postcode_match","distance")]
foo_false$UPRN = as.character(foo_false$UPRN)
write.csv(st_drop_geometry(foo_false),"data/bad_uprns2.csv",row.names = FALSE, na = "")

epc_both = uprn_historical_epc_lr_dom[uprn_historical_epc_lr_dom$domestic == "ambiguous epc",]

epc_both_dom = epc_dom[epc_dom$UPRN %in% epc_both$UPRN,]
epc_both_non = epc_non[epc_non$UPRN %in% epc_both$UPRN,]


epc_both2 = full_join(st_drop_geometry(epc_both_dom[,c("UPRN","addr","ADDRESS2","ADDRESS3","POSTCODE")]),
                      st_drop_geometry(epc_both_non[,c("UPRN","adr1","adr2","adr3","postcode")]),
                      by = "UPRN"
                      )

epc_both2 = epc_both2[!is.na(epc_both2$POSTCODE) & !is.na(epc_both2$postcode),]
epc_both2 = epc_both2[epc_both2$POSTCODE != epc_both2$postcode,]
