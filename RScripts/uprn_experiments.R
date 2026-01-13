# Classificy UPRN as domestic / non domesitc / unknown and combiend price and EPC data
library(dplyr)
library(sf)
library(targets)
library(tmap)

tar_load(bounds_postcodes_2024)
tar_load(parameters)
tar_load(uprn_historical)
epc_dom = readRDS(file.path(parameters$path_data,"epc/GB_domestic_epc.Rds"))
epc_non = readRDS(file.path(parameters$path_data,"epc/GB_nondomestic_epc.Rds"))
tar_load(house_prices_nowcast)



summary(epc_dom$UPRN %in% epc_non$UPRN)

uprn_historical$epc_dom = uprn_historical$UPRN %in% epc_dom$UPRN
uprn_historical$epc_nondom = uprn_historical$UPRN %in% epc_non$UPRN

uprn_historical$lr_dom = uprn_historical$UPRN %in% house_prices_nowcast$uprn[house_prices_nowcast$property_type != "O"]
uprn_historical$lr_nondom = uprn_historical$UPRN %in% house_prices_nowcast$uprn[house_prices_nowcast$property_type == "O"]


uprn_historical <- uprn_historical %>%
  mutate(
    domestic = case_when(
      !epc_dom & !epc_nondom & !lr_dom & !lr_nondom ~ "unknown",
      (epc_dom | lr_dom) & !(epc_nondom | lr_nondom) ~ "domestic",
      (epc_nondom | lr_nondom) & !(epc_dom | lr_dom) ~ "non-domestic",
      (epc_dom & epc_nondom) & (!lr_nondom & !lr_dom)  ~ "ambiguous epc",
      (!epc_dom & !epc_nondom) & (lr_nondom & lr_dom)  ~ "ambiguous lr",
      TRUE ~ "ambiguous other"
    )
  )

# Is it a old or new UPRN
uprn_historical <- uprn_historical %>%
  mutate(
    exists = date_last == lubridate::ymd("2025-11-01"),
    newbuild = date_first > lubridate::ymd("2020-06-01")
    )

# Join on Price data
house_prices_nowcast$date = as.Date(house_prices_nowcast$date)

uprn_historical = left_join(uprn_historical,
                            house_prices_nowcast[,c("uprn","price","price_2024","date","property_type","freehold","address1","address2")],
                            by = c("UPRN" = "uprn"))
# Non existant properties can't have a price
uprn_historical$price_2024[!uprn_historical$exists] = NA

summary(duplicated(epc_dom$UPRN))

epc_dom = epc_dom[order(epc_dom$year, decreasing = TRUE),]
epc_dom = epc_dom[!duplicated(epc_dom$UPRN),]

epc_non = epc_non[order(epc_non$year, decreasing = TRUE),]
epc_non = epc_non[!duplicated(epc_non$UPRN),]

# Make three datasets, domestic, non-domestic, and unknown

uprn_historical_dom = left_join(uprn_historical, epc_dom,
                                by = c("UPRN" = "UPRN"))
uprn_historical_dom = uprn_historical_dom[!uprn_historical_dom$domestic %in% c("non-domestic","unknown"),]


uprn_historical_nondom = left_join(uprn_historical, epc_non,
                                by = c("UPRN" = "UPRN"))
uprn_historical_nondom = uprn_historical_nondom[!uprn_historical_nondom$domestic %in% c("domestic","unknown"),]

uprn_historical_unknown = uprn_historical[uprn_historical$domestic == "unknown",]

uprn_historical_dom = uprn_historical_dom[,!names(uprn_historical_dom) %in% c("epc_dom","epc_nondom","lr_dom","lr_nondom")]
uprn_historical_nondom = uprn_historical_nondom[,!names(uprn_historical_nondom) %in% c("epc_dom","epc_nondom","lr_dom","lr_nondom")]
uprn_historical_unknown = uprn_historical_unknown[,!names(uprn_historical_unknown) %in% c("epc_dom","epc_nondom","lr_dom","lr_nondom")]

uprn_historical_unknown = uprn_historical_unknown[,c("UPRN","date_first","date_last","X_COORDINATE","Y_COORDINATE","LATITUDE","LONGITUDE","domestic","exists","newbuild")]

summary(uprn_historical_dom$exists)

# Check quality of UPRN matches (some suspisouse matches)
# Perhaps 17,000 bad UPRNs

uprn_historical_dom = st_as_sf(uprn_historical_dom, coords = c("X_COORDINATE","Y_COORDINATE"), crs = 27700)
names(bounds_postcodes_2024) = c("POSTCODE_join","PC_AREA","geometry")
bounds_postcodes_2024 = bounds_postcodes_2024[substr(bounds_postcodes_2024$POSTCODE_join,1,1) != "V",] #Remove postboxes?

# 23909300 rows
uprn_historical_dom = st_join(uprn_historical_dom, bounds_postcodes_2024)
# 23912993 rows
uprn_historical_dom$postcode_match = uprn_historical_dom$POSTCODE == uprn_historical_dom$POSTCODE_join

foo_na = uprn_historical_dom[is.na(uprn_historical_dom$postcode_match),]
foo_false = uprn_historical_dom[!is.na(uprn_historical_dom$postcode_match),]
foo_false = foo_false[!foo_false$postcode_match,]
foo_false = foo_false[,c("UPRN","date_first","date_last","domestic","address1","address2","addr","ADDRESS2","ADDRESS3","POSTCODE","POSTCODE_join","PC_AREA","postcode_match" )]

foo_na = foo_na[,c("UPRN","date_first","date_last","domestic","address1","address2","addr","ADDRESS2","ADDRESS3","POSTCODE","POSTCODE_join","PC_AREA","postcode_match" )]

foo_false$outcode1 = sapply(strsplit(foo_false$POSTCODE," "),`[[`,1)
foo_false$outcode2 = sapply(strsplit(foo_false$POSTCODE_join," "),`[[`,1)

foo_false$outcode_match = foo_false$outcode1 == foo_false$outcode2

foo_false2 = foo_false[!foo_false$outcode_match, ] # most seem to be just accross the outcoude boundary
foo_false2$area_match = stringi::stri_detect_regex(foo_false2$outcode1, foo_false2$PC_AREA)
# Really small number ~ 553 that are in very much the wrong area
# summary(foo_false2$area_match)
# Mode   FALSE    TRUE
# logical     553   16504

qtm(foo_false2[!foo_false2$area_match,])

foo_false3 = foo_false2[!foo_false2$area_match,]

epc_dom_bad = epc_dom[epc_dom$UPRN %in% foo_false3$UPRN,]

