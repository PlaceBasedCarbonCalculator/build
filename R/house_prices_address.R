# Match Up Land Registry Data with UPRN when Possible
# Or Postcode Centroid when not

land_registry_add_uprn = function(house_price_lr,
                                  house_prices_ubdc,
                                  uprn_historical,
                                  lookup_postcode_OA_LSOA_MSOA_2021,
                                  bounds_lsoa_GB_full,
                                  path_epc = file.path(parameters$path_data,"epc/GB_domestic_epc.Rds"),
                                  path_epc_nondom = file.path(parameters$path_data,"epc/GB_nondomestic_epc.Rds")){

  # Load Data
  house_price_lr = house_price_lr[!duplicated(house_price_lr$transactionid),]
  epc = readRDS(path_epc)
  epc = st_drop_geometry(epc)
  epc = epc[,c("UPRN","addr","ADDRESS2","ADDRESS3","POSTCODE","year")]
  epc_nondom = readRDS(path_epc_nondom)
  epc_nondom = st_drop_geometry(epc_nondom)
  epc_nondom = epc_nondom[,c("UPRN","adr1","adr2","adr3","postcode","year")]

  uprn_historical = st_as_sf(uprn_historical, coords = c("X_COORDINATE","Y_COORDINATE"), crs = 27700)
  lookup_postcode_OA_LSOA_MSOA_2021 = lookup_postcode_OA_LSOA_MSOA_2021[,c("pcds","lsoa21cd")]
  names(lookup_postcode_OA_LSOA_MSOA_2021) = c("pcds","LSOA21CD")

  # Join On ubdc data
  house_price_lr = dplyr::left_join(house_price_lr, house_prices_ubdc, by = "transactionid")

  # Split out with(out) UPRN
  lr_withuprn = house_price_lr[!is.na(house_price_lr$uprn),]
  lr_nouprn = house_price_lr[is.na(house_price_lr$uprn),]
  # Small number of transactionid in the UBDC data are missing in the LR data? About 29439

  # Check for matching addresses
  unique_address = lr_withuprn[,c("postcode","property_type",
                                  "address1","address2","address3","address4",
                                  "town","la","county","uprn","parentuprn","usrn")]

  unique_address = unique_address[!duplicated(unique_address[,c("postcode","property_type",
                                                                "address1","address2","address3",
                                                                "address4","town","la","county")]),]

  # Match Based on Address
  lr_nouprn$uprn = NULL
  lr_nouprn$usrn = NULL
  lr_nouprn$parentuprn = NULL

  lr_nouprn = dplyr::left_join(lr_nouprn,
                               unique_address,
                               by = c("postcode","property_type",
                                      "address1","address2","address3",
                                      "address4","town","la","county"))

  lr_withuprn = rbind(lr_withuprn, lr_nouprn[!is.na(lr_nouprn$uprn),])
  lr_nouprn = lr_nouprn[is.na(lr_nouprn$uprn),]
  lr_nouprn$uprn = NULL
  lr_nouprn$parentuprn = NULL
  lr_nouprn$usrn = NULL

  #nrow(lr_nouprn) + nrow(lr_withuprn) == nrow(house_price_lr) # TRUE
  rm(unique_address)
  # Now Try to match based on EPC Address
  #summary(epc$UPRN %in% lr_withuprn$uprn)
  # 9,865,292 UPRNs with EPC but no Land Registry Data (many may be Scotland)

  # Clean Addresses for Joining
  lr_nouprn$address1[is.na(lr_nouprn$address1)] = ""
  lr_nouprn$address2[is.na(lr_nouprn$address2)] = ""
  lr_nouprn$address3[is.na(lr_nouprn$address3)] = ""
  lr_nouprn$address4[is.na(lr_nouprn$address4)] = ""

  # Check addresses against EPCs
  lr_nouprn$join_address <- trimws(gsub("\\s+", " ",
                                                      paste(lr_nouprn$address1,
                                                            lr_nouprn$address2,
                                                            lr_nouprn$address3)))

  # Two slightly differnt approaches to addresses are being harmonised
  lr_nouprn$join_address2 <- trimws(gsub("\\s+", " ",
                                                       paste(lr_nouprn$address1,
                                                             lr_nouprn$address2,
                                                             lr_nouprn$address3,
                                                             lr_nouprn$address4)))

  epc$addr[is.na(epc$addr)] = ""
  epc$ADDRESS2[is.na(epc$ADDRESS2)] = ""
  epc$ADDRESS3[is.na(epc$ADDRESS3)] = ""

  epc$join_address <- toupper(trimws(gsub("\\s+", " ",
                                          paste(epc$addr,
                                                epc$ADDRESS2,
                                                epc$ADDRESS3))))
  epc$join_address <- gsub(",","",epc$join_address) # Some addresses have commas

  # Small number of duplicated addresses with different UPRNs
  # All very close to each other. Possible multiple UPRN for same address?
  # Take the newest one as definitive version
  epc = epc[,c("UPRN","join_address","POSTCODE","year")]
  names(epc) = c("uprn","join_address","POSTCODE","year")
  epc = epc[order(epc$year, decreasing = TRUE),]
  epc$year = NULL
  epc = epc[!duplicated(st_drop_geometry(epc[,c("join_address","POSTCODE")])),]

  lr_nouprn = dplyr::left_join(lr_nouprn, epc, by = c("join_address" = "join_address", "postcode" = "POSTCODE"))

  lr_nouprn_good = lr_nouprn[!is.na(lr_nouprn$uprn),] # 1,000,739
  lr_nouprn_bad = lr_nouprn[is.na(lr_nouprn$uprn),] #1,938,708

  lr_withuprn = dplyr::bind_rows(lr_withuprn, lr_nouprn_good)
  rm(lr_nouprn_good, lr_nouprn)

  lr_nouprn_bad$uprn = NULL

  lr_nouprn_bad2 = dplyr::left_join(lr_nouprn_bad, epc, by = c("join_address2" = "join_address", "postcode" = "POSTCODE"))

  lr_nouprn_good2 = lr_nouprn_bad2[!is.na(lr_nouprn_bad2$uprn),] # 470,692
  lr_nouprn_bad2 = lr_nouprn_bad2[is.na(lr_nouprn_bad2$uprn),] # 1,468,016

  lr_withuprn = dplyr::bind_rows(lr_withuprn, lr_nouprn_good2)

  lr_nouprn_bad2$uprn = NULL
  rm(lr_nouprn_bad, lr_nouprn_good2)

  # Now Try Non-Dom EPCs
  epc_nondom$adr1[is.na(epc_nondom$adr1)] = ""
  epc_nondom$adr2[is.na(epc_nondom$adr2)] = ""
  epc_nondom$adr3[is.na(epc_nondom$adr3)] = ""

  epc_nondom$join_address <- toupper(trimws(gsub("\\s+", " ",
                                          paste(epc_nondom$adr1,
                                                epc_nondom$adr2,
                                                epc_nondom$adr3))))
  epc_nondom$join_address <- gsub(",","",epc_nondom$join_address)

  epc_nondom = epc_nondom[,c("UPRN","join_address","postcode","year")]
  names(epc_nondom) = c("uprn","join_address","postcode","year")
  epc_nondom = epc_nondom[order(epc_nondom$year, decreasing = TRUE),]
  epc_nondom$year = NULL
  epc_nondom = epc_nondom[!duplicated(st_drop_geometry(epc_nondom[,c("join_address","postcode")])),]


  lr_nouprn_bad2 = dplyr::left_join(lr_nouprn_bad2, epc_nondom,
                               by = c("join_address" = "join_address", "postcode" = "postcode"))
  lr_nouprn_bad2_good = lr_nouprn_bad2[!is.na(lr_nouprn_bad2$uprn),]
  lr_nouprn_bad2_bad = lr_nouprn_bad2[is.na(lr_nouprn_bad2$uprn),]

  lr_withuprn = dplyr::bind_rows(lr_withuprn, lr_nouprn_bad2_good)
  rm(lr_nouprn_bad2_good, lr_nouprn_bad2)

  lr_nouprn_bad2_bad$uprn = NULL

  lr_nouprn_bad3 = dplyr::left_join(lr_nouprn_bad2_bad, epc_nondom,
                         by = c("join_address2" = "join_address", "postcode" = "postcode"))

  lr_nouprn_bad3_good = lr_nouprn_bad3[!is.na(lr_nouprn_bad3$uprn),]
  lr_nouprn_bad3_bad = lr_nouprn_bad3[is.na(lr_nouprn_bad3$uprn),]

  lr_withuprn = dplyr::bind_rows(lr_withuprn, lr_nouprn_bad3_good)
  rm(lr_nouprn_bad3_good, lr_nouprn_bad3)

  lr_nouprn_bad3_bad$uprn = NULL

  # Add LSOA from postcode
  #summary(lr_nouprn_bad3_bad$postcode %in% lookup_postcode_OA_LSOA_MSOA_2021$pcds)
  # Mode   FALSE    TRUE
  # logical   57800 1373229
  # Mostly NA postcodes

  lr_nouprn_bad3_bad = dplyr::left_join(lr_nouprn_bad3_bad,
                                    lookup_postcode_OA_LSOA_MSOA_2021,
                                    by = c("postcode" = "pcds"))

  lr_nouprn_bad3_bad$join_address = NULL
  lr_nouprn_bad3_bad$join_address2 = NULL

  lr_withuprn$LATITUDE = NULL
  lr_withuprn$LONGITUDE = NULL
  lr_withuprn$join_address = NULL
  lr_withuprn$join_address2 = NULL

  lr_withuprn = dplyr::left_join(lr_withuprn, uprn_historical, by = c("uprn" = "UPRN"))
  lr_withuprn = sf::st_as_sf(lr_withuprn)
  lr_withuprn = sf::st_join(lr_withuprn, bounds_lsoa_GB_full)
  lr_withuprn = sf::st_drop_geometry(lr_withuprn)

  lr_final = dplyr::bind_rows(lr_withuprn, lr_nouprn_bad3_bad)

  lr_final

}



house_price_lsoa_summary = function(house_price_lr_uprn){

  house_price_lr_uprn$year = lubridate::year(house_price_lr_uprn$date)

  house_price_lsoa = house_price_lr_uprn |>
    dplyr::group_by(LSOA21CD, year) |>
    dplyr::summarise(transactions = dplyr::n(),
                     price_min = min(price),
                     price_25 = quantile(price, 0.25),
                     price_median = median(price),
                     price_75 = quantile(price, 0.75),
                     price_max = max(price)
    )

  house_price_lsoa = house_price_lsoa[!is.na(house_price_lsoa$LSOA21CD),]

  house_price_lsoa
}

# Extrapolate todays price from hisotrical price
house_price_extrapolate = function(house_price_lr_uprn, lsoa_admin){

  lsoa_admin = lsoa_admin[,c("LSOA21CD","LAD25CD")]

  house_price_lr_uprn = dplyr::left_join(house_price_lr_uprn, lsoa_admin, by = "LSOA21CD")
  house_price_lr_uprn$year = lubridate::year(house_price_lr_uprn$date)

  house_price_la = house_price_lr_uprn |>
    dplyr::group_by(LAD25CD, year, property_type) |>
    dplyr::summarise(price_median = median(price),
                     transactions = dplyr::n())

  # Not enough transactions per year for the O type
  house_price_la_general = house_price_lr_uprn |>
    dplyr::group_by(LAD25CD, year) |>
    dplyr::summarise(price_median = median(price),
                     transactions = dplyr::n())

  house_price_la = house_price_la[!is.na(house_price_la$LAD25CD),]
  house_price_la = house_price_la[house_price_la$property_type != "O",]

  house_price_la_O = house_price_la_general
  house_price_la_O = house_price_la_O[!is.na(house_price_la_O$LAD25CD),]
  house_price_la_O$property_type = "O"
  house_price_la$property_type = as.character(house_price_la$property_type)

  house_price_la = rbind(house_price_la, house_price_la_O)
  house_price_la = dplyr::ungroup(house_price_la)

  # Get change to 2024

  # 1) Build a lookup of 2024 prices for each LAD25CD + property_type
  price_2024_lookup <- house_price_la |>
    dplyr::filter(year == 2024) |>
    dplyr::select(
      LAD25CD,
      property_type,
      price_median_2024 = price_median
    )

  # 2) Join back to the full table
  house_price_la_w_growth <- house_price_la |>
    dplyr::left_join(price_2024_lookup, by = c("LAD25CD", "property_type")) |>
    # 3) Compute growth multiple: "how many times the price has increased"
    dplyr::mutate(
      growth_multiple = dplyr::case_when(
        !is.na(price_median_2024) & price_median > 0 ~ price_median_2024 / price_median,
        TRUE ~ NA_real_
      )
    )



  uprn_latest = house_price_lr_uprn[order(house_price_lr_uprn$date, decreasing = TRUE),]
  uprn_latest = uprn_latest[!duplicated(uprn_latest$uprn),] # 15 million properties

  uprn_latest$year = lubridate::year(uprn_latest$date)

  uprn_latest = dplyr::left_join(uprn_latest,
                                 house_price_la_w_growth[,c("LAD25CD","year","property_type","growth_multiple")],
                                 by = c("LAD25CD","year","property_type")
                                 )

  uprn_latest$price_2024 = round(uprn_latest$price * uprn_latest$growth_multiple/1000,0) * 1000

  uprn_latest

}





