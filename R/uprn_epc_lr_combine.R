combine_uprn_epc_lr = function(uprn_historical,
                               house_prices_nowcast,
                               path_epc_dom = file.path(parameters$path_data,"epc/GB_domestic_epc.Rds"),
                               path_epc_nondom = file.path(parameters$path_data,"epc/GB_nondomestic_epc.Rds")

                               ){

  epc_dom = readRDS(path_epc_dom)
  epc_non = readRDS(path_epc_nondom)

  epc_dom = sf::st_drop_geometry(epc_dom)
  epc_non = sf::st_drop_geometry(epc_non)

  epc_dom$uprn_date_first = NULL
  epc_dom$uprn_date_last = NULL
  epc_non$uprn_date_first = NULL
  epc_non$uprn_date_last = NULL

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
  # Non existent properties can't have a price
  uprn_historical$price_2024[!uprn_historical$exists] = NA

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

  # Output Result
  list(domestic = uprn_historical_dom,
       nondomestic = uprn_historical_nondom,
      unknown = uprn_historical_unknown)



}
