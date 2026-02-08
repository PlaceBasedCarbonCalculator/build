make_vehicle_summary = function(vehicle_registrations, ulev_registrations){

  vh_long = pivot_longer(vehicle_registrations,
                         cols = names(vehicle_registrations)[3:ncol(vehicle_registrations)],
                         names_sep = "_",
                         names_to = c("BodyType","Keepership","Licence"),
                         values_to = "vehicles")
  ulev_long = pivot_longer(ulev_registrations,
                           cols = names(ulev_registrations)[3:ncol(ulev_registrations)],
                           names_sep = "_",
                           names_to = c("Fuel","Keepership"),
                           values_to = "ulevs")

  vh_long$year = as.integer(gsub(" Q1","",vh_long$quarter))
  ulev_long$year = as.integer(gsub(" Q1","",ulev_long$quarter))
  vh_long$quarter = NULL
  ulev_long$quarter = NULL

  vh_long = vh_long[substr(vh_long$LSOA21CD,1,1) != "N",] # Remove NI
  vh_long = vh_long[vh_long$LSOA21CD != "Miscellaneous",]

  ulev_long = ulev_long[substr(ulev_long$LSOA21CD,1,1) != "N",] # Remove NI
  ulev_long = ulev_long[ulev_long$LSOA21CD != "Miscellaneous",]

  vh_long = vh_long[vh_long$Keepership != "DISPOSAL",]

  vh_long$vehicles[is.na(vh_long$vehicles)] = 0
  ulev_long$ulevs[is.na(ulev_long$ulevs)] = 0

  # Tidy Up the Fuels
  ulev_long$Fuel[ulev_long$Fuel == "BATTERY ELECTRIC"] = "BEV"
  ulev_long$Fuel[ulev_long$Fuel == "PLUG-IN HYBRID ELECTRIC (PETROL)"] = "PHEV"
  ulev_long$Fuel[ulev_long$Fuel == "PLUG-IN HYBRID ELECTRIC (DIESEL)"] = "PHEV"
  ulev_long$Fuel[ulev_long$Fuel == "PETROL"] = "iceULEV"
  ulev_long$Fuel[ulev_long$Fuel == "DIESEL"] = "iceULEV"
  ulev_long$Fuel[ulev_long$Fuel == "HYBRID ELECTRIC (DIESEL)"] = "HEV"
  ulev_long$Fuel[ulev_long$Fuel == "HYBRID ELECTRIC (PETROL)"] = "HEV"
  ulev_long$Fuel[ulev_long$Fuel == "FUEL CELLS"] = "fuelcell"
  ulev_long$Fuel[ulev_long$Fuel == "RANGE EXTENDED ELECTRIC"] = "REEV"

  vh_long$BodyType[vh_long$BodyType == "Other vehicles"] = "Other"

  ulev_long = ulev_long |>
    dplyr::group_by(LSOA21CD,year,Fuel,Keepership) |>
    dplyr::summarise(ulevs = sum(ulevs, na.rm = TRUE))


  length(unique(vh_long$LSOA21CD)) #43064 - correct
  length(unique(ulev_long$LSOA21CD)) #42120 - some missing!

  lsoa_pop = unique(population$LSOA21CD)
  lsoa_vh = unique(vh_long$LSOA21CD)
  lsoa_ulev = unique(ulev_long$LSOA21CD)

  head(lsoa_vh[!lsoa_vh %in% lsoa_pop])
  head(lsoa_ulev[!lsoa_ulev %in% lsoa_pop])

  vh_summary = vh_long |>
    dplyr::group_by(LSOA21CD,year,Keepership) |>
    dplyr::summarise(vehicles = sum(vehicles, na.rm = TRUE))

  vh_summary_alt = vh_long |>
    tidyr::pivot_wider(names_from = c("BodyType","Licence"), values_from = "vehicles", values_fill = 0)

  vh_summary2 = dplyr::left_join(vh_summary, vh_summary_alt, by = c("LSOA21CD","year","Keepership"))

  ulev_summary = pivot_wider(ulev_long, names_from = "Fuel", values_from = "ulevs")
  ulev_summary$ULEVs = rowSums(ulev_summary[,c("BEV","PHEV","iceULEV","HEV","REEV","fuelcell")])

  all_summary = dplyr::left_join(vh_summary2, ulev_summary, by = c("LSOA21CD","year","Keepership"))
  all_summary$vehicles[is.na(all_summary$vehicles)] = 0
  all_summary$BEV[is.na(all_summary$BEV)] = 0
  all_summary$ULEVs[is.na(all_summary$ULEVs)] = 0

  all_summary$HEV[is.na(all_summary$HEV)] = 0
  all_summary$PHEV[is.na(all_summary$PHEV)] = 0
  all_summary$REEV[is.na(all_summary$REEV)] = 0
  all_summary$fuelcell[is.na(all_summary$fuelcell)] = 0
  all_summary$iceULEV[is.na(all_summary$iceULEV)] = 0

  all_summary$ice = all_summary$vehicles - all_summary$ULEVs
  all_summary$ice[all_summary$ice < 0] = 0
  all_summary$pBEV = ifelse(all_summary$vehicles == 0,0,round(all_summary$BEV / all_summary$vehicles * 100,1))
  all_summary$pULEV = ifelse(all_summary$vehicles == 0,0,round(all_summary$ULEVs / all_summary$vehicles * 100,1))


  all_summary_wide = tidyr::pivot_wider(all_summary,
                                        id_cols = c("LSOA21CD","year"),
                                        names_from = "Keepership",
                                        values_from = names(all_summary)[4:ncol(all_summary)])

  all_summary_wide
}
