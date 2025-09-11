
# Convert vehicle registration to single dataset with 2021 boundaries
vehicle_reg_to_21 = function(vehicle_registrations,lsoa_11_21_tools, lookup_dz_2011_22, mode = "vehicle_registrations"){

  if(mode == "vehicle_registrations"){
    nms = c("Cars_Company_Licensed","Cars_Private_Licensed",
            "Other body types_Company_Licensed","Other body types_Private_Licensed",
            "Motorcycles_Company_Licensed","Motorcycles_Private_Licensed",
            "Cars_Company_SORN","Cars_Private_SORN","Other body types_Company_SORN",
            "Other body types_Private_SORN","Motorcycles_Company_SORN",
            "Motorcycles_Private_SORN","Cars_Disposal_Licensed",
            "Motorcycles_Disposal_Licensed","Other body types_Disposal_Licensed",
            "Cars_Disposal_SORN","Motorcycles_Disposal_SORN",
            "Other body types_Disposal_SORN")
  } else if (mode == "ulev_registrations") {
    nms = c("Plug-in hybrid electric (petrol)_Company",
            "Battery electric_Private","Plug-in hybrid electric (petrol)_Private","Battery electric_Company",
            "Range extended electric_Private","Range extended electric_Company","Hybrid electric (petrol)_Private",
            "Hybrid electric (petrol)_Company","Plug-in hybrid electric (diesel)_Company","Plug-in hybrid electric (diesel)_Private",
            "Diesel_Private",
            "Diesel_Company","Hybrid electric (diesel)_Company","Hybrid electric (diesel)_Private",
            "Fuel cell electric_Company","Fuel cell electric_Private",
            "Petrol_Company","Petrol_Private",
            "Battery electric_Disposal","Fuel cell electric_Disposal","Hybrid electric (petrol)_Disposal",
            "Plug-in hybrid electric (diesel)_Disposal","Plug-in hybrid electric (petrol)_Disposal","Range extended electric_Disposal",
            "Diesel_Disposal","Hybrid electric (diesel)_Disposal")
  } else if (mode == "ev_registrations") {
    nms = c("Battery electric_Company",
            "Plug-in hybrid electric (petrol)_Company","Battery electric_Private","Plug-in hybrid electric (petrol)_Private",
            "Range extended electric_Private","Range extended electric_Company","Plug-in hybrid electric (diesel)_Company",
            "Plug-in hybrid electric (diesel)_Private","Battery electric_Disposal","Plug-in hybrid electric (diesel)_Disposal",
            "Plug-in hybrid electric (petrol)_Disposal","Range extended electric_Disposal")
  } else {
    stop("Unknown mode")
  }

  # Scotlamd
  vehicle_registrations_Scot = vehicle_registrations[vehicle_registrations$LSOA11CD %in% lookup_dz_2011_22$LSOA11CD,]

  vehicle_registrations_Scot = dplyr::left_join(vehicle_registrations_Scot,
                                       lookup_dz_2011_22,
                                       by = c("LSOA11CD"),
                                       relationship = "many-to-many")

  vehicle_registrations_Scot$year = as.integer(gsub(" Q1","",vehicle_registrations_Scot$quarter))
  vehicle_registrations_Scot$quarter = NULL

  vehicle_registrations_Scot <- vehicle_registrations_Scot |>
    dplyr::group_by(year, LSOA21CD) |>
    dplyr::summarise(
      dplyr::across(
        .cols = where(is.numeric) & !matches("splitshare"),
        .fns = ~ round(sum(.x * splitshare, na.rm = TRUE)),
        .names = "{.col}"
      )
    ) |>
    dplyr::ungroup()

  # England and Wales

  # vehicle_registrations 2010 - 2023 (2011 bounds)
  # car_emissions_perkm 2001 - 2018 (converted to 2021 bounds)
  # population 2002 - 2022 (converted to 2021 bounds)

  # Step 1: Convert vehicle_registrations  to 2021

  vehicle_registrations_S = vehicle_registrations[vehicle_registrations$LSOA11CD %in% lsoa_11_21_tools$lookup_split$LSOA11CD,]
  vehicle_registrations_M = vehicle_registrations[vehicle_registrations$LSOA11CD %in% lsoa_11_21_tools$lookup_merge$LSOA11CD,]
  vehicle_registrations_U = vehicle_registrations[vehicle_registrations$LSOA11CD %in% lsoa_11_21_tools$lookup_unchanged$LSOA11CD,]

  #TODO: Fix for old Scotland 2011 Bounds
  vehicle_registrations_scot = vehicle_registrations[substr(vehicle_registrations$LSOA11CD,1,1) == "S",]
  vehicle_registrations_scot$LSOA21CD = vehicle_registrations_scot$LSOA11CD

  #Unchanged
  vehicle_registrations_U = dplyr::left_join(vehicle_registrations_U, lsoa_11_21_tools$lookup_unchanged, by = "LSOA11CD")
  vehicle_registrations_U = rbind(vehicle_registrations_U, vehicle_registrations_scot)

  # Merge
  vehicle_registrations_M = dplyr::left_join(vehicle_registrations_M, lsoa_11_21_tools$lookup_merge, by = "LSOA11CD")
  vehicle_registrations_M = dplyr::select(vehicle_registrations_M, -LSOA11CD)
  vehicle_registrations_M = dplyr::group_by(vehicle_registrations_M, quarter, LSOA21CD)
  vehicle_registrations_M = dplyr::summarise_all(vehicle_registrations_M, sum, na.rm = TRUE)
  vehicle_registrations_M = dplyr::ungroup(vehicle_registrations_M)

  vehicle_registrations_S$year = as.integer(gsub(" Q1","",vehicle_registrations_S$quarter))
  vehicle_registrations_M$year = as.integer(gsub(" Q1","",vehicle_registrations_M$quarter))
  vehicle_registrations_U$year = as.integer(gsub(" Q1","",vehicle_registrations_U$quarter))

  #Split
  lookup_split = lsoa_11_21_tools$lookup_split
  lookup_split = lookup_split[,c("LSOA11CD","LSOA21CD","year","household_ratio")]
  lookup_split = lookup_split[lookup_split$year %in% unique(vehicle_registrations_S$year),]
  vehicle_registrations_S = dplyr::left_join(lookup_split, vehicle_registrations_S,
                                    by = c("LSOA11CD", "year"),
                                    relationship = "many-to-many")
  vehicle_registrations_S = as.data.frame(vehicle_registrations_S)



  for(i in nms){
    vehicle_registrations_S[i] = vehicle_registrations_S[i] * vehicle_registrations_S$household_ratio
  }

  nms = c("LSOA21CD","year",nms)

  vehicle_registrations_S = vehicle_registrations_S[,nms]
  vehicle_registrations_M = vehicle_registrations_M[,nms]
  vehicle_registrations_U = vehicle_registrations_U[,nms]
  vehicle_registrations_Scot = vehicle_registrations_Scot[,nms]

  vehicle_registrations = rbind(vehicle_registrations_S, vehicle_registrations_M, vehicle_registrations_U, vehicle_registrations_Scot)


  vehicle_registrations

}


car_km_2009_2011_to_2021 = function(car_km_2009_2011, lsoa_11_21_tools){

  ckm_S = car_km_2009_2011[car_km_2009_2011$LSOA11 %in% lsoa_11_21_tools$lookup_split$LSOA11CD,]
  ckm_M = car_km_2009_2011[car_km_2009_2011$LSOA11 %in% lsoa_11_21_tools$lookup_merge$LSOA11CD,]
  ckm_U = car_km_2009_2011[car_km_2009_2011$LSOA11 %in% lsoa_11_21_tools$lookup_unchanged$LSOA11CD,]

  #TODO: Fix for old Scotland 2011 Bounds
  ckm_scot = car_km_2009_2011[substr(car_km_2009_2011$LSOA11,1,1) == "S",]
  ckm_scot$LSOA21CD = ckm_scot$LSOA11

  #Unchanged
  ckm_U = dplyr::left_join(ckm_U, lsoa_11_21_tools$lookup_unchanged, by = c("LSOA11" = "LSOA11CD"))
  ckm_U = rbind(ckm_U, ckm_scot)

  # Merge
  ckm_M = dplyr::left_join(ckm_M, lsoa_11_21_tools$lookup_merge, by = c("LSOA11" = "LSOA11CD"))
  ckm_M = dplyr::select(ckm_M, -LSOA11)
  ckm_M = dplyr::group_by(ckm_M, LSOA21CD)
  ckm_M = dplyr::summarise_all(ckm_M, sum, na.rm = TRUE)
  ckm_M = dplyr::ungroup(ckm_M)

  #Split
  lookup_split = lsoa_11_21_tools$lookup_split
  lookup_split = lookup_split[,c("LSOA11CD","LSOA21CD","year","household_ratio")]
  lookup_split = lookup_split[lookup_split$year %in% 2009:2011,]

  names(ckm_S) = gsub("vans_total","vanstotal",names(ckm_S))
  names(ckm_S) = gsub("cars_total","carstotal",names(ckm_S))
  names(ckm_S) = gsub("cars_total","carstotal",names(ckm_S))
  names(ckm_S) = gsub("car_km","carkm",names(ckm_S))
  names(ckm_S) = gsub("van_km","vankm",names(ckm_S))

  ckm_S = tidyr::pivot_longer(ckm_S,
                            cols = c("carstotal_09","carstotal_10","carstotal_11",
                                     "vanstotal_09","vanstotal_10","vanstotal_11",
                                     "carkm_09","carkm_10","carkm_11","vankm_09",
                                     "vankm_10","vankm_11"),
                            names_to = c(".value","year"),
                            names_sep = "_"
                            )
  ckm_S$year = as.integer(paste0("20",ckm_S$year))

  ckm_S = dplyr::left_join(lookup_split,
                           ckm_S,
                             by = c("LSOA11CD" = "LSOA11", "year"),
                             relationship = "many-to-many")
  ckm_S = as.data.frame(ckm_S)

  nms = c("carstotal","vanstotal","carkm","vankm")

  for(i in nms){
    ckm_S[i] = ckm_S[i] * ckm_S$household_ratio
  }

  ckm_S$year = substr(as.character(ckm_S$year),3,4)
  ckm_S = ckm_S[,c("LSOA21CD","year","carstotal","vanstotal","carkm","vankm")]

  ckm_S = tidyr::pivot_wider(ckm_S, id_cols = "LSOA21CD", names_sep = "_",
                             values_from = c("carstotal","vanstotal","carkm","vankm"),
                             names_from = "year")

  names(ckm_S) = gsub("vanstotal","vans_total",names(ckm_S))
  names(ckm_S) = gsub("carstotal","cars_total",names(ckm_S))
  names(ckm_S) = gsub("carstotal","cars_total",names(ckm_S))
  names(ckm_S) = gsub("carkm","car_km",names(ckm_S))
  names(ckm_S) = gsub("vankm","van_km",names(ckm_S))

  ckm_M = ckm_M[,names(ckm_S)]
  ckm_U = ckm_U[,names(ckm_S)]

  ckm = rbind(ckm_S, ckm_M, ckm_U)


  ckm


}
