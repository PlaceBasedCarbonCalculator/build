select_retofit_vars = function(epc_dom_summary, population) {
  sub = epc_dom_summary[,c("LSOA21CD","epc_total","epc_score_avg","floor_area_avg")]
  sub$modal_age = modal(epc_dom_summary[,c("age_pre1900",
                                           "age_19001929","age_19301949","age_19501966",
                                           "age_19671975","age_19761982","age_19831990",
                                           "age_19911995","age_19962002","age_20032006",
                                           "age_post2012")])
  sub$modal_wall = modal(epc_dom_summary[,c("wall_verygood","wall_good",
                                            "wall_average","wall_poor","wall_verypoor","wall_other")])
  sub$modal_roof = modal(epc_dom_summary[,c("roof_verygood","roof_good",
                                            "roof_average","roof_poor","roof_verypoor","roof_above",
                                            "roof_other")])
  sub$modal_heat = modal(epc_dom_summary[,c("mainheat_verygood","mainheat_good",
                                            "mainheat_average","mainheat_poor","mainheat_verypoor","mainheat_other")])
  sub$modal_window = modal(epc_dom_summary[,c("window_verygood","window_good","window_average","window_poor",
                                              "window_verypoor","window_other")])


  sub$modal_mainheat = modal(epc_dom_summary[,c("mainheatdesc_gasboiler","mainheatdesc_oilboiler","mainheatdesc_storageheater","mainheatdesc_portableheater",
                                            "mainheatdesc_roomheater","mainheatdesc_heatpump","mainheatdesc_community")])
  sub$modal_mainfuel = modal(epc_dom_summary[,c("mainfuel_mainsgas",
                                                "mainfuel_electric","mainfuel_oil","mainfuel_coal","mainfuel_lpg",
                                                "mainfuel_biomass","mainfuel_dualfuel")])
  sub$modal_floord = modal(epc_dom_summary[,c("floord_soliduninsulated","floord_solidinsulated","floord_solidlimitedinsulated",
                                              "floord_suspendeduninsulated","floord_suspendedinsualted","floord_suspendedlimitedinsulated","floord_below")])

  type = epc_dom_summary[,c("type_house_semi","type_house_midterrace","type_house_endterrace","type_house_detached","type_flat",
                            "type_bungalow_semi","type_bungalow_midterrace","type_bungalow_endterrace","type_bungalow_detached",
                            "type_maisonette","type_parkhome")]
  names(type) = gsub("type_","",names(type))

  sub$modal_type = modal(type, drop = FALSE)

  sub$modal_tenure = modal(epc_dom_summary[,c("tenure_owner","tenure_privaterent",
                                              "tenure_socialrent","tenure_unknown")])

  population = population[,c("LSOA21CD","year","households_est" , "all_properties")]
  population = population[population$year == 2022,]
  population$year = NULL

  #TODO: Scotland
  sub = dplyr::left_join(sub, population, by = "LSOA21CD")

  sub$percent_EPC = round(sub$epc_total / sub$all_properties * 100)
  sub$households_est = NULL
  sub$all_properties = NULL
  sub$epc_total = NULL

  sub$epc_score_avg = as.character(cut(sub$epc_score_avg, breaks = c(0,50,55,60,65,70,80,100)))
  sub$epc_score_avg = gsub("\\(|\\[|\\)|\\]","",sub$epc_score_avg)
  sub$epc_score_avg = gsub(",","-",sub$epc_score_avg)

  sub$floor_area_avg = as.character(cut(sub$floor_area_avg, breaks = c(0,40,60,80,100,120,140,160,500)))
  sub$floor_area_avg = gsub("\\(|\\[|\\)|\\]","",sub$floor_area_avg)
  sub$floor_area_avg = gsub(",","-",sub$floor_area_avg)

  sub$percent_EPC = as.character(cut(sub$percent_EPC, breaks = c(0,30,50,60,70,80,90,200)))
  sub$percent_EPC = gsub("\\(|\\[|\\)|\\]","",sub$percent_EPC)
  sub$percent_EPC = gsub(",","-",sub$percent_EPC)

  sub


}

modal = function(df, drop = TRUE){
  x = apply(df, 1, function(x) names(df)[x == max(x)][1])
  if(drop){
    x = strsplit(x,"_")
    x = sapply(x,`[[`,2)
  }
  x
}
