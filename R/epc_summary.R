
epc_summarise_domestic = function(path = file.path(parameters$path_data,"epc/GB_domestic_epc.Rds"),
                                  bounds_lsoa_GB_full
                                  ){
  certs <- readRDS(path)
  certs <- sf::st_transform(certs, 27700)

  certs <- sf::st_join(certs, bounds_lsoa_GB_full)
  certs <- sf::st_drop_geometry(certs)

  certs$b_type = as.character(certs$b_type)
  certs$b_type[is.na(certs$b_type)] = ""
  certs$buidling_type <- paste0(certs$b_type," ",certs$p_type)

  certs$fuel <- gsub("\\(not community\\)","",certs$fuel)
  certs$fuel <- gsub("\\(unknown\\)","",certs$fuel)
  certs$fuel <- gsub("\\(community\\)","",certs$fuel)

  certs$fuel <- trimws(certs$fuel)

  certs$g_type <- as.character(certs$g_type)
  certs$g_type <- gsub(", known data"," glazing",certs$g_type)
  certs$g_type <- gsub(" installed before 2002","",certs$g_type)
  certs$g_type <- gsub(" installed during or after 2002","",certs$g_type)
  certs$g_type <- gsub(", unknown install date","",certs$g_type)
  certs$g_type[is.na(certs$g_type)] <- "not defined"



    # Flag Roofs and Floors
  certs$floor_ee <- as.character(certs$floor_ee)
  certs$floor_ee <- ifelse(is.na(certs$floor_ee) &
                                     (certs$floor_d %in% c("(another dwelling below)", "(other premises below)")),
                                   "dwelling below",certs$floor_ee)


  certs$roof_ee <- as.character(certs$roof_ee)
  certs$roof_ee <- ifelse(is.na(certs$roof_ee) &
                                    (certs$roof_d %in% c("(another dwelling above)", "(other premises above)")),
                                  "dwelling above",certs$roof_ee)


  certs$sol_wat[is.na(certs$sol_wat)] = "no"

  certs$light_ee <- as.character(certs$light_ee)
  certs$wind_ee <- as.character(certs$wind_ee)
  certs$water_ee <- as.character(certs$water_ee)
  certs$cur_rate <- as.character(certs$cur_rate)
  certs$heat_ee <- as.character(certs$heat_ee)
  certs$con_ee  <- as.character(certs$con_ee )
  certs$wall_ee <- as.character(certs$wall_ee)

  certs$tenure[is.na(certs$tenure)] = "unknown"
  certs$roof_ee[is.na(certs$roof_ee)] = "unknown"
  certs$floor_ee[is.na(certs$floor_ee)] = "unknown"
  certs$light_ee[is.na(certs$light_ee)] = "unknown"
  certs$wind_ee[is.na(certs$wind_ee)] = "unknown"
  certs$wall_ee[is.na(certs$wall_ee)] = "unknown"
  certs$water_ee[is.na(certs$water_ee)] = "unknown"
  certs$heat_ee[is.na(certs$heat_ee)] = "unknown"
  certs$con_ee[is.na(certs$con_ee)] = "unknown"

  certs$con_d[is.na(certs$con_d)] = "unknown"
  certs$floor_d[is.na(certs$floor_d)] = "unknown"
  certs$roof_d[is.na(certs$roof_d)] = "unknown"
  certs$wall_d[is.na(certs$wall_d)] = "unknown"
  certs$heat_d[is.na(certs$heat_d)] = "unknown"
  certs$water_d[is.na(certs$water_d)] = "unknown"
  certs$fuel[is.na(certs$fuel)] = "unknown"
  certs$cur_rate[is.na(certs$cur_rate)] = "unknown"
  certs$buidling_type[is.na(certs$buidling_type)] = "unknown"
  certs$age[is.na(certs$age)] = "unknown"

  certs = certs[!is.na(certs$LSOA21CD),]

  cert_summ <- dplyr::group_by(certs, LSOA21CD)
  cert_summ <- dplyr::summarise(cert_summ,
              epc_total = dplyr::n(),
              epc_A = length(cur_rate[cur_rate == "A"]),
              epc_B = length(cur_rate[cur_rate == "B"]),
              epc_C = length(cur_rate[cur_rate == "C"]),
              epc_D = length(cur_rate[cur_rate == "D"]),
              epc_E = length(cur_rate[cur_rate == "E"]),
              epc_F = length(cur_rate[cur_rate == "F"]),
              epc_G = length(cur_rate[cur_rate == "G"]),
              epc_other = length(cur_rate[cur_rate == "unknown"]),
              epc_score_avg = mean(cur_ee, na.rm = TRUE),

              type_house_semi = length(buidling_type[buidling_type == "Semi-Detached House"]),
              type_house_midterrace = length(buidling_type[buidling_type %in% c("Enclosed Mid-Terrace House", "Mid-Terrace House")]),
              type_house_endterrace = length(buidling_type[buidling_type %in% c("Enclosed End-Terrace House", "End-Terrace House")]),
              type_house_detached = length(buidling_type[buidling_type == "Detached House"]),
              type_flat = length(buidling_type[grepl("Flat",buidling_type)]),
              type_bungalow_semi = length(buidling_type[buidling_type == "Semi-Detached Bungalow"]),
              type_bungalow_midterrace = length(buidling_type[buidling_type %in% c("Enclosed Mid-Terrace Bungalow", "Mid-Terrace Bungalow")]),
              type_bungalow_endterrace = length(buidling_type[buidling_type %in% c("Enclosed End-Terrace Bungalow", "End-Terrace Bungalow")]),
              type_bungalow_detached = length(buidling_type[buidling_type == "Detached Bungalow"]),
              type_maisonette = length(buidling_type[grepl("Maisonette",buidling_type)]),
              type_parkhome = length(buidling_type[grepl("Park home",buidling_type)]),

              tenure_owner = length(tenure[tenure == "owner-occupied"]),
              tenure_privaterent = length(tenure[tenure == "rented (private)"]),
              tenure_socialrent = length(tenure[tenure == "rented (social)"]),
              tenure_unknown = length(tenure[tenure == "unknown"]),

              age_pre1900 = length(age[age == "before 1900"]),
              age_19001929 = length(age[age == "1900-1929"]),
              age_19301949 = length(age[age == "1930-1949"]),
              age_19501966 = length(age[age == "1950-1966"]),
              age_19671975 = length(age[age == "1967-1975"]),
              age_19761982 = length(age[age == "1976-1982"]),
              age_19831990 = length(age[age == "1983-1990"]),
              age_19911995 = length(age[age == "1991-1995"]),
              age_19962002 = length(age[age == "1996-2002"]),
              age_20032006 = length(age[age == "2003-2006"]),
              age_20072011 = length(age[age == "2007-2011"]),
              age_20122021 = length(age[age %in% c("2012 onwards","2012-2021")]),
              age_post2022 = length(age[age == "2022 onwards"]),
              age_unknown = length(age[age == "unknown"]),

              floor_area_avg = round(mean(area, na.rm = TRUE)),

              floor_verygood = length(floor_ee[floor_ee == "Very Good"]),
              floor_good = length(floor_ee[floor_ee == "Good"]),
              floor_average = length(floor_ee[floor_ee == "Average"]),
              floor_poor = length(floor_ee[floor_ee == "Poor"]),
              floor_verypoor = length(floor_ee[floor_ee == "Very Poor"]),
              floor_below = length(floor_ee[floor_ee == "dwelling below"]),
              floor_other = length(floor_ee[!floor_ee %in% c("Very Good","Good","Average","Poor","Very Poor","dwelling below")]),

              floord_soliduninsulated = length(floor_d[grepl("solid, no insulation",floor_d)]),
              floord_solidinsulated = length(floor_d[grepl("solid, insulated ",floor_d)]),
              floord_solidlimitedinsulated = length(floor_d[grepl("solid, limited insulation",floor_d)]),
              floord_suspendeduninsulated = length(floor_d[grepl("suspended, no insulation",floor_d)]),
              floord_suspendedinsualted = length(floor_d[grepl("solid, insulated ",floor_d)]),
              floord_suspendedlimitedinsulated = length(floor_d[grepl("suspended, limited insulation",floor_d)]),
              floord_below = length(floor_d[floor_d == "(another dwelling below)" | floor_d == "(other premises below)"]),

              window_verygood = length(wind_ee[wind_ee == "Very Good"]),
              window_good = length(wind_ee[wind_ee == "Good"]),
              window_average = length(wind_ee[wind_ee == "Average"]),
              window_poor = length(wind_ee[wind_ee == "Poor"]),
              window_verypoor = length(wind_ee[wind_ee == "Very Poor"]),
              window_other = length(wind_ee[!wind_ee %in% c("Very Good","Good","Average","Poor","Very Poor")]),

              water_verygood = length(water_ee[water_ee == "Very Good"]),
              water_good = length(water_ee[water_ee == "Good"]),
              water_average = length(water_ee[water_ee == "Average"]),
              water_poor = length(water_ee[water_ee == "Poor"]),
              water_verypoor = length(water_ee[water_ee == "Very Poor"]),
              water_other = length(water_ee[!water_ee %in% c("Very Good","Good","Average","Poor","Very Poor")]),

              waterd_mainsystem = length(water_d[grepl("from main system",water_d)]),
              waterd_immersion = length(water_d[grepl("electric immersion",water_d)]),
              waterd_community = length(water_d[grepl("community scheme",water_d)]),
              waterd_instantaneous = length(water_d[grepl("electric instantaneous",water_d)]),
              waterd_gasmultipoint = length(water_d[grepl("gas multipoint",water_d)]),

              glazing_single = length(g_type[g_type == "single glazing"]),
              glazing_double = length(g_type[g_type == "double glazing"]),
              glazing_triple = length(g_type[g_type == "triple glazing"]),
              glazing_secondary = length(g_type[g_type == "secondary glazing"]),
              glazing_unknown = length(g_type[g_type == "not defined"]),

              wall_verygood = length(wall_ee[wall_ee == "Very Good"]),
              wall_good = length(wall_ee[wall_ee == "Good"]),
              wall_average = length(wall_ee[wall_ee == "Average"]),
              wall_poor = length(wall_ee[wall_ee == "Poor"]),
              wall_verypoor = length(wall_ee[wall_ee == "Very Poor"]),
              wall_other = length(wall_ee[!wall_ee %in% c("Very Good","Good","Average","Poor","Very Poor")]),

              walld_cavity = length(wall_d[grepl("cavity wall",wall_d)]),
              walld_solid = length(wall_d[grepl("solid brick,",wall_d)]),
              walld_timber = length(wall_d[grepl("timber frame",wall_d)]),
              walld_sandlimestone = length(wall_d[grepl("sandstone or limestone",wall_d)]),
              walld_granitewhinstine = length(wall_d[grepl("granite or whinstone",wall_d)]),
              walld_system = length(wall_d[grepl("system built",wall_d)]),

              roof_verygood = length(roof_ee[roof_ee == "Very Good"]),
              roof_good = length(roof_ee[roof_ee == "Good"]),
              roof_average = length(roof_ee[roof_ee == "Average"]),
              roof_poor = length(roof_ee[roof_ee == "Poor"]),
              roof_verypoor = length(roof_ee[roof_ee == "Very Poor"]),
              roof_above = length(roof_ee[roof_ee == "dwelling above"]),
              roof_other = length(roof_ee[!roof_ee %in% c("Very Good","Good","Average","Poor","Very Poor","dwelling above")]),

              roofd_pitched = length(roof_d[grepl("pitched",roof_d)]),
              roofd_flat = length(roof_d[grepl("flat",roof_d)]),
              roofd_room = length(roof_d[grepl("roof room(s)",roof_d)]),
              roofd_thatched = length(roof_d[grepl("thatched",roof_d)]),
              roofd_above = length(roof_d[roof_d == "(another dwelling above)" | roof_d == "(other premises above)"]),

              mainheat_verygood = length(heat_ee[heat_ee == "Very Good"]),
              mainheat_good = length(heat_ee[heat_ee == "Good"]),
              mainheat_average = length(heat_ee[heat_ee == "Average"]),
              mainheat_poor = length(heat_ee[heat_ee == "Poor"]),
              mainheat_verypoor = length(heat_ee[heat_ee == "Very Poor"]),
              mainheat_other = length(heat_ee[!heat_ee %in% c("Very Good","Good","Average","Poor","Very Poor")]),

              mainheatdesc_gasboiler = length(heat_d[heat_d %in% c("boiler, underfloor heating, mains gas", "boiler, radiators, mains gas") ]),
              mainheatdesc_oilboiler = length(heat_d[heat_d %in% c("boiler, underfloor heating, oil","boiler, radiators, oil")]),
              mainheatdesc_storageheater = length(heat_d[grepl("electric storage heaters",heat_d)]),
              mainheatdesc_portableheater = length(heat_d[grepl("electric heaters",heat_d)]),
              mainheatdesc_roomheater = length(heat_d[grepl("room heaters",heat_d)]),
              mainheatdesc_heatpump = length(heat_d[grepl("heat pump",heat_d)]),
              mainheatdesc_community = length(heat_d[grepl("community",heat_d)]),

              mainfuel_mainsgas = length(fuel[fuel == "mains gas"]),
              mainfuel_electric = length(fuel[fuel == "electricity"]),
              mainfuel_oil = length(fuel[fuel == "oil" | grepl("b30",fuel)] ),
              mainfuel_coal = length(fuel[grepl("coal",fuel) | grepl("anthracite",fuel)]),
              mainfuel_lpg = length(fuel[grepl("lpg",fuel)]),
              mainfuel_biomass = length(fuel[grepl("wood",fuel) | grepl("biomass",fuel)]),
              mainfuel_dualfuel = length(fuel[grepl("dual fuel",fuel)]),

              mainheatcontrol_verygood = length(con_ee[con_ee == "Very Good"]),
              mainheatcontrol_good = length(con_ee[con_ee == "Good"]),
              mainheatcontrol_average = length(con_ee[con_ee == "Average"]),
              mainheatcontrol_poor = length(con_ee[con_ee == "Poor"]),
              mainheatcontrol_verypoor = length(con_ee[con_ee == "Very Poor"]),
              mainheatcontrol_other = length(con_ee[!con_ee %in% c("Very Good","Good","Average","Poor","Very Poor")]),

              controld_progthermtrvs = length(con_d[con_d == "programmer, room thermostat and trvs"]),
              controld_progtherm = length(con_d[con_d == "programmer and room thermostat"]),
              controld_progtrvsbypass = length(con_d[con_d == "programmer, trvs and bypass"]),
              controld_pzones = length(con_d[con_d == "time and temperature zone control"]),

              light_verygood = length(light_ee[light_ee == "Very Good"]),
              light_good = length(light_ee[light_ee == "Good"]),
              light_average = length(light_ee[light_ee == "Average"]),
              light_poor = length(light_ee[light_ee == "Poor"]),
              light_verypoor = length(light_ee[light_ee == "Very Poor"]),
              light_other = length(light_ee[!light_ee %in% c("Very Good","Good","Average","Poor","Very Poor")]),

              solarpv_yes = length(pv[pv == "yes"]),
              solarpv_no = length(pv[pv == "no"]),
              solarthermal_yes = length(sol_wat[sol_wat == "yes"]),
              solarthermal_no = length(sol_wat[sol_wat == "no"]),

    )

  cert_summ$type_other <- cert_summ$epc_total - rowSums(cert_summ[,grepl("type_",names(cert_summ))])
  cert_summ$mainheatdesc_other <- cert_summ$epc_total - rowSums(cert_summ[,grepl("mainheatdesc_",names(cert_summ))])
  cert_summ$floord_other <- cert_summ$epc_total - rowSums(cert_summ[,grepl("floord_",names(cert_summ))])
  cert_summ$waterd_other <- cert_summ$epc_total - rowSums(cert_summ[,grepl("waterd_",names(cert_summ))])
  cert_summ$walld_other <- cert_summ$epc_total - rowSums(cert_summ[,grepl("walld_",names(cert_summ))])
  cert_summ$roofd_other <- cert_summ$epc_total - rowSums(cert_summ[,grepl("roofd_",names(cert_summ))])
  cert_summ$controld_other <- cert_summ$epc_total - rowSums(cert_summ[,grepl("controld_",names(cert_summ))])



  cert_summ


}
