load_flights_od = function(path = "../../creds2/LDT/data/clean/od_emissions_2021.gpkg") {
  pass_od <- sf::read_sf(path)
  pass_od
}

load_flights_airports = function(path = "../../creds2/LDT/data/clean/airports_clean_second_pass_2021.gpkg", bounds_la) {
  airports <- sf::read_sf(path)

  # Add home nations to airports
  bounds_la$country_uk = substr(bounds_la$LAD24CD, 1, 1)
  bounds_la = dplyr::group_by(bounds_la, country_uk)
  bounds_la = dplyr::summarise(bounds_la)
  bounds_la = sf::st_transform(bounds_la, 4326)
  airports = sf::st_join(airports, bounds_la)
  airports$country_uk[is.na(airports$country_uk)] = "Other Country"


  airports

}

# Get annual emissions for each to the home nations
# Note some flight emissions are allocated outside the UK
get_flights_total_emissions = function(flights_od, flights_airports) {

  flights_airports = sf::st_drop_geometry(flights_airports)
  flights_airports = flights_airports[!duplicated(flights_airports),]
  flights_od = sf::st_drop_geometry(flights_od)

  names(flights_airports)[3] = "fromclass"
  flights_od = dplyr::left_join(flights_od, flights_airports,
                                by = c("airport1" = "airport",
                                       "airport1_country" = "country"))
  names(flights_airports)[3] = "toclass"
  flights_od = dplyr::left_join(flights_od, flights_airports,
                                by = c("airport2" = "airport",
                                       "airport2_country" = "country"))

  #TODO: Update to 2023
  flights_summary = flights_od[,c("fromclass","toclass",paste0("emissions_",1990:2021))]
  flights_summary = dplyr::group_by(flights_summary, fromclass, toclass)
  flights_summary = dplyr::summarise_all(flights_summary, sum, na.rm = TRUE)
  flights_summary = dplyr::ungroup(flights_summary)

  flights_summary = flights_summary[!(flights_summary$fromclass == "Other Country" &
                                      flights_summary$toclass == "Other Country"),]

  # Split International and Domestic
  summary_dom = flights_summary[!(flights_summary$fromclass == "Other Country" |
                                    flights_summary$toclass == "Other Country"), ]
  summary_int = flights_summary[(flights_summary$fromclass == "Other Country" |
                                    flights_summary$toclass == "Other Country"), ]

  # Split Domestic emissions 50:50 between nations
  part_sum = function(x, frac = 0.5){
    sum(x, na.rm = TRUE) * frac
  }

  dom_from = dplyr::select(summary_dom, -toclass)
  dom_from = dplyr::group_by(dom_from, fromclass)
  dom_from = dplyr::summarise_all(dom_from, part_sum)

  dom_to = dplyr::select(summary_dom, -fromclass)
  dom_to = dplyr::group_by(dom_to, toclass)
  dom_to = dplyr::summarise_all(dom_to, part_sum)

  names(dom_from)[1] = "country_uk"
  names(dom_to)[1] = "country_uk"
  emissions_dom = rbind(dom_from, dom_to)
  emissions_dom = dplyr::group_by(emissions_dom, country_uk)
  emissions_dom = dplyr::summarise_all(emissions_dom, sum, na.rm = TRUE)
  emissions_dom = dplyr::ungroup(emissions_dom)

  # International Emissions
  #34% of passengers are foreign residents (2016)
  #TODO: Get time series of British vs foreign residents
  for(i in 1:nrow(summary_int)){
    if(summary_int$fromclass[i] == "Other Country"){
      summary_int$fromclass[i] = summary_int$toclass[i]
    }
  }

  summary_int = dplyr::select(summary_int, -toclass)
  summary_int = dplyr::group_by(summary_int, fromclass)
  summary_int = dplyr::summarise_all(summary_int, part_sum, frac = 0.64)

  names(summary_int)[1] = "country_uk"
  summary_all = rbind(summary_int, emissions_dom)
  summary_all = dplyr::group_by(summary_all, country_uk)
  summary_all = dplyr::summarise_all(summary_all, sum, na.rm = TRUE)

  # Check 99.98219% of emissions (some channel island to Europe)
  # for(i in 1990:2021){
  #   print(sum(summary_all[paste0("emissions_",i)]) / sum(flights_od[paste0("emissions_",i)], na.rm = TRUE))
  # }

  summary_all

}

get_flights_lsoa_emissions = function(flights_total_emissions, income_lsoa, population){

  #TODO: Get Scotland Income
  #TODO: Change income shares over time

  # Define Curve
  x = seq(0,1,0.01)
  y = 1.02 * x **2 - 0.02 *x
  y[y<0] <- 0
  z = c(y[1],diff(y))

  # Split by country
  income_lsoa$county = substr(income_lsoa$LSOA21CD,1,1)

  income_lsoa_EW = income_lsoa[income_lsoa$county %in% c("E","W"), ]
  income_lsoa_S = population[,c("year","LSOA21CD")]
  income_lsoa_S = income_lsoa_S[income_lsoa_S$year == 2021,]
  income_lsoa_S$county = substr(income_lsoa_S$LSOA21CD,1,1)
  income_lsoa_S = income_lsoa_S[income_lsoa_S$county == "S", ]
  income_lsoa_S$year = NULL

  income_lsoa_EW$centile <- percentile(income_lsoa_EW$income_lsoa) / 100
  #income_lsoa_S$centile <- percentile(income_lsoa_S$income_lsoa) / 100

  match_table <- data.frame(x, y, z)
  match_table$x <- round(match_table$x, 2)

  income_lsoa_EW$emissions_share <- match_table$z[match(income_lsoa_EW$centile, match_table$x)]
  income_lsoa_EW$emissions_share <- income_lsoa_EW$emissions_share / sum(income_lsoa_EW$emissions_share, na.rm = TRUE)

  flights_total_emissions = as.data.frame(flights_total_emissions)

  for(i in 2002:2021){
    income_lsoa_EW[paste0("emissions_flights_",i)] <- income_lsoa_EW$emissions_share *
      sum(flights_total_emissions[flights_total_emissions$country_uk %in% c("E","W"),paste0("emissions_",i)], na.rm = TRUE)
  }

  # sum(income_lsoa_EW$emissions_flights_2021) /
  # sum(flights_total_emissions[flights_total_emissions$country_uk %in% c("E","W"),paste0("emissions_",2021)], na.rm = TRUE)

  for(i in 2002:2021){
    income_lsoa_S[paste0("emissions_flights_",i)] <- flights_total_emissions[flights_total_emissions$country_uk == "S",paste0("emissions_",i)] / nrow(income_lsoa_S)
  }

  income_lsoa_EW$centile = NULL
  income_lsoa_EW$emissions_share = NULL

  income_lsoa_EW$income_lsoa = NULL



  population = population[,c("year","LSOA21CD","all_ages")]
  population = tidyr::pivot_wider(population, names_from = "year",
                                  values_from = "all_ages", id_cols = "LSOA21CD")

  emissions_lsoa = rbind(income_lsoa_EW, income_lsoa_S)

  population = population[order(population$LSOA21CD),]
  emissions_lsoa = emissions_lsoa[order(emissions_lsoa$LSOA21CD),]

  if(!all(population$LSOA21CD == emissions_lsoa$LSOA21CD)){
    stop("LSOA21CD don't match")
  }

  for(i in 2002:2021){
    emissions_lsoa[paste0("emissions_flights_percap_",i)] <- emissions_lsoa[paste0("emissions_flights_",i)] / population[as.character(i)]
  }

  # Few DataZones with 0 population in 2021
  emissions_lsoa[,paste0("emissions_flights_percap_",2002:2021)] = lapply(emissions_lsoa[,paste0("emissions_flights_percap_",2002:2021)],
                                                                          function(x){
                                                                            x[is.infinite(x)] = 0
                                                                            x
                                                                          })
  emissions_lsoa
}



percentile <- function(dat){
  pt1 <- quantile(dat, probs = seq(0, 1, by = 0.01), type = 7, na.rm = TRUE)
  pt2 <- unique(as.data.frame(pt1), fromLast = TRUE)
  pt3 <- rownames(pt2)
  pt4 <- as.integer(strsplit(pt3, "%"))
  if(0 %in% pt2$pt1){
    cts <- c(-0.000001, pt2$pt1)
  } else {
    cts <- c(0, pt2$pt1)
  }
  datp <- pt4[as.integer(cut(dat, cts, labels = 1:length(pt3)))]
  return(datp)
  # foo = data.frame(data = dat,
  #                  datp = datp)
}
