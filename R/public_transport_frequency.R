load_pt_frequency = function(path = parameters$path_data){

  zone_service = list()

  for(i in c(2004:2011,2014:2023)){
    sub = readRDS(file.path(path,"pt_frequency",paste0("trips_per_lsoa21_by_mode_",i,".Rds")))
    sub$year = i
    zone_service[[i]] = sub
  }

  zone_service = dplyr::bind_rows(zone_service)

  # Combine Weekdays
  zone_service$runs_weekday_Night <- (zone_service$runs_Mon_Night +
                                        zone_service$runs_Tue_Night +
                                        zone_service$runs_Wed_Night +
                                        zone_service$runs_Thu_Night +
                                        zone_service$runs_Fri_Night) / 5

  zone_service$runs_weekday_Morning_Peak <- (zone_service$`runs_Mon_Morning Peak` +
                                               zone_service$`runs_Tue_Morning Peak` +
                                               zone_service$`runs_Wed_Morning Peak` +
                                               zone_service$`runs_Thu_Morning Peak` +
                                               zone_service$`runs_Fri_Morning Peak`) / 5

  zone_service$runs_weekday_Afternoon_Peak <- (zone_service$`runs_Mon_Afternoon Peak` +
                                                 zone_service$`runs_Tue_Afternoon Peak` +
                                                 zone_service$`runs_Wed_Afternoon Peak` +
                                                 zone_service$`runs_Thu_Afternoon Peak` +
                                                 zone_service$`runs_Fri_Afternoon Peak`) / 5

  zone_service$runs_weekday_Midday <- (zone_service$runs_Mon_Midday +
                                         zone_service$runs_Tue_Midday +
                                         zone_service$runs_Wed_Midday +
                                         zone_service$runs_Thu_Midday +
                                         zone_service$runs_Fri_Midday) / 5

  zone_service$runs_weekday_Evening <- (zone_service$runs_Mon_Evening +
                                          zone_service$runs_Tue_Evening +
                                          zone_service$runs_Wed_Evening +
                                          zone_service$runs_Thu_Evening +
                                          zone_service$runs_Fri_Evening) / 5

  zone_service$runs_weekday_Night <- (zone_service$runs_Mon_Night +
                                        zone_service$runs_Tue_Night +
                                        zone_service$runs_Wed_Night +
                                        zone_service$runs_Thu_Night +
                                        zone_service$runs_Fri_Night) / 5


  zone_service$tph_weekday_Night <- (zone_service$tph_Mon_Night +
                                       zone_service$tph_Tue_Night +
                                       zone_service$tph_Wed_Night +
                                       zone_service$tph_Thu_Night +
                                       zone_service$tph_Fri_Night) / 5

  zone_service$tph_weekday_Morning_Peak <- (zone_service$`tph_Mon_Morning Peak` +
                                              zone_service$`tph_Tue_Morning Peak` +
                                              zone_service$`tph_Wed_Morning Peak` +
                                              zone_service$`tph_Thu_Morning Peak` +
                                              zone_service$`tph_Fri_Morning Peak`) / 5

  zone_service$tph_weekday_Afternoon_Peak <- (zone_service$`tph_Mon_Afternoon Peak` +
                                                zone_service$`tph_Tue_Afternoon Peak` +
                                                zone_service$`tph_Wed_Afternoon Peak` +
                                                zone_service$`tph_Thu_Afternoon Peak` +
                                                zone_service$`tph_Fri_Afternoon Peak`) / 5

  zone_service$tph_weekday_Midday <- (zone_service$tph_Mon_Midday +
                                        zone_service$tph_Tue_Midday +
                                        zone_service$tph_Wed_Midday +
                                        zone_service$tph_Thu_Midday +
                                        zone_service$tph_Fri_Midday) / 5

  zone_service$tph_weekday_Evening <- (zone_service$tph_Mon_Evening +
                                         zone_service$tph_Tue_Evening +
                                         zone_service$tph_Wed_Evening +
                                         zone_service$tph_Thu_Evening +
                                         zone_service$tph_Fri_Evening) / 5

  zone_service$tph_weekday_Night <- (zone_service$tph_Mon_Night +
                                       zone_service$tph_Tue_Night +
                                       zone_service$tph_Wed_Night +
                                       zone_service$tph_Thu_Night +
                                       zone_service$tph_Fri_Night) / 5

  zone_service <- zone_service[,c("zone_id",
                                  "route_type",
                                  "year",
                                  "routes_Morning Peak",
                                  "routes_Midday",
                                  "routes_Afternoon Peak",
                                  "routes_Evening",
                                  "routes_Night",
                                  "runs_weekday_Morning_Peak",
                                  "runs_weekday_Midday",
                                  "runs_weekday_Afternoon_Peak",
                                  "runs_weekday_Evening",
                                  "runs_weekday_Night",
                                  "runs_Sat_Morning Peak",
                                  "runs_Sat_Midday",
                                  "runs_Sat_Afternoon Peak",
                                  "runs_Sat_Evening",
                                  "runs_Sat_Night",
                                  "runs_Sun_Morning Peak",
                                  "runs_Sun_Midday",
                                  "runs_Sun_Afternoon Peak",
                                  "runs_Sun_Evening",
                                  "runs_Sun_Night",
                                  "tph_weekday_Morning_Peak",
                                  "tph_weekday_Midday",
                                  "tph_weekday_Afternoon_Peak",
                                  "tph_weekday_Evening",
                                  "tph_weekday_Night",
                                  "tph_Sat_Morning Peak",
                                  "tph_Sat_Midday",
                                  "tph_Sat_Afternoon Peak",
                                  "tph_Sat_Evening",
                                  "tph_Sat_Night",
                                  "tph_Sun_Morning Peak",
                                  "tph_Sun_Midday",
                                  "tph_Sun_Afternoon Peak",
                                  "tph_Sun_Evening",
                                  "tph_Sun_Night")]
  names(zone_service) <- gsub(" ","_",names(zone_service))

  zone_service$tph_daytime_avg =  (zone_service$tph_weekday_Morning_Peak * 5 * 4 +
                                     zone_service$tph_weekday_Midday * 5 * 5 +
                                     zone_service$tph_weekday_Afternoon_Peak * 5 * 3 +
                                     zone_service$tph_weekday_Evening * 5 * 4 +
                                     zone_service$tph_Sat_Morning_Peak * 4 +
                                     zone_service$tph_Sat_Midday * 5 +
                                     zone_service$tph_Sat_Afternoon_Peak  * 3 +
                                     zone_service$tph_Sat_Evening * 4 +
                                     zone_service$tph_Sun_Morning_Peak  * 4 +
                                     zone_service$tph_Sun_Midday * 5 +
                                     zone_service$tph_Sun_Afternoon_Peak * 3 +
                                     zone_service$tph_Sun_Evening * 4) / (7 * 16)

  wide = tidyr::pivot_wider(zone_service,
                     names_from = c("year","route_type"),
                     values_from = tph_weekday_Morning_Peak:tph_daytime_avg,
                     id_cols = c("zone_id")
  )
  wide = wide[,!grepl("_1100$",names(wide))]

  wide[2:ncol(wide)] <- lapply(wide[2:ncol(wide)], round, digits = 1)
  wide


}



select_transport_vars = function(pt_frequency){

  pt_frequency$change_bus_2008_2023 = pt_frequency$tph_daytime_avg_2023_3 - pt_frequency$tph_daytime_avg_2008_3

  pt_frequency = pt_frequency[,c("zone_id","change_bus_2008_2023","tph_daytime_avg_2023_0",
                                 "tph_daytime_avg_2023_1","tph_daytime_avg_2023_2","tph_daytime_avg_2023_3",
                                 "tph_daytime_avg_2023_4")]

  names(pt_frequency)[1] = "LSOA21CD"
  pt_frequency

}
